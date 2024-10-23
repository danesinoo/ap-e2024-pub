module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,
    Job (..),
    JobId (..),
    workerAdd,
    WorkerMsg (..),
    Worker (..),
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void, when)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- Messages sent to SPC.
data SPCMsg
  = -- | Add the job, and reply with the job ID.
    MsgJobAdd Job (ReplyChan JobId)
  | -- | Cancel the given job.
    MsgJobCancel JobId
  | -- | Immediately reply the status of the job.
    MsgJobStatus JobId (ReplyChan JobStatus)
  | -- | Reply when the job is done.
    MsgJobWait JobId (ReplyChan JobDoneReason)
  | -- | Some time has passed.
    MsgTick
  | -- | A worker has been added.
    MsgWorkerAdd WorkerName (ReplyChan (Either String Worker))
  | -- | A job has been done.
    MsgJobDone JobId JobDoneReason WorkerName
  | -- | Stop the worker.
    MsgWorkerStop WorkerName

-- | A handle to the SPC instance.
newtype SPC = SPC (Server SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Seconds)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcWorkers :: [(WorkerName, Worker)],
    spcWaiters :: [(JobId, ReplyChan JobDoneReason)],
    spcChan :: Chan SPCMsg
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (SPCState -> SPCState) -> SPCM ()
modify f = do
  state <- get
  put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  forM_ (spcWorkers state) $ \(_, Worker worker) -> do
    isIdle <- io $ requestReply worker WorkerIsIdle
    when isIdle $ workerAssignJob (Worker worker)

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case ( lookup jobid $ spcJobsPending state,
         lookup jobid $ spcJobsRunning state
       ) of
    (Just _, _) -> do
      notify state
      modify $ \s -> s {spcJobsPending = removeAssoc jobid $ spcJobsPending s}
    (_, Just _) -> do
      notify state
      modify $ \s -> s {spcJobsRunning = removeAssoc jobid $ spcJobsRunning s}
    _ -> pure ()
  where
    notify state = do
      let (waiting, notWaiting) = partition ((== jobid) . fst) $ spcWaiters state
      forM_ waiting $ \(_, to) -> io $ reply to reason
      put $
        state
          { spcJobsDone = (jobid, reason) : spcJobsDone state,
            spcWaiters = notWaiting
          }

workerIsIdle :: WorkerName -> Worker -> SPCM ()
workerIsIdle = undefined

workerIsGone :: WorkerName -> SPCM ()
workerIsGone = undefined

checkTimeouts :: SPCM ()
checkTimeouts = do
  now <- io getSeconds
  state <- get
  forM_ (spcJobsRunning state) $ \(jobid, deadline) ->
    when (now >= deadline) $ do
      jobDone jobid DoneTimeout
      io $ send (spcChan state) $ MsgJobCancel jobid

workerExists :: WorkerName -> SPCM Bool
workerExists = undefined

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case ( lookup jobid $ spcJobsPending state,
                               lookup jobid $ spcJobsRunning state,
                               lookup jobid $ spcJobsDone state
                             ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobWait jobid to -> modify $ \s -> s {spcWaiters = (jobid, to) : spcWaiters s}
    MsgTick -> checkTimeouts
    MsgWorkerAdd wname rsvp -> do
      state <- get
      case lookup wname $ spcWorkers state of
        Just _ -> io $ reply rsvp $ Left "Worker already exists"
        Nothing -> do
          worker <- io $ startWorker c wname
          put $ state {spcWorkers = (wname, worker) : spcWorkers state}
          workerAssignJob worker
          io $ reply rsvp $ Right worker
    MsgJobDone jobid reason wname -> do
      jobDone jobid reason
      state <- get
      case lookup wname $ spcWorkers state of
        Just worker -> do
          workerAssignJob worker
        Nothing -> pure ()
    MsgJobCancel jobid -> do
      state <- get
      case ( lookup jobid $ spcJobsPending state,
             lookup jobid $ spcJobsRunning state
           ) of
        (Just _, _) -> do
          jobDone jobid DoneCancelled
        (_, Just _) -> do
          forM_ (spcWorkers state) $ \(_, Worker worker) -> do
            io $ sendTo worker $ WorkerCancelJob jobid
        _ -> pure ()
    MsgWorkerStop wname -> do
      state <- get
      case lookup wname $ spcWorkers state of
        Just worker -> do
          io $ workerStop worker
          put $ state {spcWorkers = removeAssoc wname $ spcWorkers state}
        Nothing -> pure ()

startSPC :: IO SPC
startSPC = do
  let initial_state c =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWorkers = [],
            spcWaiters = [],
            spcChan = c
          }
  c <- spawn $ \c -> runSPCM (initial_state c) $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      sendTo c MsgTick
      threadDelay 1000000

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) wname =
  requestReply c $ MsgWorkerAdd wname

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop (Worker a) = do
  requestReply a WorkerStop
  kill a

workerAssignJob :: Worker -> SPCM ()
workerAssignJob (Worker w) = do
  state <- get
  case spcJobsPending state of
    [] -> pure ()
    (jid, job) : rest -> do
      io $ sendTo w $ WorkerJobNew jid job
      now <- io getSeconds
      let deadline = now + fromIntegral (jobMaxSeconds job)
      put
        state
          { spcJobsPending = rest,
            spcJobsRunning = (jid, deadline) : spcJobsRunning state
          }

cancelJob :: JobId -> SPCM ()
cancelJob jobid = do
  jobDone jobid DoneCancelled

----------- JOB HANDLING -----------

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers. These are sent both by SPC and by
-- processes spawned by the workes.
data WorkerMsg
  = WorkerJobNew JobId Job
  | -- | A job has been done.
    WorkerJobDone JobId JobDoneReason
  | -- | SPC asks whether the worker is idle
    WorkerIsIdle (ReplyChan Bool)
  | -- | SPC asks to cancel the job with the given id
    WorkerCancelJob JobId
  | -- | SPC asks to stop the worker
    WorkerStop (ReplyChan ())

-- | A handle to a worker.
newtype Worker = Worker (Server WorkerMsg)

data WorkerState = WorkerState
  { name :: WorkerName,
    exec :: Maybe (JobId, ThreadId),
    spc :: Chan SPCMsg
  }

newtype WorkerM a = WorkerM (WorkerState -> IO (a, WorkerState))

instance Functor WorkerM where
  fmap = liftM

instance Applicative WorkerM where
  pure x = WorkerM $ \s -> pure (x, s)
  (<*>) = ap

instance Monad WorkerM where
  WorkerM m >>= f = WorkerM $ \s -> do
    (x, s') <- m s
    let WorkerM f' = f x
    f' s'

getW :: WorkerM WorkerState
getW = WorkerM $ \state -> pure (state, state)

putW :: WorkerState -> WorkerM ()
putW state = WorkerM $ \_ -> pure ((), state)

ioW :: IO a -> WorkerM a
ioW m = WorkerM $ \state -> do
  x <- m
  pure (x, state)

runWorkerM :: WorkerState -> WorkerM a -> IO a
runWorkerM state (WorkerM f) = fst <$> f state

startWorker :: Chan SPCMsg -> WorkerName -> IO Worker
startWorker spc_chan wname = do
  let initial_state =
        WorkerState
          { name = wname,
            exec = Nothing,
            spc = spc_chan
          }
  c <- spawn $ \c -> runWorkerM initial_state $ forever $ workerHandle c
  pure $ Worker c

workerHandle :: Chan WorkerMsg -> WorkerM ()
workerHandle c = do
  msg <- ioW $ receive c
  state <- getW
  case msg of
    WorkerJobNew jid job -> do
      case exec state of
        Just _ -> pure ()
        Nothing -> do
          tid <- ioW $ forkIO $ do
            let val = do
                  jobAction job
                  send c $ WorkerJobDone jid Done
                onException :: SomeException -> IO ()
                onException _ = do
                  send c $ WorkerJobDone jid DoneCrashed
            catch val onException
          putW $ state {exec = Just (jid, tid)}
    WorkerJobDone jid reason -> do
      case exec state of
        Just (jid', _) | jid == jid' -> do
          putW $ state {exec = Nothing}
          ioW $ send (spc state) $ MsgJobDone jid reason (name state)
        _ -> pure ()
    WorkerIsIdle rsvp -> do
      case exec state of
        Just _ -> ioW $ reply rsvp False
        Nothing -> ioW $ reply rsvp True
    WorkerCancelJob jid -> case exec state of
      Just (jobid, tid) | jobid == jid -> do
        ioW $ killThread tid
        putW $ state {exec = Nothing}
        ioW $ send (spc state) $ MsgJobDone jid DoneCancelled (name state)
      _ -> pure ()
    WorkerStop quit_chan -> do
      case exec state of
        Just (jobid, tid) -> do
          ioW $ killThread tid
          ioW $ send (spc state) $ MsgJobDone jobid DoneCancelled (name state)
        Nothing -> pure ()
      ioW $ reply quit_chan ()

odule SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    pingSPC,
    Job (..),
    JobId (..),
    jobAdd,
    JobDoneReason (..),
    JobStatus (..),
    jobStatus,
    jobCancel,
    jobWait,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
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

-- Messages sent to SPC.
data SPCMsg
  = MsgNewJob Job (ReplyChan JobId)
  | -- immediately reply with the status of the job
    MsgJobStatus JobId (ReplyChan (Maybe JobStatus))
  | -- cancel a job
    MsgCancelJob JobId
  | MsgJobWait JobId (ReplyChan (Maybe JobDoneReason))
  | MsgDone JobId
  | MsgCrash JobId
  | MsgTick

-- | A Handle to the SPC instance.
newtype SPC = SPC (Server SPCMsg)

type Deadline = Seconds

data SPCState = SPCState
  { spcCounter :: JobId,
    spcPending :: [(JobId, Job)],
    spcDone :: [(JobId, JobDoneReason)],
    spcHook :: [(JobId, ReplyChan (Maybe JobDoneReason))],
    spcChan :: Chan SPCMsg,
    running :: Maybe (JobId, ThreadId, Deadline)
  }

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \s -> pure (x, s)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \s -> do
    (x, s') <- m s
    let SPCM y = f x
    y s'

get :: SPCM SPCState
get = SPCM $ \s -> pure (s, s)

put :: SPCState -> SPCM ()
put s = SPCM $ \_ -> pure ((), s)

io :: IO a -> SPCM a
io m = SPCM $ \s -> do
  a <- m
  pure (a, s)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM s (SPCM m) = fst <$> m s

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  schedule
  msg <- io $ receive c
  case msg of
    MsgNewJob job to -> do
      state <- get
      let JobId jobid = spcCounter state
      put $
        state
          { spcCounter = JobId (jobid + 1),
            spcPending = (spcCounter state, job) : spcPending state
          }
      io $ reply to $ JobId jobid
    MsgJobWait jobid to -> do
      state <- get
      put $
        state
          { spcHook = (jobid, to) : spcHook state
          }
    MsgJobStatus jobid to -> do
      state <- get
      io $ reply to $ case ( lookup jobid $ spcPending state,
                             lookup jobid $ spcDone state,
                             running state
                           ) of
        (Just _, _, _) -> Just JobPending
        (_, Just a, _) -> Just $ JobDone a
        (_, _, Just (jid, _, _)) | jid == jobid -> Just JobRunning
        _ -> Nothing
    MsgCancelJob jobid -> jobDone jobid DoneCancelled
    MsgDone jobid -> jobDone jobid Done
    MsgCrash jobid -> jobDone jobid DoneCrashed
    MsgTick -> checkTimeouts

startSPC :: IO SPC
startSPC = do
  let state c =
        SPCState
          { spcCounter = JobId 0,
            spcPending = [],
            spcDone = [],
            spcHook = [],
            spcChan = c,
            running = Nothing
          }
  server <- spawn $ \c -> runSPCM (state c) $ forever $ handleMsg c
  void $ spawn $ \_ -> forever $ sendTo server MsgTick >> threadDelay 1000000
  pure $ SPC server

pingSPC :: SPC -> IO JobId
pingSPC (SPC c) = do
  requestReply c $ MsgNewJob Job {jobAction = pure (), jobMaxSeconds = 0}

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

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) j = requestReply c $ MsgNewJob j

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled.
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
  | -- | The job is unknown i.e. it has never been enqueued.
    JobUnknown
  deriving (Eq, Ord, Show)

jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobid = requestReply c $ MsgJobStatus jobid

jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid = sendTo c $ MsgCancelJob jobid

-- | Synchronously block until job is done and return the reason.
-- Returns 'Nothing' if job is not known to this SPC instance.
jobWait :: SPC -> JobId -> IO (Maybe JobDoneReason)
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobid reason = do
  state <- get
  case (lookup jobid $ spcPending state, running state) of
    (Just _, _) -> do
      notify state
      state' <- get -- get the updated state
      put $ state' {spcPending = removeAssoc jobid $ spcPending state'}
    (_, Just (jid, tid, _)) | jid == jobid -> do
      io $ killThread tid
      notify state
      state' <- get -- get the updated state
      put $ state' {running = Nothing}
    _ -> pure ()
  where
    notify state = do
      let (waiting, notWaiting) = partition ((== jobid) . fst) $ spcHook state
      forM_ waiting $ \(_, to) -> io $ reply to $ Just reason
      put $
        state
          { spcDone = (jobid, reason) : spcDone state,
            spcHook = notWaiting
          }

schedule :: SPCM ()
schedule = do
  state <- get
  case (running state, spcPending state) of
    (Nothing, []) -> pure ()
    (Nothing, (jobid, job) : jobs) -> do
      tid <- io $ forkIO $ do
        let val = do
              jobAction job
              send (spcChan state) $ MsgDone jobid
            onException :: SomeException -> IO ()
            onException _ = do
              send (spcChan state) $ MsgCrash jobid
        catch val onException
      now <- io getSeconds
      put $ state {running = Just (jobid, tid, now + fromIntegral (jobMaxSeconds job)), spcPending = jobs}
    _ -> pure ()

checkTimeouts :: SPCM ()
checkTimeouts = do
  now <- io getSeconds
  state <- get
  case running state of
    Just (jobid, _, deadline) | now >= deadline -> jobDone jobid DoneTimeout
    _ -> pure ()

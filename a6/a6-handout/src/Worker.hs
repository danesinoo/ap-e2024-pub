module Worker
  ( Worker,
    startWorker,
    WorkerName,
    Job (..),
    JobId (..),
  )
where

import Control.Concurrent
  ( ThreadId,
  )
import Control.Monad (ap, forever, liftM)
import GenServer

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
data WorkerMsg = MsgJobNew JobId Job

-- | A handle to a worker.
newtype Worker = Worker (Server WorkerMsg)

data WorkerState = WorkerState
  { name :: WorkerName,
    exec :: Maybe (JobId, ThreadId)
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

get :: WorkerM WorkerState
get = WorkerM $ \state -> pure (state, state)

put :: WorkerState -> WorkerM ()
put state = WorkerM $ \_ -> pure ((), state)

-- | Modify the state.
modify :: (WorkerState -> WorkerState) -> WorkerM ()
modify f = do
  state <- get
  put $ f state

io :: IO a -> WorkerM a
io m = WorkerM $ \state -> do
  x <- m
  pure (x, state)

runWorkerM :: WorkerState -> WorkerM a -> IO a
runWorkerM state (WorkerM f) = fst <$> f state

startWorker :: WorkerName -> IO Worker
startWorker wname = do
  let initial_state =
        WorkerState
          { name = wname,
            exec = Nothing
          }
  c <- spawn $ \c -> runWorkerM initial_state $ forever $ workerHandle c
  undefined

workerHandle :: Chan WorkerMsg -> WorkerM ()
workerHandle c = do
  msg <- io $ receive c
  case msg of
    MsgJobNew jid job -> do
      undefined
    _ -> pure ()

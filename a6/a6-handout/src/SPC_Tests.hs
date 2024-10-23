module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, replicateM)
import Data.IORef
import GenServer
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "adding job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 1
          r <- jobStatus spc j
          r @?= JobPending,
        testCase "adding worker" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker"
          e <- workerAdd spc "worker"
          case e of
            Left _ -> pure ()
            Right _ -> assertFailure "workerAdd should have failed",
        testCase "adding worker" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 1
          _ <- workerAdd spc "worker"
          c <- jobWait spc j
          c @?= Done,
        testCase "canceling job" $ do
          spc <- startSPC
          j <- jobAdd spc $ Job (pure ()) 1
          jobCancel spc j
          r <- jobStatus spc j
          r @?= JobDone DoneCancelled,
        testCase "timeout" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          _ <- workerAdd spc "worker"
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= DoneTimeout,
        testCase "crash" $ do
          spc <- startSPC
          j1 <- jobAdd spc $ Job (error "boom") 1
          _ <- workerAdd spc "worker"
          r1 <- jobWait spc j1
          r1 @?= DoneCrashed
          -- Ensure new jobs can still work.
          ref <- newIORef False
          j2 <- jobAdd spc $ Job (writeIORef ref True) 1
          r2 <- jobWait spc j2
          r2 @?= Done
          v <- readIORef ref
          v @?= True
      ]

module APL.InterpIO (module APL.InterpIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

-- Creates a fresh temporary database, passes it to a function returning an
-- IO-computation, executes the computation, deletes the temporary database, and
-- finally returns the result of the computation. The temporary database file is
-- guaranteed fresh and won't have a name conflict with any other files.
withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB -- Create a new temp database file.
  res <- m tempDB -- Run the computation with the new file.
  removeFile tempDB -- Delete the temp database file.
  pure res -- Return the result of the computation.

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  runEvalIO' envEmpty dbFile evalm
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (StateGetOp k)) = runEvalIO' r db . f =<< readDB db
      where
        f (Right s) = k s
        f (Left e) = failure e
    runEvalIO' r db (Free (StatePutOp s k)) = writeDB db s *> runEvalIO' r db k
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
    runEvalIO' r db (Free (TryCatchOp m n)) = runEvalIO' r db $ catch m n
    runEvalIO' r db (Free (KvGetOp key k)) = runEvalIO' r db . fun =<< bar db
      where
        fun val = case val of
          Right v -> k v
          Left i -> failure $ "Invalid value input: " ++ i
        bar db' = do
          s <- readDB db'
          case lookup key <$> s of
            Left _ -> fail "Invalid DB."
            (Right (Just s')) -> pure $ Right s'
            (Right Nothing) -> do
              putStr $ "Key not found: " ++ show key ++ ". "
              input <- prompt "Enter a replacement: "
              case readVal input of
                Just v -> pure $ Right v
                _ -> pure $ Left input
    runEvalIO' r db (Free (KvPutOp key val m)) = do
      s <- readDB db
      case s of
        Right s' -> do
          writeDB db $ insert key val s'
          runEvalIO' r db m
          where
            insert k v [] = [(k, v)]
            insert k v ((k', v') : kvs)
              | k == k' = (k, v) : kvs
              | otherwise = (k', v') : insert k v kvs
        Left _ -> undefined
    runEvalIO' r db (Free (TransactionOp m n)) = do
      withTempDB $ \db' -> do
        copyDB db db'
        res <- runEvalIO' r db' m
        case res of
          Right _ -> do
            copyDB db' db
          _ -> do
            pure ()
      runEvalIO' r db n

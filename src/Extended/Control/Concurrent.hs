module Extended.Control.Concurrent (
    module Control.Concurrent
  , foreverRetry
    ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Monad
import           Data.Thyme.Calendar      (showGregorian)
import           Data.Thyme.LocalTime
import           System.IO                (hFlush, stderr, stdout)

prettyThreadDetails :: String -> IO ()
prettyThreadDetails msg = do
  (ZonedTime (LocalTime d' t') _) <- getZonedTime
  putStrLn $ (showGregorian d') ++ "T" ++ (take 15 $ show t') ++ " [THREAD]: " ++ msg
  hFlush stdout >> hFlush stderr

foreverRetry :: String -> IO () -> IO ()
foreverRetry threadName action = void $ forkIO $ forever $ do
  threadDied <- newEmptyMVar
  void $ forkFinally (prettyThreadDetails (threadName ++ " launching") >> action >> putMVar threadDied ())
    $ \res -> do
      case res of
        Right () -> prettyThreadDetails $ threadName ++ " died returning () with no details"
        Left err -> prettyThreadDetails $ threadName ++ " exception " ++ show err
      putMVar threadDied ()
  takeMVar threadDied
  prettyThreadDetails $ threadName ++ "got MVar... restarting"

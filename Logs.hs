module Logs (
  Logs, 
  log, 
  emptyLogs, 
  getLastLogs) 
where

import Data.Sequence hiding (replicate)
import Prelude hiding (drop, length, log)
import qualified Config
import Control.Concurrent.STM
import Data.Foldable
import Data.XML.Types

type Logs = TVar (Seq [Node])

log :: [Node] -> Logs -> IO ()
log s logs = atomically $ do
  oldLogs <- readTVar logs
  let newLogs = drop 1 $ oldLogs |> s
  writeTVar logs newLogs

emptyLogs :: IO Logs
emptyLogs = newTVarIO $ fromList $ replicate Config.maxLoggedLines []

getLastLogs :: Int -> Logs -> IO [[Node]]
getLastLogs n logs = atomically $ do
  s <- readTVar logs
  return $ foldMap (:[]) . viewl $ drop (length s - min Config.maxLoggedLines n) s

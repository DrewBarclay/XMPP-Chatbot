{-# LANGUAGE BangPatterns #-}

module Logs (
  Logs, 
  log, 
  emptyLogs, 
  getLastLogs,
  getSavedLogs,
  saveLogs
) 
where

import Data.Sequence hiding (replicate)
import Prelude hiding (drop, length, log, readFile)
import qualified Config
import Control.Concurrent.STM
import Data.Foldable hiding (length)
import Data.XML.Types
import System.IO
import System.FilePath
import System.Directory
import Instances
import Control.DeepSeq
import Text.Read (readMaybe)
import Control.Monad

type Logs = TVar (Seq [Node])

log :: [Node] -> Logs -> IO ()
log s logs = atomically $ do
  !oldLogs <- readTVar logs
  let !newLogs = drop 1 $ oldLogs |> s
  writeTVar logs newLogs

emptyLogs :: IO Logs
emptyLogs = newTVarIO $ fromList $ replicate Config.maxLoggedLines []

getLastLogs :: Int -> Logs -> IO [[Node]]
getLastLogs n logs = atomically $ do
  s <- readTVar logs
  return $ foldMap (:[]) . viewl $ drop (length s - min Config.maxLoggedLines n) s

saveLogs :: Logs -> IO ()
saveLogs logs = do
  dir <- Config.appDir
  fp <- logsFile
  createDirectoryIfMissing True dir
  h <- openFile fp WriteMode
  readTVarIO logs >>= hPutStr h . show
  hClose h

getSavedLogs :: IO Logs
getSavedLogs = do
  fp <- logsFile
  exists <- doesFileExist fp
  case exists of
    True -> do
      !parsedLogs <- fmap readMaybe $ readFile fp
      case parsedLogs of
        Just !ls -> newTVarIO ls
        Nothing -> emptyLogs
    False -> emptyLogs

logsFile :: IO FilePath
logsFile = fmap (</> "logs.dat") Config.appDir

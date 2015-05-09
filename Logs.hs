module Logs (Logs, log, emptyLogs, getLastLogs) where

import Data.Sequence 
import Prelude hiding (drop, length)
import qualified Config

type Logs = Seq String

log :: String -> Logs -> Logs
log m s = drop 1 $ s |> m

emptyLogs :: Logs
emptyLogs = fromList $ replicate Config.maxLoggedLines ""

getLastLogs :: Integer -> Logs -> [String]
getLastLogs n s -> fold . viewl $ drop (length s - n) s

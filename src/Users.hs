{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Users (
  User(..), 
  Users(..),
  saveUsers, 
  getUsers,
  getUser,
  setUser
) where

import Prelude hiding (readFile)
import qualified Network.Xmpp as Xmpp
import qualified Data.Map.Strict as Map
import System.IO(hClose, openFile, IOMode(WriteMode))
import Data.ByteString.Lazy(hPut, readFile)
import System.FilePath
import System.Directory
import qualified Config
import Control.Monad
import Data.Maybe
import Data.Text (unpack)
import Control.Concurrent.STM
import Text.Read (readMaybe)
import Control.DeepSeq
import Data.Binary
import GHC.Generics(Generic)
import Instances
  
data User = User { jid :: Xmpp.Jid,
                   alias :: String,
                   multicast :: Bool,
                   squelchList :: [Xmpp.Jid]
                 } deriving (Show, Read, Generic)

instance Binary User

type Users = TVar (Map.Map Xmpp.Jid User)

saveUsers :: Users -> IO ()
saveUsers usT = do
  dir <- Config.appDir
  fp <- usersFile
  createDirectoryIfMissing True dir
  h <- openFile fp WriteMode
  readTVarIO usT >>= hPut h . encode
  hClose h

getUsers :: IO Users
getUsers = do
  fp <- usersFile
  exists <- doesFileExist fp
  case exists of
    True -> do
      !parsedUsers <- fmap decodeOrFail $ readFile fp
      case parsedUsers of
        Right !(_, _, us) -> newTVarIO us
        Left _ -> newTVarIO Map.empty
    False -> newTVarIO Map.empty

usersFile :: IO FilePath
usersFile = fmap (</> "users.dat") Config.appDir

--internal only
getUserM j us = Map.findWithDefault (User { jid = Xmpp.toBare j, alias = unpack $ fromMaybe "" (Xmpp.localpart j), multicast = False, squelchList = [] }) (Xmpp.toBare j) us  

getUser :: Xmpp.Jid -> Users -> IO User
getUser j usT = atomically $ do
  !us <- readTVar usT
  return $ getUserM j us 

setUser :: User -> Users -> IO ()
setUser u usT = atomically $ do
  us <- readTVar usT
  let !us' = Map.insert (jid u) u us
  writeTVar usT us'  
         

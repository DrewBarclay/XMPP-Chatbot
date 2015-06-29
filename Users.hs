{-# LANGUAGE OverloadedStrings #-}

module Users (
  User(..), 
  Users(..), 
  getUsers,
  getUser,
  setUserAlias
) where

import qualified Network.Xmpp as Xmpp
import qualified Data.Map.Strict as Map
import System.IO
import System.FilePath
import System.Directory
import qualified Config
import Control.Monad
import Data.Maybe
import Data.Text (unpack)
import Control.Concurrent.STM
  
data User = User { jid :: Xmpp.Jid,
                   alias :: String
                 } deriving (Show, Read)

type Users = TVar (Map.Map Xmpp.Jid User)

saveUsers :: Users -> IO ()
saveUsers usT = do
  dir <- Config.appDir
  fp <- usersFile
  createDirectoryIfMissing True dir
  h <- openFile fp WriteMode
  readTVarIO usT >>= hPutStr h . show
  hClose h

getUsers :: IO Users
getUsers = do
  fp <- usersFile
  exists <- doesFileExist fp
  case exists of
    True -> readFile fp >>= newTVarIO . read
    False -> newTVarIO Map.empty

usersFile :: IO FilePath
usersFile = fmap (</> "users.dat") Config.appDir

--internal only
getUserM j us = Map.findWithDefault (User { jid = Xmpp.toBare j, alias = unpack $ fromMaybe "" (Xmpp.localpart j) }) j us  

getUser :: Xmpp.Jid -> Users -> IO User
getUser j usT = atomically $ do
  us <- readTVar usT
  return $ getUserM j us    

setUserAlias :: String -> Xmpp.Jid -> Users -> IO ()
setUserAlias s j usT = do
  atomically $ do
    us <- readTVar usT
    let u = getUserM j us
    let u' = u {alias=s}
    let us' = Map.insert (Xmpp.toBare j) u' us
    writeTVar usT us'
  saveUsers usT
         

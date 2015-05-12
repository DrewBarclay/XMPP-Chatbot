{-# LANGUAGE OverloadedStrings #-}

module Users (
  User(..), 
  Users(..), 
  getUsers,
  getUser
) where

import qualified Network.Xmpp as Xmpp
import qualified Data.Map.Strict as Map
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

getUsers :: IO Users
getUsers = do
  fp <- usersFile
  exists <- doesFileExist fp
  case exists of
    True -> readFile fp >>= newTVarIO . read
    False -> newTVarIO Map.empty
 
usersFile :: IO FilePath
usersFile = fmap (</> "users.dat") (getAppUserDataDirectory Config.appName)

--internal only
getUserM j us = Map.findWithDefault (User { jid = Xmpp.toBare j, alias = unpack $ fromMaybe "" (Xmpp.localpart j) }) j us  

getUser :: Xmpp.Jid -> Users -> IO User
getUser j usT = atomically $ do
  us <- readTVar usT
  return $ getUserM j us    

setUserAlias :: String -> Xmpp.Jid -> Users -> IO ()
setUserAlias s j usT = atomically $ do
  us <- readTVar usT
  let u = getUserM j us
  let u' = u {alias=s}
  let us' = Map.insert (Xmpp.toBare j) u' us
  writeTVar us' usT
       

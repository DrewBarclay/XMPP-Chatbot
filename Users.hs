module Users (User, Users, getUsers) where

import qualified Network.Xmpp as Xmpp
import qualified Data.Map.Strict as Map
import System.FilePath
import System.Directory
import qualified Config
import Control.Monad
  
data User = User { jid :: Xmpp.Jid,
                   alias :: String
                 } deriving (Show, Read)

type Users = Map.Map Xmpp.Jid User

getUsers :: IO Users
getUsers = do
  fp <- usersFile
  exists <- doesFileExist fp
  case exists of
    True -> readFile fp >>= return . read
    False -> return Map.empty
 
usersFile :: IO FilePath
usersFile = fmap (</> "users.dat") (getAppUserDataDirectory Config.appName)

getUser :: Xmpp.Jid -> Users -> User
getUser j us = Map.findWithDefault (User { jid = j, alias = show j }) j us           

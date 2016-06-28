module Config (
  maxLoggedLines, 
  appName,
  appDir,
  messageCharacterLimit
) where

import System.IO
import System.FilePath
import System.Directory

maxLoggedLines :: Int
maxLoggedLines = 3000

appName :: String
appName = "hsxmppchatbot"

appDir :: IO FilePath --I wish this wasn't an IO thing.
appDir = getAppUserDataDirectory appName

messageCharacterLimit :: Int
messageCharacterLimit = 3000

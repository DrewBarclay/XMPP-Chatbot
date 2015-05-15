{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Default
import Network.Xmpp
import Network.Xmpp.IM
import System.Log.Logger
import Control.Lens
import qualified Network.TLS as TLS
import qualified Handlers
import qualified Users
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Logs
import qualified Common
import System.Environment (getArgs)
import System.Exit
import Data.Text (pack)
import Control.Concurrent (threadDelay)
import Control.Exception

main :: IO ()
main = do
  updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

  args <- getArgs
  if length args /= 3
    then putStrLn "Usage: ./Chatbot <username> <password> <domain>" >> exitSuccess
    else return ()

  let [username, password, domain] = args

  --connect to the XMPP server
  result <- session
               domain
                (Just (\_ -> ( [scramSha1 (pack username) Nothing (pack password)])
                             , Nothing))
                (def & tlsUseNameIndicationL .~ True
                 & osc .~ (\_ _ _ _ -> return []))
  sess <- case result of
    Right s -> return s
    Left e -> error $ "XmppFailure: " ++ (show e)

  --init bot stuff
  sendPresence def sess
  setConnectionClosedHandler (\_ s -> reconnect' s >> sendPresence def sess >> return ()) sess

  users <- Users.getUsers
  logs <- Logs.emptyLogs
  let (Just bj) = jidFromTexts (Just $ pack username) (pack domain) Nothing
  let bd = Common.BotData {Common.session=sess, Common.users=users, Common.logs=logs, Common.botJid=bj}


  --finally, pass off everything to handlers
  [sess2, sess3] <- replicateM 2 $ dupSession sess
  as <- mapM async [Handlers.handleMessages (bd {Common.session=sess2}), Handlers.handlePresences sess3]

  forM_ as wait --infinite wait
  where
    osc = streamConfigurationL . tlsParamsL . clientHooksL . onServerCertificateL --All this exists to connect to a server despite it having a bad security certificate like we do.
    clientHooksL = lens TLS.clientHooks (\cp ch -> cp{TLS.clientHooks = ch})
    onServerCertificateL = lens TLS.onServerCertificate (\ch osc -> ch {TLS.onServerCertificate = osc})

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
import Control.Concurrent (threadDelay, myThreadId)
import Control.Exception
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)

main :: IO ()
main = do
  --updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

  args <- getArgs
  if length args /= 3
    then putStrLn "Usage: ./Chatbot <username> <password> <domain>" >> exitSuccess
    else return ()

  let [username, password, domain] = args

  --connect to the XMPP server
  result <- session
               domain
                (simpleAuth (pack username) (pack password))
                (def & tlsUseNameIndicationL .~ True
                 & osc .~ (\_ _ _ _ -> return []))
  sess <- case result of
    Right s -> return s
    Left e -> error $ "XmppFailure: " ++ (show e)

  --init bot stuff
  sendPresence def sess
  setConnectionClosedHandler (\_ s -> reconnect' s >> sendPresence def sess >> return ()) sess

  users <- Users.getUsers
  logs <- Logs.getSavedLogs
  let (Just bj) = jidFromTexts (Just $ pack username) (pack domain) Nothing
  let bd = Common.BotData {Common.session=sess, Common.users=users, Common.logs=logs, Common.botJid=bj}


  --finally, pass off everything to handlers

  (do
    tid <- myThreadId
    installHandler sigINT (Catch $ Handlers.handleExit tid logs) Nothing --this kills the cross-platform compatibility.
    installHandler sigTERM (Catch $ Handlers.handleExit tid logs) Nothing
    let hs = [Handlers.handleMessages, Handlers.handlePresences]
    ss <- replicateM (length hs - 1) $ dupSession sess -- length-1 because we're using the original session
    as <- mapM async $ zipWith (\h s -> h $ bd {Common.session=s}) hs (sess : ss)
    forM_ as wait
   ) `finally` (do
    Logs.saveLogs logs
    Users.saveUsers users
    putStrLn "Logs and users saved, exiting."
   )

  where
    osc = streamConfigurationL . tlsParamsL . clientHooksL . onServerCertificateL --All this exists to connect to a server despite it having a bad security certificate like we do.
    clientHooksL = lens TLS.clientHooks (\cp ch -> cp{TLS.clientHooks = ch})
    onServerCertificateL = lens TLS.onServerCertificate (\ch osc -> ch {TLS.onServerCertificate = osc})

module Common (
  BotData(..),
  sendMessageToAllBut,
  sendMessageToAll,
  sendMessageTo
) where

import Network.Xmpp hiding (session)
import Network.Xmpp.IM
import Users
import Logs
import Data.XML.Types
import qualified XmlUtils
import Prelude hiding (log)
import Control.Monad
import qualified Data.Map as M
import Control.Concurrent.STM

data BotData = BotData {session :: Session, users :: Users, logs :: Logs}

sendMessageToAll :: BotData -> [Node] -> IO ()
sendMessageToAll (BotData {session=sess, logs=ls}) msg = do
  let xmppMsg = message {messageType=Chat, messagePayload=XmlUtils.wrapMessage msg}
  r <- getRoster sess
  forM_ (fmap riJid $ M.elems $ items r) (\j -> do
    av <- atomically $ isPeerAvailable j sess
    if av 
      then sendMessage (xmppMsg {messageTo = Just j}) sess
      else return undefined)
  log msg ls

sendMessageToAllBut :: Jid -> BotData -> [Node] -> IO ()
sendMessageToAllBut sender (BotData {session=sess, logs=ls}) msg = do
  let xmppMsg = message {messageType=Chat, messagePayload=XmlUtils.wrapMessage msg}
  r <- getRoster sess
  forM (filter (/=toBare sender) $ fmap riJid $ M.elems $ items r) (\j -> 
    sendMessage (xmppMsg {messageTo = Just j}) sess)
  log msg ls

sendMessageTo :: Jid -> BotData -> [Node] -> IO ()
sendMessageTo sendee (BotData {session=sess}) msg = do
  let xmppMsg = message {messageType=Chat, messageTo=Just sendee, messagePayload=XmlUtils.wrapMessage msg}
  sendMessage xmppMsg sess
  return ()

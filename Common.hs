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

data BotData = BotData {session :: Session, users :: Users, logs :: Logs, botJid :: Jid}

sendMessageToAll :: BotData -> [Node] -> IO ()
sendMessageToAll bd msg = sendMessageToAllBut [] bd msg

sendMessageToAllBut :: [Jid] -> BotData -> [Node] -> IO ()
sendMessageToAllBut js (BotData {session=sess, logs=ls, botJid=bj}) msg = do
  let xmppMsg = message {messageType=Chat, messagePayload=XmlUtils.wrapMessage msg}
  ps <- atomically $ getAvailablePeers sess
  let js' = fmap toBare $ bj : js
  forM (filter (\j -> not $ elem j js') ps) (\j -> sendMessage (xmppMsg {messageTo = Just j}) sess)
  log msg ls

sendMessageTo :: Jid -> BotData -> [Node] -> IO ()
sendMessageTo sendee (BotData {session=sess}) msg = do
  let xmppMsg = message {messageType=Chat, messageTo=Just sendee, messagePayload=XmlUtils.wrapMessage msg}
  sendMessage xmppMsg sess
  return ()

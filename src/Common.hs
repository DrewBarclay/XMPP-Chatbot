module Common (
  BotData(..),
  sendSquelchableMessageToAllButFrom,
  sendSquelchableMessageToAllFrom,
  sendSquelchableMessageToAll,
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
sendMessageToAllBut js (BotData {session=sess, logs=ls, botJid=bj, users=us}) msg = do
  let js' = fmap toBare $ bj : js --list of people not to send to
  let xmppMsg = message {messageType=Chat, messagePayload=XmlUtils.wrapMessage msg}
  ps <- atomically $ getAvailablePeers sess
  psWithStatus <- forM ps (\j -> do
    u <- getUser j us
    if multicast u
      then atomically $ getPeerEntities j sess
      else return $ M.fromList [(j, Nothing)]
    ) --get jids with resources 
  let ps = concat $ fmap M.keys psWithStatus
  forM (filter (\j -> not $ elem (toBare j) js') ps) (\j -> sendMessage (xmppMsg {messageTo = Just j}) sess)
  log msg ls

--Sends a message to everyone except anyone who squelched the given jid
sendSquelchableMessageToAll :: Jid -> BotData -> [Node] -> IO ()
sendSquelchableMessageToAll = sendSquelchableMessageToAllButFrom []

--Sends a message to everyone but the sender and the people who have the sender squelched
sendSquelchableMessageToAllFrom :: Jid -> BotData -> [Node] -> IO ()
sendSquelchableMessageToAllFrom sender bd msg = sendSquelchableMessageToAllButFrom [sender] sender bd msg

sendSquelchableMessageToAllButFrom buts sender bd@(BotData {session=sess, logs=ls, botJid=bj, users=us}) msg = do
  ps <- atomically $ getAvailablePeers sess
  squelchers <- flip filterM ps (\j -> do
    u <- getUser j us
    return $ toBare sender `elem` squelchList u --Most people have 1-2 squelched people max, no point optimizing this too much
   )
  sendMessageToAllBut (buts ++ squelchers) bd msg

sendMessageTo :: Jid -> BotData -> [Node] -> IO ()
sendMessageTo sendee (BotData {session=sess}) msg = do
  let xmppMsg = message {messageType=Chat, messageTo=Just sendee, messagePayload=XmlUtils.wrapMessage msg}
  sendMessage xmppMsg sess
  return ()

{-# LANGUAGE OverloadedStrings #-}

module Handlers (
  handleMessages,
  handlePresences
) where

import Network.Xmpp hiding (session)
import Network.Xmpp.IM
import Control.Concurrent.STM
import Control.Monad hiding (forM)
import Data.Traversable
import qualified Users
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.XML.Types as Xml
import qualified XmlUtils
import Logs
import Common
import Data.Char(isSpace)
import Data.Attoparsec.Text
import qualified Data.List as List
import Control.Applicative
import Prelude hiding (takeWhile)
import qualified Data.Map as M

--The message handler takes in messages and broadcasts them to everyone on the roster.
handleMessages :: BotData -> IO a
handleMessages bd@BotData {session=sess, users=us, logs=ls} = forever $ do
  msg <- waitForMessage (\m -> isJust (messageFrom m) && isJust (XmlUtils.unwrapMessage (messagePayload m))) sess

  let (Just sender) = fmap toBare $ messageFrom msg
  let (Just payload) = XmlUtils.unwrapMessage (messagePayload msg)
  let s = XmlUtils.nodesToString payload
  alias <- fmap Users.alias $ Users.getUser sender us
  let broadcastMsg = XmlUtils.boldText alias : XmlUtils.text ": " : payload

  --Check if command
  if head s == '!'
    then parseCommand s sender
    else sendMessageToAllBut sender bd broadcastMsg

  where 
    parseCommand :: String -> Jid -> IO ()
    parseCommand s sender = case parseOnly parser (Text.pack s) of
      Left e -> sendMessageTo sender bd $ [XmlUtils.italicsText $ "Incorrect command syntax."]
      Right Help -> sendMessageTo sender bd $ [XmlUtils.italicsText "This bot is here to help! Commands: help, log <number>, ping, alias <name>, list"]
      Right (GetLogs i) -> do
        lastLogs <- getLastLogs i ls 
        sendMessageTo sender bd $ List.intercalate [XmlUtils.newline] $ [XmlUtils.boldText "Last logs:"] : lastLogs
      Right Ping -> sendMessageTo sender bd $ [XmlUtils.italicsText "PONG!"] 
      Right (Alias a) -> do
        oldAlias <- fmap Users.alias $ Users.getUser sender us
        Users.setUserAlias a sender us
        sendMessageToAll bd $ [XmlUtils.italicsNode [XmlUtils.boldText oldAlias, XmlUtils.text " is now known as ", XmlUtils.boldText a, XmlUtils.text "."]]
      Right (List) -> do
        r <- getRoster sess
        ls <- forM (fmap riJid $ M.elems $ items r) (\j -> do
          u <- Users.getUser j us
          return [XmlUtils.boldText (Users.alias u), XmlUtils.text $ " (" ++ Text.unpack (jidToText . toBare $ Users.jid u) ++ ")"])
        sendMessageTo sender bd $ List.intercalate [XmlUtils.newline] $ [XmlUtils.boldText "Users in this chat:"] : ls
 
        
    parser :: Parser BotCommand
    parser = do
      char '!'
      (string "help" >> return Help)
       <|> (string "log" >> takeWhile isSpace >> decimal >>= return . GetLogs)
       <|> (string "ping" >> return Ping)
       <|> (string "alias" >> takeWhile isSpace >> takeWhile (const True) >>= return . Alias . Text.unpack)
       <|> (string "list" >> return List)
      
data BotCommand = GetLogs Int | Help | Ping | Alias String | List
  
--The presence handler takes in presences and looks for subscription requests. Upon finding one, it subscribes them back
--and adds them to the roster.
handlePresences :: Session -> IO a
handlePresences sess = forever $ do
  pres <- waitForPresence (\p -> isJust (presenceFrom p)) sess --only deal with presences who have a 'from'
  let Just sender = presenceFrom pres
  case presenceType pres of
    Subscribe -> do
      sendPresence (presenceSubscribe sender) sess --subscribe in turn, server handles roster
      sendPresence (presenceSubscribed sender) sess
    Unsubscribe -> do
      sendPresence (presenceUnsubscribe sender) sess --unsub in turn, server handles roster
      sendPresence (presenceUnsubscribed sender) sess
    _ -> do
      return undefined

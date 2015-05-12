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

--The message handler takes in messages and broadcasts them to everyone on the roster.
handleMessages :: BotData -> IO a
handleMessages bd@BotData {session=sess, users=us, logs=ls} = forever $ do
  msg <- waitForMessage (\m -> isJust (messageFrom m) && isJust (XmlUtils.unwrapMessage (messagePayload m))) sess

  let (Just sender) = toBare $ messageFrom msg
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
      Left e -> sendMessageTo sender bd $ [XmlUtils.italicsText $ "Incorrect command syntax: " ++ e]
      Right Help -> sendMessageTo sender bd $ [XmlUtils.italicsText "This bot is here to help! Commands: help, log <number>, ping"]
      Right (GetLogs i) -> do
        lastLogs <- getLastLogs i ls 
        sendMessageTo sender bd $ XmlUtils.boldText "Last logs:" : List.intercalate [XmlUtils.newline] lastLogs
      Right Ping -> sendMessageTo sender bd $ [XmlUtils.italicsText "PONG!"] 
      Right (Alias a) -> do
        setUserAlias a sender us
 
        
    parser :: Parser BotCommand
    parser = do
      char '!'
      (string "help" >> return Help)
       <|> (string "log" >> takeWhile isSpace >> decimal >>= return . GetLogs)
       <|> (string "ping" >> return Ping)
       <|> (string "alias" >> takeWhile True >>= return . Alias . Text.unpack)
      
data BotCommand = GetLogs Int | Help | Ping | Alias String
  
--The presence handler takes in presences and looks for subscription requests. Upon finding one, it subscribes them back
--and adds them to the roster.
handlePresences :: Session -> IO a
handlePresences sess = forever $ do
  pres <- waitForPresence (\p -> isJust (presenceFrom p)) sess --only deal with presences who have a 'from'
  let Just sender = presenceFrom pres
  case presenceType pres of
    Subscribe -> do
      sendPresence (presenceSubscribed sender) sess --subscribe in turn, server handles roster
    Unsubscribe -> do
      sendPresence (presenceUnsubscribe sender) sess --unsub in turn, server handles roster
    _ -> do
      return undefined

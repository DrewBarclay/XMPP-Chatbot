{-# LANGUAGE OverloadedStrings #-}

module Handlers (
  handleMessages,
  handlePresences,
  handleExit
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
import System.Random
import Control.Concurrent
import qualified Control.Exception as E
import System.Posix.Signals
import System.Exit

--The message handler takes in messages and broadcasts them to everyone on the roster.
handleMessages :: BotData -> IO a
handleMessages bd@BotData {session=sess, users=us, logs=ls, botJid=bj} = forever $ do
  msg <- waitForMessage (\m -> isJust (messageFrom m) && isJust (XmlUtils.unwrapMessage (messagePayload m))) sess
  
  let filterText = Text.replace "<" "≺"
  let (Just sender) = messageFrom msg
  let (Just payload) = fmap (fmap $ XmlUtils.mapNodeText filterText) $ XmlUtils.unwrapMessage (messagePayload msg)
  let s = XmlUtils.nodesToString payload
  alias <- fmap Users.alias $ Users.getUser sender us
  let broadcastMsg = XmlUtils.boldText alias : XmlUtils.text ": " : payload

  --putStrLn (show payload)

  --Check if command
  if head s == '!'
    then parseCommand s sender
    else sendMessageToAllBut [sender] bd broadcastMsg

  where 
    parseCommand :: String -> Jid -> IO ()
    parseCommand s sender = case parseOnly parser (Text.pack s) of
      Left e -> sendMessageTo sender bd $ [XmlUtils.italicsText $ "Incorrect command syntax."]
      Right Help -> sendMessageTo sender bd $ [XmlUtils.italicsText "This bot is here to help! Commands: roll <num>d<num>, help, log <number>, ping, alias <name>, list"]
      Right (GetLogs i) -> do
        lastLogs <- getLastLogs i ls 
        sendMessageTo sender bd $ List.intercalate [XmlUtils.newline] $ [XmlUtils.boldText "Last logs:"] : lastLogs
      Right Ping -> sendMessageTo sender bd $ [XmlUtils.italicsText "PONG!"] 
      Right (Alias a) -> do
        oldAlias <- fmap Users.alias $ Users.getUser sender us
        Users.setUserAlias a sender us
        sendMessageToAll bd $ [XmlUtils.italicsNode [XmlUtils.boldText oldAlias, XmlUtils.text " is now known as ", XmlUtils.boldText a, XmlUtils.text "."]]
      Right (List) -> do
        ps <- atomically $ getAvailablePeers sess
        ls <- forM (filter (/=bj) ps) (\j -> do
          u <- Users.getUser j us
          return [XmlUtils.boldText (Users.alias u), XmlUtils.text $ " (" ++ Text.unpack (jidToText . toBare $ Users.jid u) ++ ")"])
        sendMessageTo sender bd $ List.intercalate [XmlUtils.newline] $ [XmlUtils.boldText "Users in this chat:"] : ls
      Right (Roll numDice numSides) -> do     
        rolls <- replicateM numDice $ randomRIO (1, numSides)
        alias <- fmap Users.alias $ Users.getUser sender us
        sendMessageToAll bd $ [XmlUtils.italicsNode [XmlUtils.boldText alias, XmlUtils.text (" rolls " ++ show numDice ++ "d" ++ show numSides ++ ". "), XmlUtils.boldText "Result: ", XmlUtils.text (show rolls)]]
 
    parser :: Parser BotCommand
    parser = do
      char '!'
      (string "help" >> return Help)
       <|> (string "log" >> takeWhile isSpace >> decimal >>= return . GetLogs)
       <|> (string "ping" >> return Ping)
       <|> (string "alias" >> takeWhile isSpace >> takeWhile (const True) >>= return . Alias . Text.unpack)
       <|> (string "list" >> return List)
       <|> (string "roll" >> takeWhile isSpace >> decimal >>= \d1 -> string "d" >> decimal >>= \d2 -> return $ Roll (min 100 d1) (min 10000 d2))
      
data BotCommand = GetLogs Int | Help | Ping | Alias String | List | Roll Int Int
  
--The presence handler takes in presences and looks for subscription requests. Upon finding one, it subscribes them back
--and adds them to the roster.
handlePresences :: BotData -> IO a
handlePresences bd@BotData{session=sess} = forever $ do
  pres <- waitForPresence (\p -> isJust (presenceFrom p)) sess --only deal with presences who have a 'from'
  let Just sender = presenceFrom pres
  case presenceType pres of
    Subscribe -> do
      sendPresence (presenceSubscribe sender) sess --subscribe in turn, server handles roster
      sendPresence (presenceSubscribed sender) sess
      sendMessageTo sender bd $ [XmlUtils.italicsText "You have subscribed to the bot. In order to receive messages, you must accept the subscription request sent to you. If you accidentally decline, unsubscribe from this bot and then resubscribe."]
    Unsubscribe -> do
      sendPresence (presenceUnsubscribe sender) sess --unsub in turn, server handles roster
      sendPresence (presenceUnsubscribed sender) sess
      return ()
    _ -> do
      return undefined

handleExit :: ThreadId -> Logs -> IO ()
handleExit tid logs = do
  saveLogs logs
  E.throwTo tid ExitSuccess

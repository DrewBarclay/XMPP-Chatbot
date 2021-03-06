{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
import qualified Config

--The message handler takes in messages and broadcasts them to everyone on the roster.
handleMessages :: BotData -> IO ()
handleMessages bd@BotData {session=sess, users=us, logs=ls, botJid=bj} = forever $ do
  !msg <- waitForMessage (\m -> isJust (messageFrom m) && isJust (XmlUtils.unwrapMessage (messagePayload m))) sess
  
  let filterText = Text.replace "<" "≺"
  let (Just !sender) = messageFrom msg
  let (Just !payload) = fmap (fmap $ XmlUtils.mapNodeText filterText) $ XmlUtils.unwrapMessage (messagePayload msg)
  let !s = XmlUtils.nodesToString payload
  u <- Users.getUser sender us
  let alias = Users.alias u
  let !broadcastMsg = XmlUtils.boldText alias : XmlUtils.text ": " : payload
  let msgLength = XmlUtils.messageLength payload

  --putStrLn (show payload)

  --Check if command
  if head s == '!'
    then parseCommand s sender
    else if msgLength <= Config.messageCharacterLimit
      then sendSquelchableMessageToAllFrom sender bd broadcastMsg
      else sendMessageTo sender bd $ [XmlUtils.italicsText $ "Error: Message too long by " ++ show (msgLength - Config.messageCharacterLimit) ++ " characters."]

  where 
    parseCommand :: String -> Jid -> IO ()
    parseCommand !s !sender = case parseOnly parser (Text.pack s) of
      Left e -> sendMessageTo sender bd $ [XmlUtils.italicsText $ "Incorrect command syntax."]
      Right Help -> sendMessageTo sender bd $ [XmlUtils.italicsText "This bot is here to help! Commands: roll <num>d<num>, help, log <number>, ping, alias <name>, list, multicast"]
      Right (GetLogs i) -> do
        lastLogs <- getLastLogs i ls 
        sendMessageTo sender bd $ List.intercalate [XmlUtils.newline] $ [XmlUtils.boldText "Last logs:"] : lastLogs
      Right Ping -> sendMessageTo sender bd $ [XmlUtils.italicsText "PONG!"] 
      Right (Alias a) -> do
        if length a > 0 && length a <= 20
          then do
            u <- Users.getUser sender us
            let oldAlias = Users.alias u
            let u' = u {Users.alias = a}
            Users.setUser u' us
            sendSquelchableMessageToAll sender bd $ [XmlUtils.italicsNode [XmlUtils.boldText oldAlias, XmlUtils.text " is now known as ", XmlUtils.boldText a, XmlUtils.text "."]]
          else sendMessageTo sender bd $ [XmlUtils.italicsText "Error: You must enter an alias between 1-20 characters, eg. !alias fel."]
      Right (List) -> do
        ps <- atomically $ getAvailablePeers sess
        ls <- forM (filter (/=bj) ps) (\j -> do
          u <- Users.getUser j us
          return [XmlUtils.boldText (Users.alias u), XmlUtils.text $ " (" ++ Text.unpack (jidToText . toBare $ Users.jid u) ++ ")"])
        sendMessageTo sender bd $ List.intercalate [XmlUtils.newline] $ [XmlUtils.boldText "Users in this chat:"] : ls
      Right (Roll numDice numSides) -> do     
        rolls <- replicateM numDice $ randomRIO (1, numSides)
        alias <- fmap Users.alias $ Users.getUser sender us
        sendSquelchableMessageToAll sender bd $ [XmlUtils.italicsNode [XmlUtils.boldText alias, XmlUtils.text (" rolls " ++ show numDice ++ "d" ++ show numSides ++ ". "), XmlUtils.boldText "Result: ", XmlUtils.text (show rolls)]]
      Right (Multicast) -> do
        u <- Users.getUser sender us
        let m = not $ Users.multicast u
        let u' = u {Users.multicast = m}
        Users.setUser u' us
        sendMessageTo sender bd $ [XmlUtils.italicsText "Your multicast is now toggled to ", XmlUtils.boldText (show m), XmlUtils.italicsText "."]
      Right (Squelch rawJid) -> do
        case jidFromText (Text.pack rawJid) of
          Just j -> do
            u <- Users.getUser sender us
            let u' = u {Users.squelchList = j : Users.squelchList u}
            Users.setUser u' us
            sendMessageTo sender bd $ [XmlUtils.italicsText "You have squelched ", XmlUtils.boldText rawJid, XmlUtils.italicsText $ ". Current squelch list: " ++ (show $ fmap (Text.unpack . jidToText) (Users.squelchList u'))]
          Nothing -> do
            sendMessageTo sender bd $ [XmlUtils.italicsText "Invalid JID entered."]
      Right (Unsquelch rawJid) -> do
        case jidFromText (Text.pack rawJid) of
          Just j -> do
            u <- Users.getUser sender us
            let u' = u {Users.squelchList = List.delete j $ Users.squelchList u}
            Users.setUser u' us
            sendMessageTo sender bd $ [XmlUtils.italicsText "You have unsquelched ", XmlUtils.boldText rawJid, XmlUtils.italicsText $ ". Current squelch list: " ++ (show $ fmap (Text.unpack . jidToText) (Users.squelchList u'))]
          Nothing -> do
            sendMessageTo sender bd $ [XmlUtils.italicsText "Invalid JID entered."]

    parser :: Parser BotCommand
    parser = do
      char '!'
      (string "help" >> return Help)
       <|> (string "log" >> takeWhile isSpace >> decimal >>= return . GetLogs)
       <|> (string "ping" >> return Ping)
       <|> (string "alias" >> takeWhile isSpace >> takeWhile (const True) >>= return . Alias . Text.unpack)
       <|> (string "list" >> return List)
       <|> (string "roll" >> takeWhile isSpace >> decimal >>= \d1 -> string "d" >> decimal >>= \d2 -> return $ Roll (min 100 d1) (min 10000 d2))
       <|> (string "multicast" >> return Multicast)
       <|> (string "squelch" >> takeWhile isSpace >> takeWhile (const True) >>= return . Squelch . Text.unpack)
       <|> (string "unsquelch" >> takeWhile isSpace >> takeWhile (const True) >>= return . Unsquelch . Text.unpack)
      
data BotCommand = GetLogs Int | Help | Ping | Alias String | List | Roll Int Int | Multicast | Squelch String | Unsquelch String
  
--The presence handler takes in presences and looks for subscription requests. Upon finding one, it subscribes them back
--and adds them to the roster.
handlePresences :: BotData -> IO ()
handlePresences bd@BotData{session=sess} = forever $ do
  !pres <- waitForPresence (\p -> isJust (presenceFrom p)) sess --only deal with presences who have a 'from'
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
  putStrLn "Interrupt detected, exiting..."
  E.throwTo tid (ExitSuccess) --exit code in main code

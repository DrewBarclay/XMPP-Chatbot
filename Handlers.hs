{-# LANGUAGE OverloadedStrings #-}

module Handlers (
  handleMessages,
  handlePresences
) where

import Network.Xmpp
import Network.Xmpp.IM
import Control.Concurrent.STM
import Control.Monad hiding (forM)
import Data.Traversable
import qualified Users
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.XML.Types as Xml
import qualified XmlUtils

--The message handler takes in messages and broadcasts them to everyone on the roster.
handleMessages :: Session -> TVar Users.Users -> IO a
handleMessages sess users = forever $ do
  msg <- waitForMessage (\m -> isJust (messageFrom m) && isJust (XmlUtils.unwrapMessage (messagePayload m))) sess
  let (Just sender) = messageFrom msg
  let (Just payload) = XmlUtils.unwrapMessage (messagePayload msg)
  r <- getRoster sess
  us <- readTVarIO users
  let alias = Users.alias $ Users.getUser sender us
  let payload' = XmlUtils.wrapMessage (XmlUtils.boldText alias : XmlUtils.text ": " : payload)
  case answerMessage msg payload' of
    Nothing -> return undefined
    Just answer -> do
      forM (items r) (\i -> 
        sendMessage (answer {messageTo = Just $ riJid i}) sess)

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

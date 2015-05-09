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
import Users
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.XML.Types as Xml

--The message handler takes in messages and broadcasts them to everyone on the roster.
handleMessages :: Session -> TVar Users -> IO a
handleMessages sess users = forever $ do
  msg <- waitForMessage (\m -> isJust (messageFrom m) && or (map (not . Text.null) (messagePayload m >>= Xml.elementText))) sess
  r <- getRoster sess
  case answerMessage msg (messagePayload msg) of
    Nothing -> return undefined
    Just answer -> do
      --Construct new payload, literally using XML. ***todo: make a utility function for this, learn Data.Xml.Pickle***
      --Based on sample XML: [Element {elementName = Name {nameLocalName = "active", nameNamespace = Just "http://jabber.org/protocol/chatstates", namePrefix = Nothing}, elementAttributes = [], elementNodes = []},Element {elementName = Name {nameLocalName = "body", nameNamespace = Just "jabber:client", namePrefix = Nothing}, elementAttributes = [], elementNodes = [NodeContent (ContentText "test")]}]
      let newPayload = Xml.Element {Xml.elementName = Xml.Name "span" Nothing Nothing, Xml.elementAttributes = [], Xml.elementNodes = [Xml.NodeContent (Xml.ContentText "test: ")]} : messagePayload answer
      forM (items r) (\i -> 
        sendMessage (answer { messageTo = Just $ riJid i, messagePayload = newPayload }) sess)

--The presence handler takes in presences and looks for subscription request. Upon finding one, it subscribes them back
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

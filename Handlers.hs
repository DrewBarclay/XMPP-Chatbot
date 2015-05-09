module Handlers (handleMessages) where

import Network.Xmpp
import Network.Xmpp.IM
import Control.Concurrent.STM
import Control.Monad hiding (forM)
import Data.Traversable
import Users
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.XML.Types as Xml

handleMessages :: Session -> TVar Users -> IO a
handleMessages sess users = do 
  forever $ do
    msg <- waitForMessage (\m -> isJust (messageFrom m) && or (map (not . Text.null)) (messagePayload m >>= Xml.elementText)) sess
    r <- getRoster sess
    case answerMessage msg (messagePayload msg) of
      Nothing -> return undefined
      Just answer -> forM (items r) (\i -> 
        sendMessage (answer { messageTo = Just $ riJid i }) sess)

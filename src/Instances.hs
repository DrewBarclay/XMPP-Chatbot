{-# LANGUAGE StandaloneDeriving #-}

module Instances where

import Data.XML.Types
import Network.Xmpp
import Data.Binary
import Data.Maybe

deriving instance Read Node
deriving instance Read Content
deriving instance Read Instruction
deriving instance Read Element
deriving instance Read Name

instance Binary Jid where
  get = fmap (fromJust . jidFromText) get
  put = put . jidToText

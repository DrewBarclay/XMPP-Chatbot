{-# LANGUAGE StandaloneDeriving #-}

module Instances where

import Data.XML.Types

deriving instance Read Node
deriving instance Read Content
deriving instance Read Instruction
deriving instance Read Element
deriving instance Read Name

{-# LANGUAGE BangPatterns #-}


import qualified Users
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM

main = do
  users <- Users.getUsers
  atomically $ do
    !us <- readTVar users
    let us' = fmap clipAlias us
    writeTVar users us' 
  Users.saveUsers users
  where
    clipAlias u = u { Users.alias = take 2 (Users.alias u) }
  

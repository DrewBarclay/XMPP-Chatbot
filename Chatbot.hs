{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Default
import Network.Xmpp
import Network.Xmpp.IM
import System.Log.Logger
import Control.Lens
import qualified Network.TLS as TLS
import qualified Handlers
import qualified Users
import Control.Concurrent.Async
import Control.Concurrent.STM

main :: IO ()
main = do
  updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG

  --connect to the XMPP server
  result <- session
               "thegoodsirs.net"
                (Just (\_ -> ( [scramSha1 "bot2" Nothing "password"])
                             , Nothing))
                (def & tlsUseNameIndicationL .~ True
                 & osc .~ (\_ _ _ _ -> return []))
  sess <- case result of
              Right s -> return s
              Left e -> error $ "XmppFailure: " ++ (show e)

  --init bot stuff
  sendPresence def sess

  users <- newTVarIO =<< Users.getUsers
  

  --finally, pass off everything to handlers
  sess2 <- dupSession sess
  as <- mapM async [Handlers.handleMessages sess2 users]

  forM_ as wait --infinite wait
  where
    osc = streamConfigurationL . tlsParamsL . clientHooksL . onServerCertificateL
    clientHooksL = lens TLS.clientHooks (\cp ch -> cp{TLS.clientHooks = ch})
    onServerCertificateL = lens TLS.onServerCertificate (\ch osc -> ch {TLS.onServerCertificate = osc})

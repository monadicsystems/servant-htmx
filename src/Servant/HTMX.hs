{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.HTMX where

import Data.Text
import Servant
import Servant.API
import Servant.Server

-- | Request headers (sent by the client to the server)
type HXRequest = Header "HX-Request" Text
type HXTriggerId = Header "HX-Trigger" Text
type HXTriggerName = Header "HX-Trigger-Name" Text
type HXTarget = Header "HX-Target" Text
type HXPrompt = Header "HX-Prompt" Text

-- | Response headers (sent by the server to the client)
type HXPush = Header "HX-Push" Text -- use safelinks
type HXRedirect = Header "HX-Redirect" Text -- use safelinks
type HXRefresh = Header "HX-Refresh" Text 
type HXTrigger = Header "HX-Trigger" Text
type HXTriggerAfterSwap = Header "HX-Trigger-After-Swap" Text
type HXTriggerAfterSettle = Header "HX-Trigger-After-Settle" Text

-- | Example usage of htmx header types
type ExampleAPI = HXRequest :> Get '[JSON] Text
                :<|> HXTriggerId :> Post '[JSON] Text
                :<|> "somePath" :> Get '[JSON] (Headers '[HXPush, HXRedirect] Text)

exampleServer :: Server ExampleAPI
exampleServer = exampleGetHandler
              :<|> examplePostHandler
              :<|> exampleSomePathHandler
  where
    exampleGetHandler :: Maybe Text -> Handler Text
    exampleGetHandler mb = case mb of
      Just "true" -> pure "The request was sent to the server by htmx" 
      _ -> pure "The request wasn't sent to the server by htmx" 

    examplePostHandler :: Maybe Text -> Handler Text
    examplePostHandler mb = case mb of
      Just "adminPanel" -> pure "The request was triggered by the admin panel" 
      _ -> pure "The request wasn't triggered by the admin panel" 

    exampleSomePathHandler :: Handler (Headers '[HXPush, HXRedirect] Text)
    exampleSomePathHandler = pure $ noHeader $ addHeader "someURLForRedirect" "This response has htmx headers" 

exampleApp :: Application
exampleApp = serve (Proxy :: Proxy ExampleAPI) exampleServer

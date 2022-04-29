{-# LANGUAGE DataKinds #-}

-- | See <https://htmx.org/reference/#request_headers> and <https://htmx.org/reference/#response_headers>
-- to see all htmx request and response headers, and how to use them.
--
-- Example Servant API:
--
-- @
-- module MyServer where
--
-- import Servant.API
-- import Servant.Server
--
-- -- | Example usage of htmx header types
-- type ExampleAPI = HXRequest :> Get '[JSON] Text
--                :\<|\> HXTriggerId :> Post '[JSON] Text
--                :\<|\> "somePath" :> Get '[JSON] (Headers '[HXPush, HXRedirect] Text)
--
-- exampleServer :: Server ExampleAPI
-- exampleServer = exampleGetHandler
--              :\<|\> examplePostHandler
--              :\<|\> exampleSomePathHandler
--  where
--    exampleGetHandler :: Maybe Text -> Handler Text
--    exampleGetHandler mb = case mb of
--      Just "true" -> pure "The request was sent to the server by htmx"
--      _ -> pure "The request wasn't sent to the server by htmx"
--
--    examplePostHandler :: Maybe Text -> Handler Text
--    examplePostHandler mb = case mb of
--      Just "adminPanel" -> pure "The request was triggered by the admin panel"
--      _ -> pure "The request wasn't triggered by the admin panel"
--
--    exampleSomePathHandler :: Handler (Headers '[HXPush, HXRedirect] Text)
--    exampleSomePathHandler = pure $ noHeader $ addHeader "someURLForRedirect" "This response has htmx headers"
--
-- exampleApp :: Application
-- exampleApp = serve (Proxy :: Proxy ExampleAPI) exampleServer
-- @
module Servant.Htmx
  ( HXRequest,
    HXTriggerId,
    HXTriggerName,
    HXTarget,
    HXPrompt,
    HXPush,
    HXRedirect,
    HXRefresh,
    HXTrigger,
    HXTriggerAfterSwap,
    HXTriggerAfterSettle,
  )
where

import Data.Text (Text)
import Servant (Header)

type HXRequest = Header "HX-Request" Text

type HXTriggerId = Header "HX-Trigger" Text

type HXTriggerName = Header "HX-Trigger-Name" Text

type HXTarget = Header "HX-Target" Text

type HXPrompt = Header "HX-Prompt" Text

type HXPush = Header "HX-Push" Text

type HXRedirect = Header "HX-Redirect" Text

type HXRefresh = Header "HX-Refresh" Text

type HXTrigger = Header "HX-Trigger" Text

type HXTriggerAfterSwap = Header "HX-Trigger-After-Swap" Text

type HXTriggerAfterSettle = Header "HX-Trigger-After-Settle" Text

type HXBoosted = Header "HX-Boosted" Text

type HXCurrentURL = Header "HX-Current-URL" Text

type HXHistoryRestoreRequest = Header "HX-History-Restore-Request" Text

type HXRetarget = Header "HX-Retarget" Text

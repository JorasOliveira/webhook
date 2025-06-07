{-# LANGUAGE OverloadedStrings #-}

module Webhook.External (
    sendWebhookNotification
) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, encode)
import Network.HTTP.Simple (httpNoBody, parseRequest, setRequestBodyLBS,
                            setRequestHeader, setRequestMethod, getResponseStatusCode)

-- | Sends a POST notification with a JSON payload to the specified URL.
-- Returns the HTTP status code of the response or an exception.
sendWebhookNotification :: (MonadIO m, ToJSON p)
                        => String                           -- ^ Target URL
                        -> p                                -- ^ Payload to send (must be ToJSON)
                        -> m (Either SomeException Int)     -- ^ Status code or exception
sendWebhookNotification url payload = liftIO $ do
    eresult <- try $ do
        initialRequest <- parseRequest url
        let request = setRequestMethod "POST"
                    $ setRequestHeader "Content-Type" ["application/json; charset=utf-8"]
                    $ setRequestBodyLBS (encode payload)
                    $ initialRequest
        response <- httpNoBody request -- Use httpLBS or httpJSON if you need the response body
        let status = getResponseStatusCode response
        liftIO $ putStrLn $ "Notification sent to " ++ url ++ ". Status: " ++ show status
        return status

    case eresult of
        Left ex -> do
            liftIO $ putStrLn $ "Failed to send notification to " ++ url ++ ": " ++ show ex
            return $ Left ex
        Right status -> return $ Right status
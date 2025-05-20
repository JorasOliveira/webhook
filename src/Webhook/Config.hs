{-# LANGUAGE OverloadedStrings #-}

module Webhook.Config (
    AppConfig (..),
    loadAppConfig
) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

-- | Holds application configuration.
data AppConfig = AppConfig {
    cfgAuthToken      :: !Text,
    cfgConfirmationUrl :: !String,
    cfgCancellationUrl :: !String,
    cfgServerPort     :: !Int
} deriving (Show)

-- | Loads configuration, prioritizing environment variables over defaults.
loadAppConfig :: IO AppConfig
loadAppConfig = do
    -- Auth Token
    envAuthToken <- lookupEnv "WEBHOOK_AUTH_TOKEN"
    let authToken = maybe "SECRET_TOKEN_FROM_GATEWAY" T.pack envAuthToken

    -- Confirmation URL
    envConfirmationUrl <- lookupEnv "CONFIRMATION_URL"
    let confirmationUrl = maybe "http://localhost:3001/payment-confirmed" id envConfirmationUrl

    -- Cancellation URL
    envCancellationUrl <- lookupEnv "CANCELLATION_URL"
    let cancellationUrl = maybe "http://localhost:3002/payment-cancelled" id envCancellationUrl

    -- Server Port
    envPortStr <- lookupEnv "WEBHOOK_PORT"
    let port = case envPortStr of
                 Just portStr -> maybe 3000 read portStr
                 Nothing      -> 3000
    
    return AppConfig {
        cfgAuthToken = authToken,
        cfgConfirmationUrl = confirmationUrl,
        cfgCancellationUrl = cancellationUrl,
        cfgServerPort = port
    }
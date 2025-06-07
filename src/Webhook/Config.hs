{-# LANGUAGE OverloadedStrings #-}

module Webhook.Config (
    AppConfig(..),
    loadAppConfig
) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe) -- Added for safe parsing

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
    mAuthToken <- lookupEnv "WEBHOOK_AUTH_TOKEN"
    let authToken = maybe "meu-token-secreto" T.pack mAuthToken

    -- Confirmation URL
    mConfirmationUrl <- lookupEnv "CONFIRMATION_URL"
    let confirmationUrl = maybe "http://localhost:5001/confirmar" id mConfirmationUrl

    -- Cancellation URL
    mCancellationUrl <- lookupEnv "CANCELLATION_URL"
    let cancellationUrl = maybe "http://localhost:5001/cancelar" id mCancellationUrl

    -- Server Port (FIXED)
    mPortStr <- lookupEnv "WEBHOOK_PORT"
    let port = maybe 3000 id (mPortStr >>= readMaybe)
    
    return AppConfig {
        cfgAuthToken = authToken,
        cfgConfirmationUrl = confirmationUrl,
        cfgCancellationUrl = cancellationUrl,
        cfgServerPort = port
    }
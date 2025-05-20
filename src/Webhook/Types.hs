{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Webhook.Types (
    PaymentPayload (..)
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Represents the structure of the incoming payment webhook payload.
data PaymentPayload = PaymentPayload {
    event          :: !Text,   -- ^ Type of event, e.g., "payment_success"
    transaction_id :: !Text,   -- ^ Unique identifier for the transaction
    amount         :: !Double, -- ^ Payment amount
    currency       :: !Text,   -- ^ Currency code (e.g., "BRL", "USD")
    timestamp      :: !Text    -- ^ ISO 8601 timestamp of the event
} deriving (Show, Eq, Generic) -- Added Eq for potential comparisons

-- BangPatterns (!) are used for strictness, which can be beneficial for performance
-- by avoiding unnecessary thunks, especially in data structures processed frequently.

instance FromJSON PaymentPayload where
    parseJSON = withObject "PaymentPayload" $ \v -> PaymentPayload
        <$> v .: "event"
        <*> v .: "transaction_id"
        <*> v .: "amount"
        <*> v .: "currency"
        <*> v .: "timestamp"

instance ToJSON PaymentPayload
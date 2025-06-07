{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Webhook.Types (
    PaymentPayload (..)
) where

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:))
import Data.Text (Text)
import qualified Data.Text as T -- Import qualified for T.unpack
import GHC.Generics (Generic)
import Control.Applicative ((<|>)) -- To combine parsers
import Text.Read (readMaybe) -- For safe string-to-number parsing
import Control.Monad (mzero) -- For failing the parse

-- | Represents the structure of the incoming payment webhook payload.
data PaymentPayload = PaymentPayload {
    event          :: !Text,   -- ^ Type of event, e.g., "payment_success"
    transaction_id :: !Text,   -- ^ Unique identifier for the transaction
    amount         :: !Double, -- ^ Payment amount
    currency       :: !Text,   -- ^ Currency code (e.g., "BRL", "USD")
    timestamp      :: !Text    -- ^ ISO 8601 timestamp of the event
} deriving (Show, Eq, Generic)

-- ToJSON can still be derived automatically, as we only need custom logic for *reading* JSON.
instance ToJSON PaymentPayload

-- We provide a custom FromJSON instance to handle the 'amount' field,
-- which might come in as a String instead of a Number.
instance FromJSON PaymentPayload where
    parseJSON = withObject "PaymentPayload" $ \v -> do
        -- This is the special parser for the 'amount' field
        let parseAmount =
                -- First, try to parse it as a standard Double (a JSON number)
                (v .: "amount") <|>
                -- If that fails, try to parse it as a Text (a JSON string)
                -- and then manually convert that Text to a Double.
                (do textAmount <- v .: "amount"
                    -- Safely read the string value. `readMaybe` returns a Maybe Double.
                    -- T.unpack converts Text to String for readMaybe.
                    case readMaybe (T.unpack (textAmount :: Text)) of
                        Just d  -> return d
                        -- If readMaybe fails (returns Nothing), we fail the entire JSON parse.
                        Nothing -> mzero
                )

        -- Now, we build the final PaymentPayload, using our special parser for 'amount'.
        PaymentPayload
            <$> v .: "event"
            <*> v .: "transaction_id"
            <*> parseAmount -- Use our custom amount parser here
            <*> v .: "currency"
            <*> v .: "timestamp"
{-# LANGUAGE OverloadedStrings #-}

module Webhook.Validation (
    isValidPayload
) where

import qualified Data.Text as T
import Webhook.Types (PaymentPayload(..))

-- | Validates the content of the 'PaymentPayload'.
-- Returns True if the payload is considered valid, False otherwise.
isValidPayload :: PaymentPayload -> Bool
isValidPayload p =
    event p == "payment_success" &&  -- Check for the correct event type [cite: 18]
    amount p > 0.0 &&                -- Amount should be positive [cite: 20]
    not (T.null (currency p)) &&     -- Currency should not be empty
    not (T.null (timestamp p))       -- Timestamp should not be empty
    -- Note: The requirement "Se alguma informação faltante (exceto transaction_id), deve-se cancelar" [cite: 20]
    -- is primarily handled by Aeson's parsing ensuring field presence.
    -- This function checks the *values* of those present fields.
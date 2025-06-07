{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Webhook.Handler (
    webhookPostHandler
) where

import Web.Scotty (ActionM, body, header, json, status, liftIO)
-- ** THE FIX IS HERE: Added (.=) to the import list **
import Data.Aeson (FromJSON(..), eitherDecode, object, withObject, (.:), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (status200, status400, status401, status409, status422)
import GHC.Generics (Generic)

-- Local Imports
import Webhook.Types (PaymentPayload (..))
import Webhook.Config (AppConfig (..))
import Webhook.State (ProcessedTxIds, checkAndMarkTransaction)
import Webhook.External (sendWebhookNotification)
import Webhook.Validation (isValidPayload)

-- A helper data type to extract only the transaction_id for robust error handling.
data TxIdOnly = TxIdOnly { tx_id :: Text } deriving (Generic)
instance FromJSON TxIdOnly where
    parseJSON = withObject "TxIdOnly" $ \v -> TxIdOnly <$> v .: "transaction_id"

webhookPostHandler :: AppConfig -> ProcessedTxIds -> ActionM ()
webhookPostHandler appCfg txIdRef = do
    liftIO $ putStrLn "Received POST request on /webhook"
    mAuthHeader <- header "X-Webhook-Token"

    case mAuthHeader of
        Nothing -> do
            liftIO $ putStrLn "X-Webhook-Token header missing."
            status status401
            json $ object ["error" .= ("X-Webhook-Token header missing" :: Text)]

        Just authHeaderVal -> do
            let expectedToken = TL.fromStrict (cfgAuthToken appCfg)
            if authHeaderVal /= expectedToken then do
                liftIO $ putStrLn "Invalid token. Failing test."
                status status401
                json $ object ["error" .= ("Invalid token" :: Text)]
            else do
                liftIO $ putStrLn "Token authenticated."
                rawBody <- body

                case eitherDecode rawBody :: Either String TxIdOnly of
                    Left _ -> do
                        liftIO $ putStrLn "JSON parsing error: could not find transaction_id."
                        status status400
                        json $ object ["error" .= ("Malformed request: missing transaction_id" :: Text)]
                    
                    Right (TxIdOnly txId) -> do
                        case eitherDecode rawBody :: Either String PaymentPayload of
                            Left err -> do
                                liftIO $ putStrLn $ "Incomplete payload for txId " ++ T.unpack txId ++ ". Error: " ++ err
                                _ <- liftIO $ sendWebhookNotification (cfgCancellationUrl appCfg) (object ["transaction_id" .= txId, "reason" .= ("Incomplete or malformed payload." :: Text)])
                                status status422
                                json $ object ["message" .= ("Incomplete payload. Cancellation initiated." :: Text), "transaction_id" .= txId]
                            
                            Right payload -> do
                                liftIO $ putStrLn $ "Parsed payload: " ++ show payload
                                isDuplicate <- liftIO $ checkAndMarkTransaction txIdRef txId

                                if isDuplicate then do
                                    liftIO $ putStrLn $ "Duplicate transaction_id: " ++ T.unpack txId
                                    status status409
                                    json $ object ["message" .= ("Duplicate transaction. Already processed." :: Text), "transaction_id" .= txId]
                                else do
                                    if not (isValidPayload payload) then do
                                        liftIO $ putStrLn $ "Invalid payload data for txId: " ++ T.unpack txId
                                        _ <- liftIO $ sendWebhookNotification (cfgCancellationUrl appCfg) (object ["transaction_id" .= txId, "reason" .= ("Invalid data in payload." :: Text)])
                                        status status422
                                        json $ object ["message" .= ("Transaction data invalid. Cancellation initiated." :: Text), "transaction_id" .= txId]
                                    else do
                                        liftIO $ putStrLn $ "Transaction OK for txId: " ++ T.unpack txId
                                        _ <- liftIO $ sendWebhookNotification (cfgConfirmationUrl appCfg) payload
                                        status status200
                                        json $ object ["message" .= ("Transaction successful. Confirmation initiated." :: Text), "transaction_id" .= txId]
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} -- For type annotation on 'eitherDecode' result

module Webhook.Handler (
    webhookPostHandler
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Web.Scotty.Trans (ActionT, body, header, json, status) -- Scotty's ActionT
import Data.Aeson (eitherDecode, object, (.=))
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Network.HTTP.Types.Status (status200, status400, status401)

-- Local Imports
import Webhook.Types (PaymentPayload (..))
import Webhook.Config (AppConfig (..)) -- For accessing config fields
import Webhook.State (ProcessedTxIds, checkAndMarkTransaction)
import Webhook.External (sendWebhookNotification)
import Webhook.Validation (isValidPayload)

-- | Handles the POST request to the /webhook endpoint.
webhookPostHandler :: (MonadIO m)
                   => AppConfig         -- ^ Application configuration
                   -> ProcessedTxIds    -- ^ Reference to the set of processed transaction IDs
                   -> ActionT TL.Text m () -- Using TL.Text as Scotty's error type
webhookPostHandler appCfg txIdRef = do
    liftIO $ putStrLn "Received POST request on /webhook"

    mAuthHeader <- header "Authorization" -- Get Authorization header

    case mAuthHeader of
        Nothing -> do
            liftIO $ putStrLn "Authorization header missing."
            status status401 -- Unauthorized
            json $ object ["error" .= ("Authorization header missing" :: Text)]
        Just authHeaderVal -> do
            let expectedFullToken = "Bearer " <> cfgAuthToken appCfg
            if TL.toStrict authHeaderVal /= expectedFullToken then do
                liftIO $ putStrLn $ "Invalid token. Expected: '" ++ T.unpack expectedFullToken ++ "', Got: '" ++ TL.unpack authHeaderVal ++ "'"
                -- "Se o token estiver errado, é uma transação falsa e deve-se ignorá-la" [cite: 20]
                status status200 -- Return 200 OK to not reveal token failure.
                json $ object ["message" .= ("Request ignored due to invalid token." :: Text)]
            else do
                liftIO $ putStrLn "Token authenticated."
                rawBody <- body -- Get the raw request body
                
                -- Attempt to decode the JSON payload
                case eitherDecode rawBody :: Either String PaymentPayload of
                    Left err -> do
                        liftIO $ putStrLn $ "JSON parsing error: " ++ err
                        status status400 -- Bad Request for malformed JSON
                        json $ object ["error" .= ("Invalid JSON payload" :: Text), "details" .= err]
                    Right payload -> do
                        liftIO $ putStrLn $ "Parsed payload: " ++ show payload
                        let txId = transaction_id payload

                        -- Check for duplicate transaction and mark if new
                        isDuplicate <- checkAndMarkTransaction txIdRef txId

                        if isDuplicate then do
                            liftIO $ putStrLn $ "Duplicate transaction_id: " ++ T.unpack txId ++ ". Acknowledging without reprocessing."
                            status status200 -- Idempotency: Acknowledge receipt [cite: 15]
                            json $ object ["message" .= ("Duplicate transaction. Already processed." :: Text), "transaction_id" .= txId]
                        else do
                            -- Validate payload content
                            if not (isValidPayload payload) then do
                                liftIO $ putStrLn $ "Invalid payload data for txId: " ++ T.unpack txId ++ ". Initiating cancellation."
                                -- "Se alguma informação estiver errada (ex: valor), deve-se cancelar a transação fazendo um request" [cite: 20]
                                -- "Se alguma informação faltante (exceto transaction_id), deve-se cancelar a transação fazendo um request" [cite: 20]
                                _ <- sendWebhookNotification (cfgCancellationUrl appCfg) (object ["transaction_id" .= txId, "reason" .= ("Invalid data in payload." :: Text)])
                                -- "Se uma transação não estiver ok, não deve retornar 400" [cite: 20]
                                status status200 -- Acknowledge receipt, cancellation handled async
                                json $ object ["message" .= ("Transaction data invalid. Cancellation initiated." :: Text), "transaction_id" .= txId]
                            else do
                                -- All checks passed, transaction is OK
                                liftIO $ putStrLn $ "Transaction OK for txId: " ++ T.unpack txId ++ ". Initiating confirmation."
                                -- "Se uma transação estiver ok, deve-se retornar 200 e fazer um request em uma url de confirmação" [cite: 20]
                                _ <- sendWebhookNotification (cfgConfirmationUrl appCfg) payload -- Send full payload
                                status status200
                                json $ object ["message" .= ("Transaction successful. Confirmation initiated." :: Text), "transaction_id" .= txId]
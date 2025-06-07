{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (scotty, post)

-- Local Imports
import Webhook.Handler (webhookPostHandler)
import Webhook.State (newProcessedTxIds)
import Webhook.Config (loadAppConfig, cfgServerPort)

main :: IO ()
main = do
    putStrLn "Loading configuration..."
    appConfig <- loadAppConfig
    putStrLn "Configuration loaded."

    processedTxsRef <- newProcessedTxIds
    putStrLn "Shared state initialized."

    let port = cfgServerPort appConfig
    putStrLn $ "Webhook server starting on port " ++ show port ++ "..."

    scotty port $ do
        post "/webhook" $ webhookPostHandler appConfig processedTxsRef
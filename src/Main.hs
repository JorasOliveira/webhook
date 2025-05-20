{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty.Trans (scottyOpts, post, defaultHandler, status, json, Options(..), settings)
-- It's common to use Scotty.Trans for custom monad stacks, but for IO, Web.Scotty is simpler.
-- However, since Handler uses ActionT m (), using Scotty.Trans.scottyT is fine.
-- For basic IO, we can use 'scotty' from Web.Scotty if ActionT is ActionM
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status (status500)
import Network.Wai.Handler.Warp (setPort, defaultSettings) -- For setting port in Scotty Options

-- Local Imports
import Webhook.Handler (webhookPostHandler)
import Webhook.State (newProcessedTxIds)
import Webhook.Config (AppConfig(..), loadAppConfig) -- Load the full config

main :: IO ()
main = do
    -- Load application configuration
    appConfig <- loadAppConfig
    liftIO $ putStrLn $ "Loaded configuration: " ++ show appConfig
    liftIO $ putStrLn $ "Authentication token for bearer: " ++ show (cfgAuthToken appConfig)

    -- Initialize shared state
    processedTxsRef <- newProcessedTxIds

    liftIO $ putStrLn $ "Webhook server starting on port " ++ show (cfgServerPort appConfig) ++ "..."

    let scottyOptions = Options {
            verbose = 1, -- Prints requests to stdout (0 for quiet)
            settings = setPort (cfgServerPort appConfig) defaultSettings
        }

    -- Run the Scotty server.
    -- scottyT uses (forall a. m a -> IO (Either e a)) for its runner if 'm' is not IO.
    -- For ActionT TL.Text IO (), the runner is simple.
    -- We can use scottyOpts from Web.Scotty, which is scottyOpts :: Options -> ScottyM () -> IO ()
    -- Since our handler is ActionT TL.Text IO (), which is compatible with ScottyM, this works.
    scottyOpts scottyOptions $ do
        -- Custom default error handler for unhandled exceptions within Scotty
        defaultHandler $ \e -> {-- e is TL.Text by default --} do
            liftIO $ putStrLn $ "Unhandled Scotty error: " ++ TL.unpack e
            status status500 -- Internal Server Error
            json $ object ["error" .= ("An unexpected server error occurred." :: TL.Text), "details" .= e]

        -- Define the POST route for the webhook
        post "/webhook" $
            webhookPostHandler appConfig processedTxsRef
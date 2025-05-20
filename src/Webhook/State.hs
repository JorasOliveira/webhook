module Webhook.State (
    ProcessedTxIds,
    newProcessedTxIds,
    checkAndMarkTransaction
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Type alias for the IORef holding the set of processed transaction IDs.
type ProcessedTxIds = IORef (Set Text)

-- | Creates a new, empty 'ProcessedTxIds' store.
newProcessedTxIds :: MonadIO m => m ProcessedTxIds
newProcessedTxIds = liftIO $ newIORef Set.empty

-- | Atomically checks if a transaction ID is already in the set.
-- If it is, returns True (indicating it's a duplicate).
-- If it's not, adds it to the set and returns False (indicating it's new).
checkAndMarkTransaction :: MonadIO m => ProcessedTxIds -> Text -> m Bool
checkAndMarkTransaction ref txId = liftIO $
    atomicModifyIORef' ref $ \txs ->
        if Set.member txId txs
        then (txs, True)  -- True means it *was* already processed (is a duplicate)
        else (Set.insert txId txs, False) -- False means it was *not* processed (is new)
module StmHamt.Concurrency.UnfoldlM where

import StmHamt.Concurrency.Prelude hiding (filter, all)
import StmHamt.Concurrency.Types
import DeferredFolds.UnfoldlM hiding (tVarValue)
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified PrimitiveExtras.By6Bits as By6Bits


hamtElements :: MonadSTM stm => Hamt stm a -> UnfoldlM stm a
hamtElements (Hamt var) = tVarValue var >>= By6Bits.elementsUnfoldlM >>= branchElements

branchElements :: MonadSTM stm => Branch stm a -> UnfoldlM stm a
branchElements = \ case
  LeavesBranch _ array -> SmallArray.elementsUnfoldlM array
  BranchesBranch hamt -> hamtElements hamt

-- | TVar contents
{-# INLINE tVarValue #-}
-- Copied from deferred-folds-0.9.18.3, generalized to MonadSTM
tVarValue :: MonadSTM stm => TVar stm a -> UnfoldlM stm a
tVarValue var = UnfoldlM $ \step state -> do
  a <- readTVar var
  step state a

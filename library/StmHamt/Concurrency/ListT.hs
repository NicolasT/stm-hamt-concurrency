module StmHamt.Concurrency.ListT where

import StmHamt.Concurrency.Prelude hiding (filter, all)
import StmHamt.Concurrency.Types
import ListT
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified PrimitiveExtras.By6Bits as By6Bits


hamtElements :: Hamt STM a -> ListT STM a
hamtElements (Hamt var) = tVarValue var >>= By6Bits.elementsListT >>= branchElements

branchElements :: Branch STM a -> ListT STM a
branchElements = \ case
  LeavesBranch _ array -> SmallArray.elementsListT array
  BranchesBranch hamt -> hamtElements hamt

tVarValue :: MonadSTM stm => TVar stm a -> ListT stm a
tVarValue var = lift (readTVar var)

module StmHamt.Concurrency.ListT where

import StmHamt.Concurrency.Prelude hiding (filter, all)
import StmHamt.Concurrency.Types
import ListT
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified PrimitiveExtras.By6Bits as By6Bits


hamtElements :: Hamt a -> ListT STM a
hamtElements (Hamt var) = tVarValue var >>= By6Bits.elementsListT >>= branchElements

branchElements :: Branch a -> ListT STM a
branchElements = \ case
  LeavesBranch _ array -> SmallArray.elementsListT array
  BranchesBranch hamt -> hamtElements hamt

tVarValue :: TVar a -> ListT STM a
tVarValue var = lift (readTVar var)

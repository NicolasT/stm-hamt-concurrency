-- |
-- HAMT API,
-- optimized for a fast 'size' operation.
-- That however comes at the cost of a small overhead in the other operations.
module StmHamt.Concurrency.SizedHamt
(
  SizedHamt,
  new,
  newIO,
  null,
  size,
  focus,
  insert,
  lookup,
  reset,
  unfoldlM,
  listT,
)
where

import StmHamt.Concurrency.Prelude hiding (insert, lookup, delete, fold, null)
import StmHamt.Concurrency.Types
import qualified Focus as Focus
import qualified StmHamt.Concurrency.Hamt as Hamt


{-# INLINE new #-}
new :: MonadSTM stm => stm (SizedHamt stm element)
new = SizedHamt <$> newTVar 0 <*> Hamt.new

{-# INLINE newIO #-}
newIO :: MonadConc m => m (SizedHamt (STM m) element)
newIO = SizedHamt <$> newTVarConc 0 <*> Hamt.newIO

-- |
-- /O(1)/.
{-# INLINE null #-}
null :: MonadSTM stm => SizedHamt stm element -> stm Bool
null (SizedHamt sizeVar _) = (== 0) <$> readTVar sizeVar

-- |
-- /O(1)/.
{-# INLINE size #-}
size :: MonadSTM stm => SizedHamt stm element -> stm Int
size (SizedHamt sizeVar _) = readTVar sizeVar

{-# INLINE reset #-}
reset :: MonadSTM stm => SizedHamt stm element -> stm ()
reset (SizedHamt sizeVar hamt) =
  do
    Hamt.reset hamt
    writeTVar sizeVar 0

{-# INLINE focus #-}
focus :: (MonadSTM stm, Eq key, Hashable key) => Focus element stm result -> (element -> key) -> key -> SizedHamt stm element -> stm result
focus focus elementToKey key (SizedHamt sizeVar hamt) =
  do
    (result, sizeModifier) <- Hamt.focus newFocus elementToKey key hamt
    forM_ sizeModifier (modifyTVar' sizeVar)
    return result
  where
    newFocus = Focus.testingSizeChange (Just pred) Nothing (Just succ) focus

{-# INLINE insert #-}
insert :: (MonadSTM stm, Eq key, Hashable key) => (element -> key) -> element -> SizedHamt stm element -> stm ()
insert elementToKey element (SizedHamt sizeVar hamt) =
  do
    inserted <- Hamt.insert elementToKey element hamt
    when inserted (modifyTVar' sizeVar succ)

{-# INLINE lookup #-}
lookup :: (MonadSTM stm, Eq key, Hashable key) => (element -> key) -> key -> SizedHamt stm element -> stm (Maybe element)
lookup elementToKey key (SizedHamt _ hamt) = Hamt.lookup elementToKey key hamt

{-# INLINE unfoldlM #-}
unfoldlM :: MonadSTM stm => SizedHamt stm a -> UnfoldlM stm a
unfoldlM (SizedHamt _ hamt) = Hamt.unfoldlM hamt

{-# INLINE listT #-}
listT :: SizedHamt (STM IO) a -> ListT (STM IO) a
listT (SizedHamt _ hamt) = Hamt.listT hamt

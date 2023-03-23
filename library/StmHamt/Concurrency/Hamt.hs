module StmHamt.Concurrency.Hamt
(
  Hamt,
  new,
  newIO,
  null,
  focus,
  focusExplicitly,
  insert,
  insertExplicitly,
  lookup,
  lookupExplicitly,
  reset,
  unfoldlM,
  listT,
)
where

import StmHamt.Concurrency.Prelude hiding (empty, insert, update, lookup, delete, null)
import StmHamt.Concurrency.Types
import qualified Focus as Focus
import qualified StmHamt.Concurrency.Focuses as Focus
import qualified StmHamt.Concurrency.UnfoldlM as UnfoldlM
import qualified StmHamt.Concurrency.ListT as ListT
import qualified StmHamt.Concurrency.IntOps as IntOps
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified PrimitiveExtras.By6Bits as By6Bits


new :: MonadSTM stm => stm (Hamt stm a)
new = Hamt <$> newTVar By6Bits.empty

newIO :: MonadConc m => m (Hamt (STM m) a)
newIO = Hamt <$> newTVarConc By6Bits.empty

focus :: (MonadSTM stm, Eq key, Hashable key) => Focus element stm result -> (element -> key) -> key -> Hamt stm element -> stm result
focus focus elementToKey key = focusExplicitly focus (hash key) ((==) key . elementToKey)

focusExplicitly :: MonadSTM stm => Focus a stm b -> Int -> (a -> Bool) -> Hamt stm a -> stm b
focusExplicitly focus hash test hamt =
  {-# SCC "focus" #-} 
  let
    Focus _ reveal = Focus.onHamtElement 0 hash test focus
    in fmap fst (reveal hamt)

{-|
Returns a flag, specifying, whether the size has been affected.
-}
insert :: (MonadSTM stm, Eq key, Hashable key) => (element -> key) -> element -> Hamt stm element -> stm Bool
insert elementToKey element = let
  !key = elementToKey element
  in insertExplicitly (hash key) ((==) key . elementToKey) element

{-|
Returns a flag, specifying, whether the size has been affected.
-}
insertExplicitly :: MonadSTM stm => Int -> (a -> Bool) -> a -> Hamt stm a -> stm Bool
insertExplicitly hash testKey element =
  {-# SCC "insertExplicitly" #-}
  let
    loop depth (Hamt var) = let
      !branchIndex = IntOps.indexAtDepth depth hash
      in do
        branchArray <- readTVar var
        case By6Bits.lookup branchIndex branchArray of
          Nothing -> do
            writeTVar var $! By6Bits.insert branchIndex (LeavesBranch hash (pure element)) branchArray
            return True
          Just branch -> case branch of
            LeavesBranch leavesHash leavesArray -> if leavesHash == hash
              then case SmallArray.findWithIndex testKey leavesArray of
                Just (leavesIndex, _) -> let
                  !newLeavesArray = SmallArray.set leavesIndex element leavesArray
                  !newBranch = LeavesBranch hash newLeavesArray
                  !newBranchArray = By6Bits.replace branchIndex newBranch branchArray
                  in do
                    writeTVar var newBranchArray
                    return False
                Nothing -> let
                  newLeavesArray = SmallArray.cons element leavesArray
                  in do
                    writeTVar var $! By6Bits.replace branchIndex (LeavesBranch hash newLeavesArray) branchArray
                    return True
              else do
                hamt <- pair (IntOps.nextDepth depth) hash (LeavesBranch hash (pure element)) leavesHash (LeavesBranch leavesHash leavesArray)
                writeTVar var $! By6Bits.replace branchIndex (BranchesBranch hamt) branchArray
                return True
            BranchesBranch hamt -> loop (IntOps.nextDepth depth) hamt
    in loop 0

pair :: MonadSTM stm => Int -> Int -> Branch stm a -> Int -> Branch stm a -> stm (Hamt stm a)
pair depth hash1 branch1 hash2 branch2 =
  {-# SCC "pair" #-}
  let
    index1 = IntOps.indexAtDepth depth hash1
    index2 = IntOps.indexAtDepth depth hash2
    in if index1 == index2
        then do
          deeperHamt <- pair (IntOps.nextDepth depth) hash1 branch1 hash2 branch2
          var <- newTVar (By6Bits.singleton index1 (BranchesBranch deeperHamt))
          return (Hamt var)
        else Hamt <$> newTVar (By6Bits.pair index1 branch1 index2 branch2)

{-|
Returns a flag, specifying, whether the size has been affected.
-}
lookup :: (MonadSTM stm, Eq key, Hashable key) => (element -> key) -> key -> Hamt stm element -> stm (Maybe element)
lookup elementToKey key = lookupExplicitly (hash key) ((==) key . elementToKey)

lookupExplicitly :: MonadSTM stm => Int -> (a -> Bool) -> Hamt stm a -> stm (Maybe a)
lookupExplicitly hash test =
  {-# SCC "lookupExplicitly" #-}
  let
    loop depth (Hamt var) = let
      !index = IntOps.indexAtDepth depth hash
      in do
        branchArray <- readTVar var
        case By6Bits.lookup index branchArray of
          Just branch -> case branch of
            LeavesBranch leavesHash leavesArray -> if leavesHash == hash
              then return (SmallArray.find test leavesArray)
              else return Nothing
            BranchesBranch hamt -> loop (IntOps.nextDepth depth) hamt
          Nothing -> return Nothing
    in loop 0

reset :: MonadSTM stm => Hamt stm a -> stm ()
reset (Hamt branchSsaVar) = writeTVar branchSsaVar By6Bits.empty

unfoldlM :: MonadSTM stm => Hamt stm a -> UnfoldlM stm a
unfoldlM = UnfoldlM.hamtElements

listT :: Hamt (STM IO) a -> ListT (STM IO) a
listT = ListT.hamtElements

null :: MonadSTM stm => Hamt stm a -> stm Bool
null (Hamt branchSsaVar) = do
  branchSsa <- readTVar branchSsaVar
  return (By6Bits.null branchSsa)

{-|
Render the structure of HAMT.
-}
introspect :: (Show a, MonadSTM stm) => Hamt stm a -> stm String
introspect (Hamt branchArrayVar) = do
  branchArray <- readTVar branchArrayVar
  indexedList <- traverse (traverse introspectBranch) (By6Bits.toIndexedList branchArray)
  return $
    "[" <> intercalate ", " (fmap (\ (i, branchString) -> "(" <> show i <> ", " <> branchString <> ")") indexedList) <> "]"
  where
    introspectBranch = \ case
      BranchesBranch deeperHamt -> do
        deeperString <- introspect deeperHamt
        return (showString "BranchesBranch " deeperString)
      LeavesBranch hash array -> return (showString "LeavesBranch " (shows hash (showChar ' ' (show (SmallArray.toList array)))))

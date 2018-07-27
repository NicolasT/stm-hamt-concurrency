module StmHamt.Hamt
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
  unfoldM,
)
where

import StmHamt.Prelude hiding (empty, insert, update, lookup, delete, null)
import StmHamt.Types
import qualified Focus as Focus
import qualified StmHamt.Focuses as Focuses
import qualified StmHamt.Constructors.Hash as HashConstructors
import qualified StmHamt.Accessors.Hash as HashAccessors
import qualified StmHamt.UnfoldMs as UnfoldMs
import qualified PrimitiveExtras.SmallArray as SmallArray
import qualified PrimitiveExtras.SparseSmallArray as SparseSmallArray


new :: STM (Hamt a)
new = Hamt <$> newTVar SparseSmallArray.empty

newIO :: IO (Hamt a)
newIO = Hamt <$> newTVarIO SparseSmallArray.empty

pair :: Int -> Branch a -> Int -> Branch a -> STM (Hamt a)
pair hash1 branch1 hash2 branch2 =
  {-# SCC "pair" #-}
  let
    index1 = HashAccessors.index hash1
    index2 = HashAccessors.index hash2
    in if index1 == index2
      then pair (HashConstructors.succLevel hash1) branch1 (HashConstructors.succLevel hash2) branch2
      else Hamt <$> newTVar (SparseSmallArray.pair index1 branch1 index2 branch2)

focus :: (Eq key, Hashable key) => Focus element STM result -> (element -> key) -> key -> Hamt element -> STM result
focus focus elementToKey key = focusExplicitly focus (hash key) ((==) key . elementToKey)

focusExplicitly :: Focus a STM b -> Int -> (a -> Bool) -> Hamt a -> STM b
focusExplicitly focus hash test hamt =
  {-# SCC "focus" #-} 
  let
    Focus conceal reveal = Focuses.onHamtElement hash test focus
    in fmap fst (reveal hamt)

{-|
Returns a flag, specifying, whether the size has been affected.
-}
insert :: (Eq key, Hashable key) => (element -> key) -> element -> Hamt element -> STM Bool
insert elementToKey element = let
  !key = elementToKey element
  in insertExplicitly (hash key) ((==) key . elementToKey) element

{-|
Returns a flag, specifying, whether the size has been affected.
-}
insertExplicitly :: Int -> (a -> Bool) -> a -> Hamt a -> STM Bool
insertExplicitly hash testKey element (Hamt var) =
  {-# SCC "insertExplicitly" #-} 
  let
    !branchIndex = HashAccessors.index hash
    in do
      branchArray <- readTVar var
      case SparseSmallArray.lookup branchIndex branchArray of
        Nothing -> do
          writeTVar var $! SparseSmallArray.insert branchIndex (LeavesBranch hash (pure element)) branchArray
          return True
        Just branch -> case branch of
          LeavesBranch leavesHash leavesArray -> if leavesHash == hash
            then case SmallArray.findWithIndex testKey leavesArray of
              Just (leavesIndex, leavesElement) -> do
                let
                  !newLeavesArray = SmallArray.set leavesIndex element leavesArray
                  !newBranch = LeavesBranch hash newLeavesArray
                  !newBranchArray = SparseSmallArray.replace branchIndex newBranch branchArray
                  in do
                    writeTVar var newBranchArray
                    return False
              Nothing -> do
                writeTVar var $! SparseSmallArray.replace branchIndex (LeavesBranch hash (SmallArray.cons element leavesArray)) branchArray
                return True
            else let
              nextHash = HashConstructors.succLevel hash
              nextLeavesHash = HashConstructors.succLevel leavesHash
              in do
                hamt <- pair nextHash (LeavesBranch nextHash (pure element)) nextLeavesHash (LeavesBranch nextLeavesHash leavesArray)
                writeTVar var $! SparseSmallArray.replace branchIndex (BranchesBranch hamt) branchArray
                return True
          BranchesBranch hamt -> insertExplicitly (HashConstructors.succLevel hash) testKey element hamt

{-|
Returns a flag, specifying, whether the size has been affected.
-}
lookup :: (Eq key, Hashable key) => (element -> key) -> key -> Hamt element -> STM (Maybe element)
lookup elementToKey key = lookupExplicitly (hash key) ((==) key . elementToKey)

lookupExplicitly :: Int -> (a -> Bool) -> Hamt a -> STM (Maybe a)
lookupExplicitly hash test (Hamt var) =
  {-# SCC "lookupExplicitly" #-}
  let
    !index = HashAccessors.index hash
    in do
      branchArray <- readTVar var
      case SparseSmallArray.lookup index branchArray of
        Just branch -> case branch of
          LeavesBranch leavesHash leavesArray -> if leavesHash == hash
            then return (SmallArray.find test leavesArray)
            else return Nothing
          BranchesBranch hamt -> lookupExplicitly (HashConstructors.succLevel hash) test hamt
        Nothing -> return Nothing

reset :: Hamt a -> STM ()
reset (Hamt branchSsaVar) = writeTVar branchSsaVar SparseSmallArray.empty

unfoldM :: Hamt a -> UnfoldM STM a
unfoldM = UnfoldMs.hamtElements

null :: Hamt a -> STM Bool
null (Hamt branchSsaVar) = do
  branchSsa <- readTVar branchSsaVar
  return (SparseSmallArray.null branchSsa)
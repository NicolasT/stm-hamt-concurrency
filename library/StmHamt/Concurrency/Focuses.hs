{-|
Utility focuses.
-}
module StmHamt.Concurrency.Focuses where

import StmHamt.Concurrency.Prelude
import StmHamt.Concurrency.Types
import Focus hiding (onTVarValue)
import qualified StmHamt.Concurrency.IntOps as IntOps
import qualified StmHamt.Concurrency.Constructors.Branch as BranchConstructors
import qualified PrimitiveExtras.By6Bits as By6Bits
import qualified PrimitiveExtras.SmallArray as SmallArray


onBranchElement :: forall a b stm. MonadSTM stm => Int -> Int -> (a -> Bool) -> Focus a stm b -> Focus (Branch stm a) stm b
onBranchElement depth hash testElement elementFocus@(Focus concealElement revealElement) =
  let
    ~(Focus concealLeaves revealLeaves) = SmallArray.onFoundElementFocus testElement (const False) elementFocus
    branchesVarFocus :: Int -> Focus (TVar stm (By6Bits (Branch stm a))) stm b
    branchesVarFocus depth = let
      !branchIndex = IntOps.indexAtDepth depth hash
      in onTVarValue (By6Bits.onElementAtFocus branchIndex (branchFocus ( depth)))
    branchFocus :: Int -> Focus (Branch stm a) stm b
    branchFocus depth = Focus concealBranch revealBranch where
      concealBranch = fmap (fmap (fmap (LeavesBranch hash))) concealLeaves
      revealBranch = \ case
        LeavesBranch leavesHash leavesArray -> 
          case leavesHash == hash of
            True -> fmap (fmap (fmap (LeavesBranch leavesHash))) (revealLeaves leavesArray)
            False -> let
              interpretChange = \ case
                Set !newElement -> Set <$> BranchConstructors.pair (IntOps.nextDepth depth) hash (BranchConstructors.singleton hash newElement) leavesHash (LeavesBranch leavesHash leavesArray)
                _ -> return Leave
              in concealElement >>= traverse interpretChange
        BranchesBranch (Hamt var) -> let
          Focus _ revealBranchesVar = branchesVarFocus (IntOps.nextDepth depth)
          in fmap (fmap (fmap (BranchesBranch . Hamt))) (revealBranchesVar var)
    in branchFocus depth

onHamtElement :: MonadSTM stm => Int -> Int -> (a -> Bool) -> Focus a stm b -> Focus (Hamt stm a) stm b
onHamtElement depth hash test focus =
  let
    branchIndex = IntOps.indexAtDepth depth hash
    Focus concealBranches revealBranches =
      By6Bits.onElementAtFocus branchIndex $
      onBranchElement depth hash test focus
    concealHamt = let
      hamtChangeStm = \ case
        Leave -> return Leave
        Set !branches -> Set . Hamt <$> newTVar branches
        Remove -> Set . Hamt <$> newTVar By6Bits.empty
      in concealBranches >>= traverse hamtChangeStm
    revealHamt (Hamt branchesVar) = do
      branches <- readTVar branchesVar
      (result, branchesChange) <- revealBranches branches
      case branchesChange of
        Leave -> return (result, Leave)
        Set !newBranches -> writeTVar branchesVar newBranches $> (result, Leave)
        Remove -> writeTVar branchesVar By6Bits.empty $> (result, Leave)
    in Focus concealHamt revealHamt

{-|
Focus on the contents of a TVar.
-}
{-# INLINE onTVarValue #-}
-- Copied from focus-1.0.3, adapted to use MonadSTM
onTVarValue :: MonadSTM stm => Focus a stm b -> Focus (TVar stm a) stm b
onTVarValue (Focus concealA presentA) = Focus concealTVar presentTVar where
  concealTVar = concealA >>= traverse interpretAChange where
    interpretAChange = \ case
      Leave -> return Leave
      Set !a -> Set <$> newTVar a
      Remove -> return Leave
  presentTVar var = readTVar var >>= presentA >>= traverse interpretAChange where
    interpretAChange = \ case
      Leave -> return Leave
      Set !a -> writeTVar var a $> Leave
      Remove -> return Remove

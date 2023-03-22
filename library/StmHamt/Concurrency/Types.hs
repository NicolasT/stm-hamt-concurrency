{-# OPTIONS_GHC -funbox-strict-fields #-}
module StmHamt.Concurrency.Types where

import StmHamt.Concurrency.Prelude

{-|
STM-specialized Hash Array Mapped Trie,
extended with its size-tracking functionality,
allowing for a fast 'size' operation.
-}
data SizedHamt stm element = SizedHamt !(TVar stm Int) !(Hamt stm element)

{-|
STM-specialized Hash Array Mapped Trie.
-}
newtype Hamt stm element = Hamt (TVar stm (By6Bits (Branch stm element)))

data Branch stm element = BranchesBranch !(Hamt stm element) | LeavesBranch !Int !(SmallArray element)

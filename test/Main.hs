module Main where

import Test.TypeSpec
import Generic.Type.Function.FoldMap
import GHC.Generics ( type Rep )
import DeFun.Core ( type (~>), type App )

import GHC.TypeNats ( type Natural, type (+), type (*), type (^) )
import Data.Word ( type Word8, type Word16, type Word32, type Word64 )

type AddSym :: Natural ~> Natural ~> Natural
data AddSym f
type instance App AddSym f = AddSym1 f

type AddSym1 :: Natural -> Natural ~> Natural
data AddSym1 l r
type instance App (AddSym1 l) r = l + r

type FieldCountSym :: a ~> Natural
data FieldCountSym a
type instance App FieldCountSym a = 1

type MultSym :: Natural ~> Natural ~> Natural
data MultSym f
type instance App MultSym f = MultSym1 f

type MultSym1 :: Natural -> Natural ~> Natural
data MultSym1 l r
type instance App (MultSym1 l) r = l * r

-- | Defun symbol for total number of unique values a type can take.
--
-- TODO name...
type RangeSym :: a ~> Natural
data RangeSym a
type instance App RangeSym a = Range a

-- | Types which have a known amount of possible terms.
class HasRange a where type Range a :: Natural
instance HasRange Word8  where type Range Word8  = 2^8
instance HasRange Word16 where type Range Word16 = 2^16
instance HasRange Word32 where type Range Word32 = 2^32
instance HasRange Word64 where type Range Word64 = 2^64

-- true, but we can't use with current generic type foldMap because it has
-- multiple constructors
instance HasRange Bool where type Range Bool = 2

main :: IO ()
main = print spec

type AddFieldCount a = GTFoldMapC AddSym  0 FieldCountSym (Rep a)
type MultRange     a = GTFoldMapC MultSym 1 RangeSym      (Rep a)

spec :: Expect
    '[ AddFieldCount () `Is` 0
     , AddFieldCount (a, b, c) `Is` 3
     , MultRange     () `Is` 1
     , MultRange     (Word8, Word8, Word16) `Is` (2^32)
     ]
spec = Valid

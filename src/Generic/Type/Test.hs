-- {-# LANGUAGE UndecidableInstances #-}

module Generic.Type.Test where

import DeFun.Core ( type (~>), type App )
import GHC.Generics ( Generic )
import GHC.TypeNats ( type (+), type Natural )

type CBLenSym :: a ~> Natural
data CBLenSym a
type instance App CBLenSym a = 1

type PlusSym :: Natural ~> Natural ~> Natural
data PlusSym f
type instance App PlusSym f = PlusSym1 f

type PlusSym1 :: Natural -> Natural ~> Natural
data PlusSym1 l r
type instance App (PlusSym1 l) r = l + r

data X = X { x1 :: (), x2 :: () }
    deriving stock Generic

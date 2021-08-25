{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Value
    ( Value (..)

    , ValueType (..)
    , IdentifyV (..)

    , IsValue
    , v_cast
    ) where

import IR.DeclSpan
import IR.Typed

import Data.Typeable (Typeable, cast)

data ValueType = VTConstFunctionPointer
class IdentifyV d where
    identify_v :: d -> ValueType

class (Typeable v, Eq v, DeclSpan ctx v, Typed ctx tyr v, IdentifyV v) => IsValue ctx tyr v
data Value ctx tyr = forall v. IsValue ctx tyr v => Value v

instance Eq (Value ctx tyr) where
    Value v1 == Value v2 =
        case cast v2 of
            Just v2' -> v1 == v2'
            Nothing -> False

instance Typed ctx tyr (Value ctx tyr) where
    type_of (Value v) = type_of v

instance DeclSpan ctx (Value ctx tyr) where
    decl_span (Value v) = decl_span v

instance IdentifyV (Value ctx tyr) where
    identify_v (Value v) = identify_v v

v_cast :: IsValue ctx tyr v => Value ctx tyr -> Maybe v
v_cast (Value v) = cast v

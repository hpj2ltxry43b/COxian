{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.DeclSymbol
    ( DeclSymbol (..)

    , DeclSymbolType (..)
    , IdentifyDS (..)

    , IsDeclSymbol
    , ds_cast
    ) where

import IR.DeclSpan
import Data.Typeable (Typeable, cast)

data DeclSymbolType = DSTModule | DSTType
class IdentifyDS d where
    identify_ds :: d -> DeclSymbolType

class (Typeable d, Eq d, DeclSpan ctx d, IdentifyDS d) => IsDeclSymbol ctx d
data DeclSymbol ctx = forall d. IsDeclSymbol ctx d => DeclSymbol d

instance Eq (DeclSymbol ctx) where
    DeclSymbol d1 == DeclSymbol d2 =
        case cast d2 of
            Just d2' -> d1 == d2'
            Nothing -> False

instance DeclSpan ctx (DeclSymbol ctx) where
    decl_span (DeclSymbol d) = decl_span d

instance IdentifyDS (DeclSymbol ctx) where
    identify_ds (DeclSymbol d) = identify_ds d

ds_cast :: IsDeclSymbol ctx d => DeclSymbol ctx -> Maybe d
ds_cast (DeclSymbol d) = cast d

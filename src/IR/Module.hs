{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Module
    ( Module
    , new_module
    ) where

import StateAndLens

import Location

import IR.DeclSpan
import IR.DeclSymbol

import IR.IRCtx

import IR.ChildList

import IR.Type (builtin_types)

import qualified Control.Monad.State as State (State)

data Module = Module Span

new_module :: Span -> State.State IRCtx (DSIdx Module)
new_module sp =
    get_ds (Module sp) >>= \ mod_idx ->
    sequence
        (map
        (\ (name, ty) ->
            get_ds ty >>= \ ty_dsidx ->
            over_s ds_child_list (add_replace (upcast_dsidx mod_idx) name (upcast_dsidx ty_dsidx))
            )
        builtin_types) >>
    return mod_idx

instance DeclSpan IRCtx Module where
    decl_span (Module sp) = return $ Just sp

instance Eq Module where
    _ == _ = True

instance IsDeclSymbol IRCtx Module

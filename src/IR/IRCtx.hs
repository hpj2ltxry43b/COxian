{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module IR.IRCtx
    ( IRCtx

    , IsDeclSymbol'
    , DeclSymbol'
    , IsValue'
    , Value'

    , ds_interner
    , v_interner
    , ds_child_list
    , v_child_list

    , DSIdx
    , upcast_dsidx
    , downcast_dsidx
    , get_ds
    , resolve_dsidx

    , VIdx
    , upcast_vidx
    , downcast_vidx
    , get_v
    , resolve_vidx
    ) where

import SimpleLens

import Interner

import IR.ChildList

import IR.DeclSymbol
import IR.Value

import {-# SOURCE #-} IR.Type

import qualified Control.Monad.State as State (State, state)

type IsDeclSymbol' = IsDeclSymbol IRCtx
type DeclSymbol' = DeclSymbol IRCtx
type IsValue' = IsValue IRCtx (DSIdx Type)
type Value' = Value IRCtx (DSIdx Type)

data IRCtx
    = IRCtx
      { _ds_interner :: Interner DeclSymbol'
      , _v_interner :: Interner Value'
      , _ds_child_list :: ChildList (InternerIdx DeclSymbol') (InternerIdx DeclSymbol') String
      , _v_child_list :: ChildList (InternerIdx DeclSymbol') (InternerIdx Value') String
      }
ds_interner :: Lens IRCtx (Interner DeclSymbol')
ds_interner = Lens _ds_interner (\ a b -> a { _ds_interner = b })

v_interner :: Lens IRCtx (Interner Value')
v_interner = Lens _v_interner (\ a b -> a { _v_interner = b })

ds_child_list :: Lens IRCtx (ChildList (InternerIdx DeclSymbol') (InternerIdx DeclSymbol') String)
ds_child_list = Lens _ds_child_list (\ a b -> a { _ds_child_list = b })

v_child_list :: Lens IRCtx (ChildList (InternerIdx DeclSymbol') (InternerIdx Value') String)
v_child_list = Lens _v_child_list (\ a b -> a { _v_child_list = b })

newtype DSIdx d = DSIdx { upcast_dsidx :: InternerIdx DeclSymbol' } deriving (Eq, Ord)

downcast_dsidx :: IsDeclSymbol' d => InternerIdx DeclSymbol' -> State.State IRCtx (Maybe (DSIdx d))
downcast_dsidx idx = State.state $ \ irctx ->
    let ds = resolve_interner_idx idx (view ds_interner irctx)

        into_dsidx :: Maybe d -> Maybe (DSIdx d)
        into_dsidx d = DSIdx idx <$ d

    in (into_dsidx (ds_cast ds), irctx)

get_ds :: IsDeclSymbol' d => d -> State.State IRCtx (DSIdx d)
get_ds d = State.state $ \ irctx ->
    let (iidx, irctx') = modify ds_interner (get_from_interner (DeclSymbol d)) irctx
    in (DSIdx iidx, irctx')

resolve_dsidx :: IsDeclSymbol' d => DSIdx d -> State.State IRCtx d
resolve_dsidx (DSIdx iidx) = State.state $ \ irctx ->
    case ds_cast $ resolve_interner_idx iidx $ view ds_interner irctx of
        Just d -> (d, irctx)
        Nothing -> error "DSIdx does not have correct type"


newtype VIdx v = VIdx { upcast_vidx :: InternerIdx Value' } deriving (Eq, Ord)

downcast_vidx :: IsValue' v => InternerIdx Value' -> State.State IRCtx (Maybe (VIdx v))
downcast_vidx idx = State.state $ \ irctx ->
    let v = resolve_interner_idx idx (view v_interner irctx)
        
        into_vidx :: Maybe v -> Maybe (VIdx v)
        into_vidx v' = VIdx idx <$ v'

    in (into_vidx (v_cast v), irctx)

get_v :: IsValue' v => v -> State.State IRCtx (VIdx v)
get_v v = State.state $ \ irctx ->
    let (iidx, irctx') = modify v_interner (get_from_interner (Value v)) irctx
    in (VIdx iidx, irctx')

resolve_vidx :: IsValue' v => VIdx v -> State.State IRCtx v
resolve_vidx (VIdx iidx) = State.state $ \ irctx ->
    case v_cast $ resolve_interner_idx iidx $ view v_interner irctx of
        Just v -> (v, irctx)
        Nothing -> error "VIdx does not have correct type"

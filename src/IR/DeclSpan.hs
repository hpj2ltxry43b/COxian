{-# LANGUAGE MultiParamTypeClasses #-}

module IR.DeclSpan where

import Location

import qualified Control.Monad.State as State (State)

class DeclSpan ctx d where
    decl_span :: d -> State.State ctx (Maybe Span)

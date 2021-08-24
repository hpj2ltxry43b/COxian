{-# LANGUAGE MultiParamTypeClasses #-}

module IR.DeclSpan where

import Location

import qualified Control.Monad.Reader as Reader (Reader)

class DeclSpan ctx d where
    decl_span :: d -> Reader.Reader ctx (Maybe Span)

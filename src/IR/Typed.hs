{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Typed
    ( Typed(..)
    ) where

import qualified Control.Monad.State as State (State)

-- 'tyr' for 'type representation'
class Typed ctx tyr v where
    type_of :: v -> State.State ctx tyr

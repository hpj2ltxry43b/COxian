{-# LANGUAGE MultiParamTypeClasses #-}

module IR.Typed
    ( Typed(..)
    ) where

import qualified Control.Monad.Reader as Reader (Reader)

-- 'tyr' for 'type representation'
class Typed ctx tyr v where
    type_of :: v -> Reader.Reader ctx tyr

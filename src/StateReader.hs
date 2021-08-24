module StateReader
    ( to_state
    ) where

import Control.Monad.State (State, state)
import Control.Monad.Reader (Reader, runReader)

to_state :: Reader s a -> State s a
to_state r = state $ \ s -> (runReader r s, s)

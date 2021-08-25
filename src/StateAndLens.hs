module StateAndLens
    ( view_s
    , view_r
    , over_s
    , modify_s
    , modify_s'
    , read_r
    , read_r'
    ) where

import SimpleLens

import qualified Control.Monad.State as State (State, state, runState)
import qualified Control.Monad.Reader as Reader (Reader, reader, runReader)

view_s :: Lens a b -> State.State a b
view_s lens = State.state $ \ a -> (view lens a, a)

view_r :: Lens a b -> Reader.Reader a b
view_r = Reader.reader . view

over_s :: Lens a b -> (b -> b) -> State.State a ()
over_s lens f = State.state $ \ a -> ((), over lens f a)

modify_s :: Lens a b -> (b -> (c, b)) -> State.State a c
modify_s lens f = State.state $ modify lens f

modify_s' :: Lens a b -> State.State b c -> State.State a c
modify_s' lens s = State.state $ modify lens (State.runState s)

read_r :: Lens a b -> (b -> c) -> a -> c
read_r lens f = f . view lens

read_r' :: Lens a b -> Reader.Reader b c -> Reader.Reader a c
read_r' lens r = Reader.reader $ read_r lens (Reader.runReader r)

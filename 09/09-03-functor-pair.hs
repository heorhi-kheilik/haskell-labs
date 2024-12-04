data Pair a = Pair a a deriving (Show)

instance Functor Pair where
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap func (Pair first second) = Pair (func first) (func second)

data Labelled e a = Labelled e a deriving (Show)

instance Functor (Labelled e) where
    fmap :: (a -> b) -> Labelled e a -> Labelled e b
    fmap func (Labelled label value) = Labelled label (func value)

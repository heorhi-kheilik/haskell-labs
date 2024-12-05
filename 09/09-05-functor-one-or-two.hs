data OneOrTwo a = One a
                | Two a a
                deriving (Show)

instance Functor OneOrTwo where
    fmap :: (a -> b) -> OneOrTwo a -> OneOrTwo b
    fmap func (One v) = One $ func v
    fmap func (Two v1 v2) = Two (func v1) (func v2)

data Either r l = Left l
                | Right r
                deriving (Show)

instance Functor (Main.Either t) where
    fmap :: (a -> b) -> Main.Either t a -> Main.Either t b
    fmap func (Main.Left a) = Main.Left $ func a
    fmap _ (Main.Right v) = Main.Right v

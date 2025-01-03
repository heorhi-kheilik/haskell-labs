data MultiTree a = Leaf
                 | Node a [MultiTree a]
                 deriving (Show)

instance Functor MultiTree where
    fmap :: (a -> b) -> MultiTree a -> MultiTree b
    fmap _ Leaf = Leaf
    fmap func (Node value trees) = Node (func value) [fmap func tree | tree <- trees]

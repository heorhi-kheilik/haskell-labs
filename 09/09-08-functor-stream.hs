data Stream a = Cons a (Stream a) deriving (Show)

toList :: Stream a -> [a]
toList (Cons x stream) = x : toList stream

fromList :: [a] -> Stream a
fromList (x : xs) = Cons x (fromList xs)

instance Functor Stream where
    fmap :: (a -> b) -> Stream a -> Stream b
    fmap func (Cons value stream) = Cons (func value) (fmap func stream)

import Data.Function (fix)

class NumberSet t where
    contains :: t -> Integer -> Bool

    fromList :: [Integer] -> t
    toList :: t -> [Integer]

    union :: t -> t -> t
    intersection :: t -> t -> t
    difference :: t -> t -> t

instance NumberSet [Integer] where
    contains :: [Integer] -> Integer -> Bool
    contains [] _ = False
    contains (x : xs) num = if x == num then True else contains xs num

    fromList :: [Integer] -> [Integer]
    fromList = fix (\rec result list -> if null list then result else
                                            let (x : xs) = list
                                            in if not $ contains result x
                                               then rec xs (x : result)
                                               else rec xs result) []

    toList :: [Integer] -> [Integer]
    toList = id

    union :: [Integer] -> [Integer] -> [Integer]
    union first second = first ++ [ x | x <- second, not $ contains first x ]

    intersection :: [Integer] -> [Integer] -> [Integer]
    intersection first second = first ++ [ x | x <- second, contains first x ]

    difference :: [Integer] -> [Integer] -> [Integer]
    difference first second = let intersectionSet = intersection first second
                              in filter (contains intersectionSet) first

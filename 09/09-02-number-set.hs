class NumberSet t where
    contains :: t -> Integer -> Bool

instance NumberSet [Integer] where
    contains :: [Integer] -> Integer -> Bool
    contains [] _ = False
    contains (x : xs) num = if x == num then True else contains xs num

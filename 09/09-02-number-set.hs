import Data.List (sort)

class NumberSet t where
    contains :: t -> Int -> Bool

    fromList :: [Int] -> t
    toList :: t -> [Int]

    union :: t -> t -> t
    intersection :: t -> t -> t
    difference :: t -> t -> t

instance NumberSet [Int] where
    contains :: [Int] -> Int -> Bool
    contains [] _ = False
    contains (x : xs) num = if x == num then True else contains xs num

    fromList :: [Int] -> [Int]
    fromList = (flip _fromListIntH) []

    toList :: [Int] -> [Int]
    toList = id

    union :: [Int] -> [Int] -> [Int]
    union first second = first ++ [ x | x <- second, not $ contains first x ]

    intersection :: [Int] -> [Int] -> [Int]
    intersection first second = [ x | x <- second, contains first x, contains second x ]

    difference :: [Int] -> [Int] -> [Int]
    difference first second = let intersectionSet = intersection first second
                              in filter (not . contains intersectionSet) first

_fromListIntH :: [Int] -> [Int] -> [Int]
_fromListIntH [] result = result
_fromListIntH (x : xs) result = if not $ null $ filter (\el -> el == x) result
    then _fromListIntH xs result
    else _fromListIntH xs (x : result)

instance NumberSet [Bool] where
    contains :: [Bool] -> Int -> Bool
    contains list index = length list >= index && (list !! pred index)

    fromList :: [Int] -> [Bool]
    fromList [] = []
    fromList list =
        let (first : preparedList) = _uniqueReversedInSorted $ sort list
        in _fromListBoolH preparedList [True] first

    toList :: [Bool] -> [Int]
    toList list = map snd $ filter fst $ zip list [1..]

    union :: [Bool] -> [Bool] -> [Bool]
    union first second =
        if length first < length second
        then union second first
        else
            let (firstB, firstE) = splitAt (length second) first
            in (map (uncurry (||)) (zip firstB second)) ++ firstE

    intersection :: [Bool] -> [Bool] -> [Bool]
    intersection = ((.) $ (.) $ map $ uncurry (&&)) zip

    difference :: [Bool] -> [Bool] -> [Bool]
    difference first second =
        let (firstB, firstE) = splitAt (length second) first
        in map (\(a, b) -> a && not b) (zip firstB second) ++ firstE

_uniqueReversedInSortedH :: [Int] -> [Int] -> [Int]
_uniqueReversedInSortedH result []              = result
_uniqueReversedInSortedH result (x : [])        = (x : result)
_uniqueReversedInSortedH result (x1 : x2 : xs)  = _uniqueReversedInSortedH newResult (x2 : xs)
    where newResult = if x1 == x2 then result else x1 : result

_uniqueReversedInSorted = _uniqueReversedInSortedH []

_fromListBoolH :: [Int] -> [Bool] -> Int -> [Bool]
_fromListBoolH [] result count = (take (pred count) (repeat False)) ++ result
_fromListBoolH (x : xs) result count = _fromListBoolH xs ((True : take (count - x - 1) (repeat False)) ++ result) x

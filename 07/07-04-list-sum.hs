import Data.Function (fix)

sumOfList [] = 0
sumOfList (x : xs) = x + sumOfList xs

sumOfListLambda = \list -> case list of
    []      ->  0
    x : xs  ->  x + sumOfListLambda xs

helperSumOfList funcParam = \list -> case list of
    []      ->  0
    x : xs  ->  x + funcParam xs

fixSumOfList = fix helperSumOfList

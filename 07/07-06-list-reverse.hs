import Data.Function (fix)

_reverseListHelperFix func = \result currentList -> case currentList of
    []      ->  result
    x : xs  ->  func (x : result) xs

fixReverseList = fix _reverseListHelperFix []

fixReverseListL = fix (\rec ys xs -> if null xs then ys else let (z : zs) = xs in rec (z : ys) zs) []

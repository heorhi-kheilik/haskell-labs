import Data.Function (fix)

_reverseListHelperFix func = \result currentList -> case currentList of
    []      ->  result
    x : xs  ->  func (x : result) xs

fixReverseList = fix _reverseListHelperFix []

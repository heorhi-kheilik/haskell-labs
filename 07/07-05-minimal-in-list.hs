import Data.Function (fix)

_minInListHelperFix func = \currentMin list -> case list of
    []      ->  currentMin
    x : xs  ->  let nextMin = min currentMin x in func nextMin xs

fixMinInList (x : xs) = fix _minInListHelperFix x xs

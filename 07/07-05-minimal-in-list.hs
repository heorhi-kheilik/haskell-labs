import Data.Function (fix)

_minInListHelperFix func = \currentMin list -> case list of
    []      ->  currentMin
    x : xs  ->  let nextMin = min currentMin x in func nextMin xs

fixMinInList (x : xs) = fix _minInListHelperFix x xs

fixMinInListL (x : xs) = fix (\rec (x : xs) m -> if x < m then rec xs x else rec xs m) xs x

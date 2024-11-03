import Data.List (unfoldr)

unfoldToBinary n = reverse $ unfoldr _unfoldToBinary_h n

_unfoldToBinary_h 0 = Nothing
_unfoldToBinary_h n = let (d, r) = divMod n 2 in Just (r, d)

import Data.List;

unfoldToCollatz num = (num : (unfoldr _unfoldToCollatzH num))

_unfoldToCollatzH :: (Integral a) => a -> Maybe (a, a)
_unfoldToCollatzH num
    | num == 1          = Nothing
    | mod num 2 == 0    = let res = div num 2 in Just (res, res)
    | otherwise         = let res = num * 3 + 1 in Just (res, res)

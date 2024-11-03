import Data.List (unfoldr)

unfoldNatural n = unfoldr func n
    where func = \x -> if x == 0 then Nothing else Just (x, x - 1)

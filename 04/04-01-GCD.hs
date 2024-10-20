gcd_c a b
    | a > b     = gcd_c (a - b) b
    | a < b     = gcd_c a (b - a)
    | otherwise = a

pow_c x 0 = 1
pow_c x 1 = x
pow_c x k
    | mod k 2 == 0  = pow_c (x * x) (div k 2)
    | otherwise     = x * pow_c (x * x) (div k 2)

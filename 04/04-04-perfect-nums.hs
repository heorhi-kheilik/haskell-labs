isPerfect :: Integer -> Bool
isPerfect n = (n - 1) == (sum $ divisors n)

divisors :: Integral a => a -> [a]
divisors n = concat [ case divMod n cur of
    (d, 0) -> [cur, d]
    (_, _) -> []
        | cur <- [2 .. floor $ sqrt $ fromIntegral n] ]

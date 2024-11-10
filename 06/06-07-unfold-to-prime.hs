import Data.List;

unfoldToPrimeWithSieve limit = unfoldr _unfoldToPrimeWithSieveH [2..limit]

_unfoldToPrimeWithSieveH [] = Nothing
_unfoldToPrimeWithSieveH (current : sieveSuffix) =
    let newSieveSuffix = filter (\x -> mod x current /= 0) sieveSuffix in Just (current, newSieveSuffix)

unfoldToPrime = (2 : unfoldr _unfoldToPrimeH ([2], 3))

_unfoldToPrimeH (primes, current) = let new = _newPrimeElement primes current in Just (new, (primes ++ [new], new + 1))

_newPrimeElement primes current = if null $ filter id [ mod current prime == 0 | prime <- primes ]
    then current
    else _newPrimeElement primes (current + 1)

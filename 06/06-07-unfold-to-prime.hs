import Data.List;

unfoldToPrime = (2 : unfoldr _unfoldToPrimeH ([2], 3))

_unfoldToPrimeH (sieve, current) = let new = _newSieveElement sieve current in Just (new, (sieve ++ [new], new + 1))

_newSieveElement sieve current = if null $ filter id [ mod current sElement == 0 | sElement <- sieve ]
    then current
    else _newSieveElement sieve (current + 1)

unfoldToPrimeNotAbove n = takeWhile (\x -> x <= n) unfoldToPrime

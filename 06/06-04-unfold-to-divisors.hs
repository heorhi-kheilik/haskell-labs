import Data.List;

unfoldToDivisors num = unfoldr _unfoldToDivisorsH (num, primes)
    where primes = unfoldToPrimeWithSieve num

_unfoldToDivisorsH (1, _) = Nothing
_unfoldToDivisorsH (num, primes) = let first = head $ dropWhile (\x -> mod num x /= 0) primes
    in Just (first, (div num first, primes))

unfoldToPrimeWithSieve limit = unfoldr _unfoldToPrimeWithSieveH [2..limit]

_unfoldToPrimeWithSieveH [] = Nothing
_unfoldToPrimeWithSieveH (current : sieveSuffix) =
    let newSieveSuffix = filter (\x -> mod x current /= 0) sieveSuffix in Just (current, newSieveSuffix)

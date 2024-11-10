import Data.List;

unfoldToFibonacci = unfoldr _unfoldToFibonacciH (0, 1)

_unfoldToFibonacciH (first, second) = Just (first, (second, first + second))

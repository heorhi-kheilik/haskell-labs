import Data.Function (fix)

-- p: разпознаватель финального состояния   (resultPredicate)
-- q: селектор результата                   (resultSelector)
-- v: преобразователь состояния             (stateTransformer)
-- u: преобразователь результата            (resultTransformer)
--
-- g x = if p x then q x else u $ g $ v x
-- g = \x -> if p x then q x else u $ g $ v x
-- h f = \x -> if p x then q x else u $ f $ v x

fibonacciList (first, second) = first : fibonacciList (second, first + second)

fibonacciListLambda = \(first, second) -> first : fibonacciListLambda (second, first + second)

helperFibonacciList fibListParam = \(first, second) -> first : fibListParam (second, first + second)

fixFibonacciList = fix helperFibonacciList (0, 1)

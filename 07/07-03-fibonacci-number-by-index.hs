import Data.Function (fix)

fibonacciByIndex (index, (first, second)) =
    if index == 1
        then first
        else fibonacciByIndex (index - 1, (second, first + second))

fibonacciByIndexLambda = \(index, (first, second)) ->
    if index == 1
        then first
        else fibonacciByIndexLambda (index - 1, (second, first + second))

helperFibonacciByIndex funcParam = \(index, (first, second)) ->
    if index == 1
        then first
        else funcParam (index - 1, (second, first + second))

fixFibonacciByIndex index = fix helperFibonacciByIndex (index, (0, 1))

fibonacciByIndexC (first, second) index = 
    if index == 1
        then first
        else fibonacciByIndexC (second, first + second) (index - 1)

fibonacciByIndexLambdaC = \(first, second) index ->
    if index == 1
        then first
        else fibonacciByIndexLambdaC (second, first + second) (index - 1)

helperFibonacciByIndexC funcParam = \(first, second) index ->
    if index == 1
        then first
        else funcParam (second, first + second) (index - 1)

fixFibonacciByIndexC = fix helperFibonacciByIndexC (0, 1)

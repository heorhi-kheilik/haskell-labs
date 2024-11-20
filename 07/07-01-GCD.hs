import Data.Function (fix)

-- p: разпознаватель финального состояния   (resultPredicate)
-- q: селектор результата                   (resultSelector)
-- v: преобразователь состояния             (stateTransformer)
-- u: преобразователь результата            (resultTransformer)
--
-- g x = if p x then q x else u $ g $ v x
-- g = \x -> if p x then q x else u $ g $ v x
-- h f = \x -> if p x then q x else u $ f $ v x

resultPredicate (a, b) = a == b

resultSelector = fst

stateTransformer pair =
    let
        minimal = (uncurry min) pair
        maximal = (uncurry max) pair
    in
        (minimal, maximal - minimal)

resultTransformer = id

gcdCommon pair =
    if resultPredicate pair
        then resultSelector pair
        else resultTransformer $ gcdCommon $ stateTransformer pair

gcdLambda = \pair ->
    if resultPredicate pair
        then resultSelector pair
        else resultTransformer $ gcdLambda $ stateTransformer pair

helperGCDFix gcdFixParam = \pair ->
    if resultPredicate pair
        then resultSelector pair
        else resultTransformer $ gcdFixParam $ stateTransformer pair

helperGCDFixSubstituted gcdParam = \pair@(a, b) ->
    if a == b
        then a
        else let newPair = if a < b then (a, b - a) else (b, a - b) in gcdParam newPair

gcdFix = fix helperGCDFix
gcdFixSubstituted = fix helperGCDFixSubstituted

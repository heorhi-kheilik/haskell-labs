import Data.Map (Map, (!), fromList)

data UnaryOperation = Negate
                    | Sqrt
                    | Sin
                    | Cos
                    | Tan
                    deriving (Eq)

instance Show UnaryOperation where
    show Negate = "-$0"
    show Sqrt = "sqrt($0)"
    show Sin = "sin($0)"
    show Cos = "cos($0)"

applyUnary :: UnaryOperation -> Double -> Double
applyUnary op num = case op of
    Negate  -> -num
    Sqrt    -> sqrt num
    Sin     -> sin num
    Cos     -> cos num
    Tan     -> tan num


data BinaryOperation = Add
                     | Subtract
                     | Multiply
                     | Divide
                     | Min
                     | Max
                     deriving (Eq)

instance Show BinaryOperation where
    show Add = "$0 + $1"
    show Subtract = "$0 - $1"
    show Multiply = "$0 * $1"
    show Divide = "$0 / $1"
    show Min = "min($0, $1)"
    show Max = "max($0, $1)"

applyBinary :: BinaryOperation -> Double -> Double -> Double
applyBinary op num1 num2 = case op of
    Add         -> num1 + num2
    Subtract    -> num1 - num2
    Multiply    -> num1 * num2
    Divide      -> num1 / num2
    Min         -> min num1 num2
    Max         -> max num1 num2

_isCommutative op = case op of
    Add         -> True
    Subtract    -> False
    Multiply    -> True
    Divide      -> False
    Min         -> True
    Max         -> True

data AlgebraicTree = ATVertexU UnaryOperation AlgebraicTree
                   | ATVertexB BinaryOperation AlgebraicTree AlgebraicTree
                   | ATLeafC Double
                   | ATLeafV String

instance Eq AlgebraicTree where
    (ATLeafC num1) == (ATLeafC num2) = num1 == num2
    (ATLeafV var1) == (ATLeafV var2) = var1 == var2
    (ATVertexU op1 tree1) == (ATVertexU op2 tree2) = op1 == op2 && tree1 == tree2
    (ATVertexB op1 tree11 tree12) == (ATVertexB op2 tree21 tree22)
        | op1 /= op2                = False
        | not $ _isCommutative op1  = tree11 == tree21 && tree12 == tree22
        | otherwise                 = (tree11 == tree21 && tree12 == tree22) || (tree11 == tree22 && tree12 == tree21)

instance Show AlgebraicTree where
    show tree = case tree of
        ATLeafC const               -> show const
        ATLeafV var                 -> var
        ATVertexU op tree           -> _format (show op) [tree]
        ATVertexB op tree1 tree2    -> "(" ++ _format (show op) [tree1, tree2] ++ ")"

_format :: Show a => String -> [a] -> String
_format template args = let (first, second) = break (\x -> x == '$') template in
    if null second
        then template
        else
            let
                argStr = show $ args !! argIndex
                (_ : argIndexStr : secondSuffix) = second
                argIndex :: Int = read [argIndexStr]
                formatted = first ++ argStr ++ secondSuffix
            in
                _format formatted args

solve :: AlgebraicTree -> Double
solve t = case t of
    ATLeafC const               -> const
    ATLeafV _                   -> error "Can't solve expression with unresolved variables"
    ATVertexU op tree           -> applyUnary op (solve tree)
    ATVertexB op tree1 tree2    -> applyBinary op (solve tree1) (solve tree2)

resolveWith :: (Map String Double) -> AlgebraicTree -> AlgebraicTree
resolveWith map tree = case tree of
    const@(ATLeafC _)           -> const
    ATLeafV var                 -> ATLeafC $ map ! var
    ATVertexU op tree           -> ATVertexU op $ resolveWith map tree
    ATVertexB op tree1 tree2    -> ATVertexB op (resolveWith map tree1) (resolveWith map tree2)

simplify :: AlgebraicTree -> AlgebraicTree
simplify tree@(ATLeafC _) = tree
simplify tree@(ATLeafV _) = tree

simplify (ATVertexU operation subtree) =
    let subtree' = simplify subtree
    in case (operation, subtree') of
        (Negate, (ATVertexU Negate subtree))    -> subtree
        (op, ATLeafC num)                       -> ATLeafC (applyUnary op num)
        _                                       -> ATVertexU operation subtree'

simplify (ATVertexB operation subtree1 subtree2) =
    let subtree1' = simplify subtree1
        subtree2' = simplify subtree2
    in case (operation, subtree1', subtree2') of
        ( Add,
          ATVertexB Multiply (ATVertexU Sin (ATLeafV var1)) (ATVertexU Sin (ATLeafV var2)),
          ATVertexB Multiply (ATVertexU Cos (ATLeafV var3)) (ATVertexU Cos (ATLeafV var4))
          ) ->
            if var1 == var2 && var2 == var3 && var3 == var4
            then ATLeafC 1
            else ATVertexB operation subtree1' subtree2'
        (Add, subtree, (ATLeafC 0)) -> subtree
        (Add, (ATLeafC 0), subtree) -> subtree
        (Subtract, subtree, (ATLeafC 0)) -> subtree
        (Subtract, (ATLeafC 0), subtree) -> ATVertexU Negate subtree
        (Subtract, first, second) ->
            if first == second
            then ATLeafC 0
            else ATVertexB operation subtree1' subtree2'
        (Multiply, _, (ATLeafC 0)) -> ATLeafC 0
        (Multiply, (ATLeafC 0), _) -> ATLeafC 0
        (Multiply, subtree, (ATLeafC 1)) -> subtree
        (Multiply, (ATLeafC 1), subtree) -> subtree
        (Divide, subtree, (ATLeafC 1)) -> subtree
        (Min, subtree1, subtree2) ->
            if subtree1 == subtree2
            then subtree1
            else ATVertexB operation subtree1' subtree2'
        (Max, subtree1, subtree2) ->
            if subtree1 == subtree2
            then subtree1
            else ATVertexB operation subtree1' subtree2'
        (operation, ATLeafC num1, ATLeafC num2) -> ATLeafC (applyBinary operation num1 num2)
        _ -> ATVertexB operation subtree1' subtree2'

-- ATVertexB Add (ATVertexB Multiply (ATVertexU Sin (ATLeafV "a")) (ATVertexU Sin (ATLeafV "a"))) (ATVertexB Multiply (ATVertexU Cos (ATLeafV "a")) (ATVertexU Cos (ATLeafV "a")))

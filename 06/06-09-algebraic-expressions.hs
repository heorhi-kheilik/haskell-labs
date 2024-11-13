import Data.Map (Map, (!), fromList)

data UnaryOperation = Negate | Sqrt | Sin | Cos | Tan

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


data BinaryOperation = Add | Subtract | Multiply | Divide | Min | Max

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


data AlgebraicTree
    = ATVertexU UnaryOperation AlgebraicTree
    | ATVertexB BinaryOperation AlgebraicTree AlgebraicTree
    | ATLeafC Double
    | ATLeafV String

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
resolveWith map (ATLeafV var) = ATLeafC $ map ! var
resolveWith _ const@(ATLeafC _) = const
resolveWith map (ATVertexU op tree) = ATVertexU op (resolveWith map tree)
resolveWith map (ATVertexB op tree1 tree2) = ATVertexB op (resolveWith map tree1) (resolveWith map tree2)

-- simplify :: AlgebraicTree -> AlgebraicTree
-- simplify tree = case tree of
--     ATLeafC _                                           -> tree
--     ATLeafV _                                           -> tree
--     ATVertexU _ (ATLeafC _)                             -> ATLeafC (solve tree)
--     ATVertexB _ (ATLeafC _) (ATLeafC _)                 -> ATLeafC (solve tree)
--     ATVertexU Negate (ATVertexU Negate (ATLeafV var))   -> ATLeafV var
--     ATVertexB Add opSin opCos
--     ATVertexB Add (
--         ATVertexB Multiply
--             (ATVertexU Sin (ATLeafV var1))
--             (ATVertexU Sin (ATLeafV var2))
--     ) (
--         ATVertexB Multiply
--             (ATVertexU Cos (ATLeafV var3))
--             (ATVertexU Cos (ATLeafV var4))
--     )                                                   -> ATLeafC 1

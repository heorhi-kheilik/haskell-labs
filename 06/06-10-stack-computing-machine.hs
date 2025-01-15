import Control.Applicative
import Control.Monad
import Data.Char

---------- Parser Type ----------

-- `Parser` is a type that has only defined `runParser` function.
newtype Parser a = Parser { runParser :: String -> [(a, String)] }

-- `Parser` is a Functor instance. Function can map it to another `Parser`.
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap function parser = Parser h where
        h s = [(function y, s') | (y, s') <- runParser parser s]

-- `Parser` is an Applicative instance. It has the smallest container
-- for value (pure) and can be combined with another `Parser`.
instance Applicative Parser where

    pure :: a -> Parser a
    pure x = Parser (\s -> [(x, s)])

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser firstParserFunc <*> Parser secondParserFunc = Parser combinedFunc where
        combinedFunc s = [(f y, s'') | (f, s') <- firstParserFunc s, (y, s'') <- secondParserFunc s']

instance Alternative Parser where

    empty :: Parser a
    empty = Parser (const [])

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser firstParserFunc <|> Parser secondParserFunc = Parser combinedFunc where
        combinedFunc s = firstParserFunc s ++ secondParserFunc s

---------- Parsers ----------

digit = Parser p where
    p (x : xs)  | isDigit x = [([x], xs)]
    p _ = []

whitespace = Parser p where
    p (x : xs)  | isSpace x = [([x], xs)]
    p _ = []

semicolon = Parser p where
    p (x : xs)  | x == ';' = [([x], xs)]
    p _ = []

number = join <$> some digit



string pattern = Parser p where
    p input
        | length input < len    = []
        | prefix == pattern     = [(prefix, suffix)]
        | otherwise             = []
        where
            len = length pattern
            (prefix, suffix) = splitAt len input

pushKeyword = Parser p where
    p (x1 : x2 : x3 : x4 : xs)  | [x1, x2, x3, x4] == "push" = [("push", xs)]
    p _ = []

pushDecomposingParser =
    (:) <$> (join <$> many whitespace) <*> (
        (:) <$> string "push" <*> (
            (:) <$> (join <$> some whitespace) <*> (
                (:) <$> number <*> (
                    (:) <$> (join <$> many whitespace) <*> (
                        (:) <$> semicolon <*> pure [])))))

pushParser = func <$> pushDecomposingParser where
    func xss = Push (read $ head $ filter (all isDigit) $ filter (not . null) xss)

commandDecomposingParser command =
    (:) <$> (join <$> many whitespace) <*> (
        (:) <$> (string command) <*> (
            (:) <$> (join <$> many whitespace) <*> (
                (:) <$> semicolon <*> pure [])))

---------- Stack Machine ----------

data Command
    = Push Double
    | Pop
    | Add
    | Sub
    | Mul
    | Div
    deriving (Show)

data SMError
    = MissingOperandError
    | CalculationError
    deriving (Show)

parserFor command = case command of
    Push _  -> pushParser
    Pop     -> Pop <$ commandDecomposingParser "pop"
    Add     -> Add <$ commandDecomposingParser "add"
    Sub     -> Sub <$ commandDecomposingParser "sub"
    Mul     -> Mul <$ commandDecomposingParser "mul"
    Div     -> Div <$ commandDecomposingParser "div"

-- First retrieved argument from stack is last argument in function.
-- Function returns results in a way that first argument must be put on stack first.
actionFor :: Command -> (Int, ([Double] -> (Either SMError [Double])))
actionFor command = case command of
    Push arg    -> (0, func) where func _            = Right [arg]

    Pop         -> (1, func) where func (a : [])     = Right []
                                   func _            = Left MissingOperandError

    Add         -> (2, func) where func (b : a : []) = Right [a + b]
                                   func _            = Left MissingOperandError

    Sub         -> (2, func) where func (b : a : []) = Right [a - b]
                                   func _            = Left MissingOperandError

    Mul         -> (2, func) where func (b : a : []) = Right [a * b]
                                   func _            = Left MissingOperandError

    Div         -> (2, func) where func (0 : a : []) = Left CalculationError
                                   func (b : a : []) = Right [a / b]
                                   func _            = Left MissingOperandError

completeParser = parserFor (Push 0)
             <|> parserFor Pop
             <|> parserFor Add
             <|> parserFor Sub
             <|> parserFor Mul
             <|> parserFor Div

data StackMachine = StackMachine { state :: Either SMError [Double]
                                 , commands :: [Command]
                                 }
                                 deriving (Show)

parseProgram :: String -> [Command]
parseProgram = fst . head . runParser (many completeParser)

stackMachineWithTextProgram :: [Double] -> String -> StackMachine
stackMachineWithTextProgram stack program = StackMachine { state = Right stack
                                                         , commands = parseProgram program
                                                         }

runCommand :: Command -> [Double] -> (Either SMError [Double])
runCommand command stack =
    let (argc, func) = actionFor command
        (argv, stack') = splitAt argc stack
    in do
        calculated <- func argv
        return $ foldl (flip (:)) stack' calculated

-- Produces new stack after applying function.
run StackMachine { state = state, commands = commands } =
    foldl (>>=) state (fmap (($) runCommand) commands)

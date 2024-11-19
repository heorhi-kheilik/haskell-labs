class Solvable a where
    solve :: a -> Double

data Node = Num 
          | Var String
          | 

data UnaryOperation a = Negate a
                      | Sqrt a
                      | Sin a
                      | Cos a
                      | Tan a

data BinaryOperation = Add
                     | Subtract
                     | Multiply
                     | Divide
                     | Min
                     | Max


module Expr where
{-
  Expr contains both expression types and value types in the language.
-}

-- define the operator types
data BoolOp = And | Or deriving (Show, Eq)
data MathOp = Add | Sub | Mul | Div | Mod deriving (Show, Eq)
data CompOp = Eq | Lt | Gt | Lte | Gte deriving (Show, Eq)

-- define the expression types
data Expr = 
      BoolExpr BoolOp [Expr]
    | NotExpr Expr   
    | MathExpr MathOp [Expr] 
    | CompExpr CompOp Expr Expr
    | VarExpr String 
    | LiteralExpr Value 
    | LambdaExpr String Expr 
    | IfExpr Expr Expr Expr
    | ApplyExpr Expr Expr
    | LetExpr String Expr Expr
    | PairExpr Expr Expr 
    | NegateExpr Expr
    | EmptyExpr 
    deriving (Show, Eq)


-- define the type for values, which in our mini language
-- can be integers, bools, pairs, or closures
data Value =
      IntVal Integer
    | BoolVal Bool
    | PairVal (Value, Value)
    | ClosureVal String String Expr ValueEnv deriving (Show, Eq)

-- define a value environment, which is a mapping from strings to values
type ValueEnv = [(String, Value)]

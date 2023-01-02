module Error where

-- we have two kinds of errors, a type error, meaning that when we evaluated, 
-- we ended up with the wrong types--these are types that the programmer made, not
-- types that we can safely ignore because we can try different things. EvalErrors can
-- usually be ignored, but have a message associated with them so we know why, and
-- NoEval means there wasn't an eval at all
data ErrorT = 
      TypeError String 
    | EvalError String 
    | SyntaxError String
    | ParseError String
    | EvalNotImplemented -- this evaluation hasn't been implemented
    | NoParse -- indicates can't parse, as in there wasn't a match, for example
    | NoEval -- indicates can't eval any further, as in maybe there wasn't a match
    | NoSymbol String deriving (Show, Eq)

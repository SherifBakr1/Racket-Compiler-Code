{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module MiniRacketParser where

import Parser
import Expr
import Control.Applicative
import Error ( ErrorT )

parseBool :: Parser Bool
parseBool = do
        parseKeyword "true"
        return True
        <|> do
            parseKeyword "false"
            return False

-- implement parsing bool operations, these are 'and' and 'or'
parseBoolOp :: Parser BoolOp
parseBoolOp = do
  do parseKeyword "and" >> return And
  <|> do parseKeyword "or" >> return Or

-- parse math operations and return the MathOp
-- TODO: Add the other math operations: *, div, mod
parseMathOp :: Parser MathOp
parseMathOp =
    do symbol "+" >> return Add
    <|> do symbol "-" >> return Sub
    <|> do symbol "*" >> return Mul
    <|> do string "div" >> return Div
    <|> do string "mod" >> return Mod

-- parse the comp operations and return the CompOp
-- TODO: add the comparison operators: equals?, <
parseCompOp :: Parser CompOp
parseCompOp = do
  symbol "=" >> return Eq
  <|> do symbol "<" >> return Lt
  <|> do symbol ">" >> return Gt
  <|> do symbol ">=" >> return Gte
  <|> do symbol "<=" >> return Lte

-- a literal in MiniRacket is true, false, or a number
-- TODO: parse literals which can be natural numbers or bools (true, false)
literal :: Parser Value
literal = do
  string "true" >> (return $ BoolVal True)
  <|> do string "false" >> (return $ BoolVal False)
  <|> do IntVal <$> int

 
-- parse a literal expression, which at this point, is just a literal
literalExpr :: Parser Expr
literalExpr = do
    LiteralExpr <$> literal


keywordList :: [String]
keywordList = ["false", "true", "not", "and", "or", "equal?", "if", "negate", "let", "lambda"]

-- try to parse a keyword, otherwise it's a variable, this can be
-- used to check if the identifier we see (i.e., variable name) is
-- actually a keyword, which isn't legal
parseKeyword :: String -> Parser String
parseKeyword keyword = do
    -- all keywords follow the identifier rules, so we'll use that
    name <- identifier
    if name `elem` keywordList && keyword == name
    then return name
    else failParse $ "saw " ++ name ++ ", expected " ++ keyword


-- TODO: parse not expressions, note that "not" is a keyword, so
-- as a big hint, you should use parseKeyword
notExpr :: Parser Expr
notExpr =  do
  parseKeyword "not"
  exp <- parseExpr
  return $ NotExpr exp

-- a bool expression is the operator followed by one or more expressions that we have to parse
-- TODO: add bool expressions
boolExpr :: Parser Expr
boolExpr = do
  f <- parseBoolOp
  x <- kplus parseExpr
  return $ BoolExpr f x


-- a math expression is the operator followed by one or more expressions that we have to parse
-- TODO: add math expressions
mathExpr :: Parser Expr
mathExpr = do
  op <- parseMathOp
  exprs <- kplus parseExpr
  return $ MathExpr op exprs

-- a comp expression is the comp operator and the parsing of two expressions
compExpr :: Parser Expr
compExpr = CompExpr <$> parseCompOp <*> parseExpr <*> (string " " >> parseExpr)


pairExpr :: Parser Expr
pairExpr = do
    expr1 <- parseExpr
    symbol "."
    PairExpr expr1 <$> parseExpr

-- note that this is syntactic sugar, cons is just replaced by the PairExpr ast
consExpr :: Parser Expr
consExpr = do
    symbol "cons"
    expr1 <- parseExpr
    PairExpr expr1 <$> parseExpr



parseParens :: Parser Expr -> Parser Expr
parseParens p = do
    symbol "("
    e <- p
    symbol ")"
    return e

-- an atom is a literalExpr, which can be an actual literal or some other things
-- parseAtom :: Parser Expr
-- parseAtom = do
--     literalExpr

-- the main parsing function which alternates between all the options you have
-- parseExpr :: Parser Expr
-- parseExpr = do
--      parseAtom
--      <|> parseParens boolExpr
--     <|> parseParens compExpr
--     <|> parseParens notExpr
--     <|> parseParens mathExpr
--     <|> parseParens pairExpr
--     <|> parseParens consExpr
--     <|> (LiteralExpr . BoolVal <$> parseBool)
--     <|> parseParens parseExpr

-- a helper function that you can use to test your parsing:
-- syntax is simply 'parseStr "5"' which will call parseExpr for you
parseStr :: String -> Either ErrorT (Expr, String)
parseStr str = do
    parse parseExpr str



-- Part 2 starts below 



-- TODO: add the additional kinds of things that can be an atom:
-- parsing an atom can be either a var, a literal, or a 
-- negated atom
parseAtom :: Parser Expr
parseAtom = do
    literalExpr 
    <|> varExpr
    <|> negateAtom
-- TODO: Implement negateAtom
-- negate an atom, we actually only have one choice here. Our
-- parsing already correctly builds negative numbers, and we
-- can't have negative boolean values (so we won't bother parsing)
-- those. That leaves variables, but this needs to build a 
-- NegateExpr around the VarExpr.
negateAtom :: Parser Expr
negateAtom = do 
    expr <- varExpr
    return (NegateExpr (expr))

-- TODO: Implement varExpr
-- parse a var expression, here we need to make sure that
-- the identifier is *not* a keyword before accepting it
-- i.e., we fail the parse if it is     
varExpr :: Parser Expr
varExpr = do 
    name <- identifier
    if name `notElem` keywordList 
        then return (VarExpr name)
        else failParse (name ++ "is a predefined keyword, cannot be used as a var")
-- TODO: Implement ifExpr
-- parse an if-expression, which begins with the keyword if,
-- and is followed by three expressions
ifExpr :: Parser Expr
ifExpr = do 
    parseKeyword "if"
    e1 <- parseExpr
    e2 <- parseExpr
    e3 <- parseExpr
    return (IfExpr e1 e2 e3)

-- TODO: Implement let expressions  
-- a let expression begins with the keyword let, followed by
-- parenthesis which contains an identifier for the name 
-- to be bound, an expression to bind to that name, a close
-- parenthesis, and a body  
letExpr :: Parser Expr
letExpr = do 
    parseKeyword "let"
    symbol "("
    name <- identifier
    expr <- parseExpr
    symbol ")"
    body <- parseExpr
    return (LetExpr name expr body)

-- TODO: Implement lambdaExpr 
-- parse a lambda expression which is a lambda, argument, 
-- and body, with proper parenthesis around it
lambdaExpr :: Parser Expr
lambdaExpr = do 
    parseKeyword "lambda"
    symbol "("
    name <- identifier
    symbol ")"
    expr <- parseExpr
    return (LambdaExpr name expr)
        

--TODO: Implement applyExpr
-- what we do know is that the left argument will result in a function,
-- otherwise we'll have an error, but nesting them like this allows us
-- to further build up functions
applyExpr :: Parser Expr
applyExpr = do 
    left <- parseExpr
    right <- parseExpr
    return (ApplyExpr left right)
    

--TODO: Add 
-- the main parsing function which alternates between all 
-- the options you have for possible expressions
parseExpr :: Parser Expr
parseExpr = do
    parseAtom
    <|> parseParens notExpr
    <|> parseParens boolExpr
    <|> parseParens mathExpr
    <|> parseParens ifExpr
    <|> parseParens applyExpr
    <|> parseParens letExpr
    <|> parseParens lambdaExpr
    <|> (LiteralExpr . BoolVal <$> parseBool)
    <|> parseParens parseExpr
    <|> parseParens compExpr
    <|> parseParens pairExpr
    <|> parseParens consExpr 

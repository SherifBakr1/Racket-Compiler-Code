module Parser where

import Control.Applicative

import Data.Char

import Error

-- a parser is a type that contains a function that goes from a String
-- to the pair of a result of the parse and the rest of the string. You
-- can think of this as a State monad really, here String is the state,
-- a is the return type. To make it useful for parsing though, we'll
-- return an Either String (a, String), so that we can have error messages.
--
-- we call the field 'parse' because it will be a function that accesses
-- the function in this type and so we simplify having to constantly
-- decompose P, we'll extract p and use it on any input string.
-- parse someParser "hello" will deconstruct someParser to extract the
-- parsing function out of it, and then apply it to the next parameter,
-- "hello" in this example
newtype Parser a = P { parse :: String -> Either ErrorT (a, String) }

-- the whole purpose of the 'item' Parser is to create a parser that
-- always reads one value, and removes it from the front of the input
-- so that we end up with the rest of the input. From here, we can
-- construct all other Parsers!
item :: Parser Char
item = P (\inp -> case inp of
            [] -> Left NoParse
            (x:xs) -> Right (x,xs))

-- ghci> parse item ""
-- Left "no more input"
-- ghci> parse item "abc"
-- ('a', "bc")

-- defines fmap over the functor, this maps the function f onto the
-- results of the parse. We assume there might be more than one answer,
-- and if so, we map over each possible answer (which is a pair)
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                            -- note that we can't just do err -> err because we could be going from a
                            -- Parser a -> Parser b, so technically Left :: Either String [(a, String)]
                            -- while Left :: Either String [(b, String)]
                            Left msg -> Left msg
                            Right (v, out) -> Right (f v, out))



-- now define the Parser as an applicative
instance Applicative Parser where
    -- pure :: a -> Parser a
    -- this returns the parser which simply returns the value (always) without consuming any input
    pure v = P (\inp -> Right (v, inp))

    pfuns <*> px = P (\inp -> case parse pfuns inp of
                        Left msg -> Left msg
                        Right (g, out) -> parse (fmap g px) out)

-- instance Alternative (Either String) where
--     empty = Left "empty"
--     Left msg <|> q = q
--     p <|> _ = p


-- let's construct some parsers using applicatives--this one will
-- parse the first 3 characters but return only the first and 3rd elements
three :: Parser (Char, Char)
three = g <$> item <*> item <*> item
    where g x _ z = (x, z)

 -- parse three "abcdefg"

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                    Left msg -> Left msg
                    Right (v, out) -> parse (f v) out)


threeM :: Parser (Char, Char)
threeM = do
    x <- item
    _ <- item
    z <- item
    return (x, z)

 --parse threeM "abcdefg"

errorParse :: ErrorT -> Parser a
errorParse t = P (\_ -> Left t)

-- define a parser that fails with a given message
noParse :: Parser a
noParse = errorParse NoParse

syntaxError :: String -> Parser a
syntaxError msg = errorParse $ SyntaxError msg

failParse :: String -> Parser a
failParse msg = errorParse $ ParseError msg

instance Alternative Parser where
     -- empty :: Parser a
    empty = syntaxError "Syntax error: nothing matches to parse"
    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
        -- ignore these left cases, but not the others
                     Left NoParse -> parse q inp
                     Left (ParseError _) -> parse q inp
                     otherParse -> otherParse)


-- the following will not work until you import Control.Applicative into ghci:
-- parse empty "abc"
-- parse (item <|> return 'd') "abc"
-- > Right ('a', "bc")
-- parse (empty <|> return 'd') "abc"
-- > Right ('d', "abc")


-- this gives us 3 basic parsers, item, which parses the first character of a non-empty input,
-- return v always succeeds with the value v and doesn't change the input, and empty that always fails

-- now let's define a parser that matches a single character
satisfies :: (Char -> Bool) -> Parser Char
satisfies p = do
    x <- item
    if p x
        then return x
        else failParse (show x ++ " didn't match expected character")

digit :: Parser Char
digit = satisfies isDigit

upper :: Parser Char
upper = satisfies isUpper

lower :: Parser Char
lower = satisfies isLower

letter :: Parser Char
letter = satisfies isAlpha

alphanum :: Parser Char
alphanum = satisfies isAlphaNum

-- this parser parses the given character
char :: Char -> Parser Char
char x = satisfies (== x)

space :: Parser Char
space = satisfies isSpace

breakChar :: Parser Char
breakChar = satisfies (\x -> isSpace x || x == ')' || x == '(')

-- this parser then takes that idea and parses an entire string
string :: String -> Parser String
string [] = return []
string (x:xs) = char x >> string xs >> return (x:xs)
-- alternatively, we can write it as
-- string (x:xs) =
-- do
--   char x
--   string xs
--   return (x:xs)

-- the next two parsers implement "many" and "some" which apply the
-- parser repeatedly until it fails, with the result values from each successful
-- parse being returned in a list

-- many x = Parser.some x <|> pure []
-- some x = (:) <$> x <*> Parser.many x
kstar :: Alternative f => f a -> f [a]
kstar x = kplus x <|> pure []
kplus :: Alternative f => f a -> f [a]
kplus x = (:) <$> x <*> kstar x


ident :: Parser String
ident = do
    x <- lower
    xs <- kstar alphanum
    return (x:xs)
-- parse ident "abc def"


nat :: Parser Integer
nat = do
    eatspace
    xs <- kplus digit
    -- notice, if Parser.some digit fails, then return
    -- is never called and read is never executed
    return (read xs)
-- parse nat "123 abc"

eatspace :: Parser ()
eatspace = do
    _ <- kstar space
    return ()
-- parse eatspace "   abc"


int :: Parser Integer
int = do
    -- try to parse a -n
    char '-'
    n <- nat
    return (-n)
    <|>
    -- otherwise, just parse an integer
    nat

-- parse int "-35"
-- parse int "35"

-- since most languages allow spacing freely, we will write a parser
-- that consumes space before and after a function
token :: Parser a -> Parser a
token p = do
    eatspace
    v <- p
    eatspace
    return v

spacedToken :: Parser a -> Parser a
spacedToken p = do
    eatspace
    v <- p
    kstar breakChar
    return v

-- now we can use token to construct parsers based on it
identifier :: Parser String
identifier = token ident

-- an identifier that must be followed by a space
spacedIdentifier :: Parser String
spacedIdentifier = spacedToken ident

natural :: Parser Integer
natural = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- match a symbol, but there must be a trailing space!
spacedSymbol :: String -> Parser String
spacedSymbol xs = spacedToken (string xs)

natlist :: Parser [Integer]
natlist = do
    eatspace
    symbol "["
    n <- natural
    ns <- kstar (symbol "," >> natural)
    symbol "["
    eatspace
    return (n:ns)

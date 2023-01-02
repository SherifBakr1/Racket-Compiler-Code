{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module MiniRacketParserSpec where 

import Test.Hspec
import Parser
import Expr 
import MiniRacketParser
import Error

type ParseResult = Either ErrorT (Expr, String)

expr :: Either ErrorT (a2, b) -> a2
expr (Right (e, _)) = e 
expr (Left (SyntaxError msg)) = error msg
expr (Left (ParseError msg)) = error msg
expr (Left NoParse) = error "no matching parse"
expr _ = error "expr in MiniRacketParser.hs is not fully implemented yet..."

spec :: Spec 
spec = do 
    describe "parse literals" $ do
        it "parses number: 1235" $ 
            parseStr "1235" `shouldBe` Right (LiteralExpr (IntVal 1235),"")
        it "parses negative numbers: -12235" $
            parseStr "-12235" `shouldBe` Right (LiteralExpr (IntVal (-12235)), "")
        it "parses true" $
            parseStr "true" `shouldBe` Right (LiteralExpr (BoolVal True), "")
        it "parses false" $
            parseStr "false" `shouldBe` Right (LiteralExpr (BoolVal False), "")

    describe "parse op values" $ do
        it "parses bool op and" $ 
            parse parseBoolOp "and" `shouldBe` Right (And,"")
        it "parses bool op or" $ 
            parse parseBoolOp "or" `shouldBe` Right (Or,"")
        it "parses bool op not" $ 
            parse notExpr "not" `shouldBe` Left NoParse 

    describe "parse Math Operations" $ do
        it "parses math op +" $ 
            parse parseMathOp "+" `shouldBe` Right (Add,"")
        it "parses math op -" $ 
            parse parseMathOp "-" `shouldBe` Right (Sub,"")
        it "parses math op *" $ 
            parse parseMathOp "*" `shouldBe` Right (Mul,"")
        it "parses math op /" $ 
            parse parseMathOp "div" `shouldBe` Right (Div,"")
        it "parses math op %" $ 
            parse parseMathOp "mod" `shouldBe` Right (Mod,"")

    describe "parse comparison Operations" $ do
        it "parses comp op =" $ 
            parse parseCompOp "=" `shouldBe` Right (Eq,"")
        it "parses comp op <" $ 
            parse parseCompOp "<" `shouldBe` Right (Lt,"")
        it "parses comp op >" $ 
            parse parseCompOp ">" `shouldBe` Right (Gt,"")
        it "parses comp op >=" $ 
            parse parseCompOp ">=" `shouldBe` Right (Gt,"=")
        it "parses comp op <=" $ 
            parse parseCompOp "<=" `shouldBe` Right (Lt,"=")

    describe "parse negate atom" $ do
        it "parses negate atom" $ 
            parse negateAtom "c" `shouldBe` Right (NegateExpr (VarExpr "c"),"")
        it "parses negate atom" $ 
            parse negateAtom "a" `shouldBe` Right (NegateExpr (VarExpr "a"),"")
        it "parses negate atom" $ 
            parse negateAtom "x" `shouldBe` Right (NegateExpr (VarExpr "x"),"")

    describe "parse varExpr" $ do
        it "parse varExpr" $ 
            parse varExpr "a" `shouldBe` Right (VarExpr "a","")
        it "parse varExpr" $ 
            parse varExpr "b" `shouldBe` Right (VarExpr "b","")
        it "parse varExpr" $ 
            parse varExpr "c" `shouldBe` Right (VarExpr "c","")

    describe "parse ifExpr" $ do
        it "parse ifExpr" $ 
            parse ifExpr "if e1 e2 e3" `shouldBe` Right (IfExpr (VarExpr "e1") (VarExpr "e2") (VarExpr "e3"),"")
        it "parse ifExpr" $ 
            parse ifExpr "if x1 x2 x3" `shouldBe` Right (IfExpr (VarExpr "x1") (VarExpr "x2") (VarExpr "x3"),"")
        it "parse ifExpr" $ 
            parse ifExpr "if s1 x2 y3" `shouldBe` Right (IfExpr (VarExpr "s1") (VarExpr "x2") (VarExpr "y3"),"")

    describe "parse letExpr" $ do
        it "parse letExpr" $ 
            parse letExpr "let (a true) x" `shouldBe` Right (LetExpr "a" (LiteralExpr (BoolVal True)) (VarExpr "x"),"")
        it "parse letExpr" $ 
            parse letExpr "let (t false) b" `shouldBe` Right (LetExpr "t" (LiteralExpr (BoolVal False)) (VarExpr "b"),"")
        it "parse letExpr" $ 
            parse letExpr "let (w true) g" `shouldBe` Right (LetExpr "w" (LiteralExpr (BoolVal True)) (VarExpr "g"),"")

    describe "parse lambdaExpr" $ do
        it "parse lambdaExpr" $ 
            parse lambdaExpr "lambda (x) a" `shouldBe` Right (LambdaExpr "x" (VarExpr "a"),"")
        it "parse lambdaExpr" $ 
            parse lambdaExpr "lambda (s) d" `shouldBe` Right (LambdaExpr "s" (VarExpr "d"),"")
        it "parse lambdaExpr" $ 
            parse lambdaExpr "lambda (h) a" `shouldBe` Right (LambdaExpr "h" (VarExpr "a"),"")

    describe "parse applyExpr" $ do
        it "parse applyExpr" $ 
            parse applyExpr "x y" `shouldBe` Right (ApplyExpr (VarExpr "x") (VarExpr "y"),"")
        it "parse applyExpr" $ 
            parse applyExpr "a b" `shouldBe` Right (ApplyExpr (VarExpr "a") (VarExpr "b"),"")
        it "parse applyExpr" $ 
            parse applyExpr "t u" `shouldBe` Right (ApplyExpr (VarExpr "t") (VarExpr "u"),"")

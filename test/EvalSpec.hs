module EvalSpec where


import Test.Hspec
import Parser
import Expr
import MiniRacketParser

import Eval
import Error

type ParseResult = Either ErrorT (Expr, String)

spec :: Spec
spec = do
    describe "eval expressions" $ do
        it "evaluates number: 1235" $ 
            evalStr "1235" `shouldBe` Right (IntVal 1235)
        it "evaluates negative numbers: -12235" $
            evalStr "-12235" `shouldBe` Right (IntVal (-12235))
        it "evaluates true" $
            evalStr "true" `shouldBe` Right (BoolVal True)
        it "evaluates false" $
            evalStr "false" `shouldBe` Right (BoolVal False)

    describe "eval < and =" $ do
        it "evaluates result" $ 
            evalStr"(= 1 1)" `shouldBe` Right (BoolVal True)
        it "evaluates result" $ 
            evalStr"(< 1 1)" `shouldBe` Right (BoolVal False)
        it "evaluates result" $ 
            evalStr"(< 0 1)" `shouldBe` Right (BoolVal True)
        it "evaluates result" $ 
            evalStr"(< 10 1)" `shouldBe` Right (BoolVal False)
        it "evaluates result" $ 
            evalStr"(= 10 1)" `shouldBe` Right (BoolVal False)

    describe "eval And and Or operations" $ do
        it "evaluates result of and/or ops" $ 
            evalStr "(and true false true)" `shouldBe` Right (BoolVal False)
        it "evaluates result of and/or ops" $ 
            evalStr "(or true false true)" `shouldBe` Right (BoolVal True)
        it "evaluates result of and/or ops" $ 
            evalStr "(or true false)" `shouldBe` Right (BoolVal True)
        it "evaluates result of and/or ops" $ 
            evalStr "(and true false)" `shouldBe` Right (BoolVal False)
        it "evaluates result of and/or ops" $ 
            evalStr "(and true (or true false))" `shouldBe` Right (BoolVal True)
        it "evaluates result of and/or ops" $ 
            evalStr "(or true (or true false))" `shouldBe` Right (BoolVal True)
        it "evaluates result of and/or ops" $ 
            evalStr "(and true (and true false))" `shouldBe` Right (BoolVal False)
        it "evaluates result of and/or ops" $ 
            evalStr "(or true (and true false))" `shouldBe` Right (BoolVal True)

    describe "eval +,-,*,div,mod" $ do
        it "evaluates result" $ 
            evalStr"(+ 4 3)" `shouldBe` Right (IntVal 7)
        it "evaluates result" $ 
            evalStr"(- 4 3)" `shouldBe` Right (IntVal 1)
        it "evaluates result" $ 
            evalStr"(* 4 3)" `shouldBe` Right (IntVal 12)
        it "evaluates result" $ 
            evalStr"(div 4 2)" `shouldBe` Right (IntVal 2)
        it "evaluates result" $ 
            evalStr"(mod 4 2)" `shouldBe` Right (IntVal 0)

    describe "eval > and >=" $ do
        it "evaluates result" $ 
            evalStr"(> 5 1)" `shouldBe` Right (BoolVal True)
        it "evaluates result" $ 
            evalStr"(> 1 10)" `shouldBe` Right (BoolVal False)

    describe "not operation" $ do
        it "evaluates result of not" $ 
            evalStr "(not true)" `shouldBe` Right (BoolVal False)
        it "evaluates result of not" $ 
            evalStr "(not false)" `shouldBe` Right (BoolVal True)           

    describe "not operation" $ do
        it "evaluates result of not" $ 
            evalStr "(not true)" `shouldBe` Right (BoolVal False)
        it "evaluates result of not" $ 
            evalStr "(not false)" `shouldBe` Right (BoolVal True)           

    describe "evaluates if expressions" $ do
        it "evaluates if expressions" $ 
            eval evalIfExpr ([],(IfExpr (LiteralExpr (BoolVal True)) (LiteralExpr (BoolVal True)) (LiteralExpr (BoolVal True)))) `shouldBe` Right (BoolVal True,([],EmptyExpr))
        it "evaluates if expressions" $ 
            eval evalIfExpr ([],(IfExpr (LiteralExpr (BoolVal False)) (LiteralExpr (BoolVal False)) (LiteralExpr (BoolVal False)))) `shouldBe` Right (BoolVal False,([],EmptyExpr))

    describe "evaluates Var" $ do
        it "evaluates Var" $ 
            eval evalVar ([("x",IntVal 5)], VarExpr "x") `shouldBe` Right (IntVal 5,([("x",IntVal 5)],EmptyExpr))
        it "evaluates Var" $ 
            eval evalVar ([("q",IntVal 1000)], VarExpr "q") `shouldBe` Right (IntVal 1000,([("q",IntVal 1000)],EmptyExpr))

    describe "Test call function" $ do
        it "call function" $ 
            callFun (ClosureVal "" "x" (VarExpr "x") []) (IntVal 5) `shouldBe` Right (IntVal 5)
        it "call function" $ 
            callFun (ClosureVal "" "a" (VarExpr "a") []) (IntVal 18) `shouldBe` Right (IntVal 18)

    describe "evaluates apply expression" $ do
        it "evalapplyexpr" $ 
            eval evalApplyExpr ([], (ApplyExpr (LambdaExpr "x" (VarExpr "x")) (LiteralExpr (IntVal 5)))) `shouldBe` Right (IntVal 5,([],EmptyExpr))
        it "evalapplyexpr" $ 
            eval evalApplyExpr ([], (ApplyExpr (LambdaExpr "y" (VarExpr "y")) (LiteralExpr (IntVal 4)))) `shouldBe` Right (IntVal 4,([],EmptyExpr))


    describe "evaluates lambda expression" $ do
        it "evalLambdaExpr" $ 
            eval evalLambdaExpr ([], LambdaExpr "x" (VarExpr "x")) `shouldBe` Right (ClosureVal "" "x" (VarExpr "x") [],([],EmptyExpr))
        it "evalLambdaExpr" $ 
            eval evalLambdaExpr ([], LambdaExpr "y" (VarExpr "y")) `shouldBe` Right (ClosureVal "" "y" (VarExpr "y") [],([],EmptyExpr))


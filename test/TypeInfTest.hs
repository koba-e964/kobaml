module Main where

import TypeInf
import CDef

import Test.HUnit
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = print =<< runTestTT tests

tests :: Test
tests = TestList [testTypeSchemeEqual, testGeneralize]

testTypeSchemeEqual :: Test
testTypeSchemeEqual = let
  x = TypeVar "x"
  y = TypeVar "y"
   in 
  TestLabel "test-typeSchemeEqual" $ TestList [
    TestCase $ assertBool "forall x.x /= forall y.y" (typeSchemeEqual (Forall (Set.singleton x) (TVar x)) (Forall (Set.singleton y) (TVar y)))
    ,TestCase $ assertBool "forall x. [x] == forall y. y" (not $ typeSchemeEqual (Forall (Set.singleton x) (TList $ TVar x)) (Forall (Set.singleton y) (TVar y)))
    ,TestCase $ assertBool "x == y" (not $ typeSchemeEqual (Forall Set.empty (TVar x)) (Forall Set.empty (TVar y)))
    ,TestCase $ assertBool "x == forall y. y" (not $ typeSchemeEqual (Forall Set.empty (TVar x)) (Forall (Set.singleton y) (TVar y)))
    ,TestCase $ assertBool "int /= int" (typeSchemeEqual (Forall Set.empty intType) (Forall Set.empty intType))
    ,TestCase $ assertBool "int == bool" (not $ typeSchemeEqual (Forall Set.empty intType) (Forall Set.empty boolType))
    ,TestCase $ assertBool "int == forall x. int" (not $ typeSchemeEqual (Forall Set.empty intType) (Forall (Set.singleton x) intType))
  ]


testGeneralize :: Test
testGeneralize = let
  x = TypeVar "x"
  y = TypeVar "y"
  emp = Map.empty
   in
  TestLabel "test-generalize" $ TestList [
   case generalize emp (TFun (TVar x) (TVar x)) of
     CDef.Forall set (TFun (TVar a1) (TVar a2)) -> TestList [a1 ~?= a2, set ~?= Set.singleton a1]
     o                                          -> TestCase $ assertFailure $ "not of form forall a. a -> a :" ++ show o 
   ,case generalize emp (TFun (TVar x) (TVar y)) of
     CDef.Forall set (TFun (TVar a) (TVar b)) -> TestList [TestCase $ assert $ a /= b, set ~?= Set.fromList [a, b]]
     o                                        -> TestCase $ assertFailure $ "not of form forall a b. a -> b :" ++ show o 
  ]


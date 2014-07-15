module Main where

import EvalLazy
import CDef

import Test.HUnit
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.ST
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = print =<< runTestTT tests

evalPure :: Expr -> Either EvalError String
evalPure expr = runST $ flip evalStateT Map.empty $ runExceptT $ do
  val <- eval expr
  showValueLazy val

tests :: Test
tests = TestList [testSimple]

testSimple = TestList [
   "simple1" ~: evalPure (EAdd (EConst $ VInt 1) (EConst $ VInt 2)) ~=? Right "3"]



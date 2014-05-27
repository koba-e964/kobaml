module Main where

import Eval
import ExprLexer
import ExprParser
import qualified Data.Map as Map

repl :: IO ()
repl = do
  cont <- getLine
  let tokens =  alexScanTokens cont
      cmd    = exparse tokens
   in
    case cmd of
      CLet (Name name) expr -> undefined
      CRLets ls             -> undefined
      CExp expr             -> print (eval Map.empty expr) >> repl
      CQuit                 -> return ()

main :: IO ()
main = repl

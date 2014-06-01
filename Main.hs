module Main where

import Control.Monad.State

import Eval
import ExprLexer
import ExprParser
import qualified Data.Map as Map
import TypeInf

repl :: Env -> St IO ()
repl env = do
  cont <- lift getLine
  let tokens =  alexScanTokens cont
      cmd    = exparse tokens
   in
    case cmd of
      CLet (Name name) expr ->
        let newenv = Map.insert name (eval env expr) env in
          repl newenv
      CRLets bindings       -> repl $ getNewEnvInRLets bindings env
      CExp expr             -> lift (print (eval env expr)) >> repl env
      CQuit                 -> return ()

main :: IO ()
main = runStateT (repl Map.empty) 0 >> return ()

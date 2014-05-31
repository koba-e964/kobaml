module Main where

import Eval
import ExprLexer
import ExprParser
import qualified Data.Map as Map

repl :: Env -> IO ()
repl env = do
  cont <- getLine
  let tokens =  alexScanTokens cont
      cmd    = exparse tokens
   in
    case cmd of
      CLet (Name name) expr ->
        let newenv = Map.insert name (eval env expr) env in
          repl newenv
      CRLets bindings       -> repl $ getNewEnvInRLets bindings env
      CExp expr             -> print (eval env expr) >> repl env
      CQuit                 -> return ()

main :: IO ()
main = repl Map.empty

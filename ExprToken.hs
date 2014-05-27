module ExprToken where

import Prelude hiding (EQ, LT, GT)

data Token 
    = INT Int
    | ID String
    | EQ
    | LET
    | REC
    | DAND
    | IN
    | IF
    | THEN
    | ELSE
    | MATCH
    | WITH
    | BAR
    | FUN
    | ARROW
    | TRUE
    | FALSE
    | PLUS
    | MINUS
    | TIMES
    | DIV
    | CONS
    | LBRACKET
    | RBRACKET
    | OR
    | AND
    | LT | LE | GT | GE
    | LPAR | RPAR
    | EOC
    | EOF
    deriving (Eq, Show)

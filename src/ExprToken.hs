module ExprToken where

import Prelude hiding (EQ, LT, GT)

data Token 
    = INT Int
    | ID String
    | EQ | NEQ
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
    | MOD
    | CONS
    | LBRACKET
    | RBRACKET
    | OR
    | AND
    | LT | LE | GT | GE
    | LPAR | RPAR | COMMA
    | DDOT {- .. -}
    | SSEQ {- #seq for forcing to evaluate, infix -}
    | STR String
    | EOC
    | SQUOTE {- single quotation, '\'' -}
    | EOF
    | VIEW {- "#v", view the specified object -}
    deriving (Eq, Show)

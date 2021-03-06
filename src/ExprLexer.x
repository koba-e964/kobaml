{
{-# OPTIONS_GHC -w #-}
module ExprLexer where

import CDef
import ExprToken
import Prelude hiding (EQ, LT, GT)

}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$ascii_x_esc = [\! \# \$ \% & \' \( \) \;:\<\=\>\?0-9 \+ \, \- \* \. \/ \@ A-Z\[\]\^ a-z \{ \_ \} ]
$esc = [ \\ \" trn0]

tokens :-

  $white+       ;
  "let"       { \_->LET }
  "rec"       { \_->REC }
  "and"       { \_->DAND }
  "in"        { \_->IN  }
  "if"        { \_->IF  }
  "then"      { \_->THEN }
  "else"      { \_->ELSE }

  "match"     { \_->MATCH }
  "with"      { \_->WITH }

  "fun"       { \_->FUN }

  "true"      { \_->TRUE }
  "false"     { \_->FALSE }

  "="         { \_->EQ }
  "+"         { \_->PLUS }
  "-"         { \_->MINUS }
  "*"         { \_->TIMES }
  "/"         { \_->DIV }
  "mod"       { \_->MOD }
  "/="        { \_->NEQ }
  "->"        { \_->ARROW } 
  "|"         { \_->BAR } 
  "<"         { \_->LT }
  ">"         { \_->GT }  
  "<="        { \_->LE }
  ">="        { \_->GE }

  "||"        { \_->OR }
  "&&"        { \_->AND }

  "::"        { \_->CONS }

  ";;"        { \_->EOC } 
  "("         { \_->LPAR }
  ")"         { \_->RPAR }

  "["         { \_->LBRACKET }
  "]"         { \_->RBRACKET }
  ","         { \_->COMMA    }
  ".."        { \_->DDOT     }

  "#seq"      { \_->SSEQ }
  "'"         { \_->SQUOTE }
  "->"        { \_->ARROW }
  "#v"        { \_->VIEW }
  $digit+ { \n ->INT (read n) }
  $alpha [$alpha $digit \_ \']* { ID }
  \"((\\$esc) | $ascii_x_esc)*\" { STR }
  eof     { \_->EOF }

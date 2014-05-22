{
module TypeParse where

import Control.Applicative hiding ((<|>), optional)
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import TypeInf
import Prelude hiding (words)

data Token = 
   ID String
  |LPAREN
  |RPAREN
  |SQUOTE
  |ARROW deriving (Eq, Show)



word :: Parser Token
word = (ID <$> (many1 (digit <|> letter) <* optional spaces))
  <|> (string "->" *> return ARROW) 
  <|> (string "("  *> return LPAREN)
  <|> (string ")"  *> return RPAREN)
  <|> (string "'"  *> return SQUOTE)

words :: Parser [Token]
words = optional spaces *> wd
  where wd = option [] ((:) <$> word <*> words)

tlex :: String -> [Token]
tlex str = case parse words undefined str of
  Left _ -> error "parse error"
  Right x -> x

typeOfString :: String -> Type
typeOfString = tparse . tlex

parseError :: [Token] -> a
parseError toks = error $ "tokens:" ++ show toks
}

%name tparse
%tokentype {Token}
%error {parseError}

%token
 id  {ID $$}
 '(' {LPAREN}
 ')' {RPAREN}
 '\'' {SQUOTE}
 arr {ARROW}
%%


ty  : aty arr ty {TFun $1 $3}
    | aty        {$1}
aty : id         {TConc $1}
    | '\'' id    {TVar (TypeVar $2)}
    | '(' ty ')' {$2}


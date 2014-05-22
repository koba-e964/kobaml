module TypeParse where

import Control.Applicative hiding ((<|>), optional)
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec
import Prelude hiding (words)

data Token = 
   ID String
  |LPAREN
  |RPAREN
  |ARROW deriving (Eq, Show)



word :: Parser Token
word = (ID <$> (many1 (digit <|> letter) <* optional spaces))
  <|> (string "->" *> return ARROW) 
  <|> (string "("  *> return LPAREN)
  <|> (string ")"  *> return RPAREN)

words :: Parser [Token]
words = optional spaces *> wd
  where wd = option [] ((:) <$> word <*> words)



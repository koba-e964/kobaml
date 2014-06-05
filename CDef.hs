{-# LANGUAGE DeriveDataTypeable #-}
module CDef where

import Data.Typeable
import Control.Exception

{-------------------
    Exceptions
-------------------}

data ParseError = ParseError String deriving (Typeable, Show)

instance Exception ParseError
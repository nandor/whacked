{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Whacked.Frontend.Parser
  ( parse
  , ParseError
  ) where

import Whacked.AST
import Text.ParsecCombinator.Parsec

parse :: String -> Either ParseError AProgram
parse source
 = Right $ AProgram [] []

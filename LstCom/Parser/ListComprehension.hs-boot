-- |
module LstCom.Parser.ListComprehension
where

import Prelude
import Text.ParserCombinators.Parsec (GenParser)

parseLstCom :: Bool -> GenParser Char st [String]

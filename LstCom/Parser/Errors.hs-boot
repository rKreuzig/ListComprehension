-- |
module LstCom.Parser.Errors
where

import Prelude
import Text.ParserCombinators.Parsec (GenParser)

outOfTplLstErrorRight :: String -> Char -> GenParser Char st ()
outOfTplLstErrorLeft :: String -> GenParser Char st ()

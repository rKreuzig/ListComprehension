-- |
module LstCom.Parser.Pattern (
-- * Functions
  parsePattern, parsePat
) where

import Prelude
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec

import LstCom.Utils
import LstCom.Constants
import LstCom.Parser.Errors
import LstCom.Parser.General
import LstCom.Parser.Helpers

parsePattern :: String
             -> Either ParseError [String]
parsePattern input = parse parsePat "" input

parsePat :: GenParser Char st [String]
parsePat = do
  noInputError
  notClosedError
  pat True

pat :: Bool
    -> GenParser Char st [String]
pat top = do
  emptyTplLstError
  input <- getInput
  let trimInput = trimL input
  case head trimInput of
    _ | top, topElem ":" trimInput -> patInfixLst True
    _ | topElem ":" trimInput      -> patInfixLst False
    '['                            -> parTplLst (pat True) '[' ']'
    '('                            -> parTplLst (pat True) '(' ')'
    _ | elem '[' trimInput         -> outOfTplLstErrorLeft input >> return []
    _ | elem '(' trimInput         -> outOfTplLstErrorLeft input >> return []
    _                              -> do {name <- var; return [name]}

patInfixLst :: Bool
            -> GenParser Char st [String]
patInfixLst hasEnd = do
  sourcePos <- getPosition
  (pre, post) <- splitByTopLastSign ":"
  if null (trim pre) || null (trim post)
    then do
      let pos = sourceColumn sourcePos + length pre
      fail $ e_COLON_WITHOUT_INPUT ++ show (pos, pos)
    else do
      setPosition sourcePos
      setInput pre
      parsedPre <- pat False

      let lastPos = sourceColumn sourcePos
      let curPos = setSourceColumn sourcePos $ length pre + lastPos + 1
      setPosition curPos
      setInput post

      if hasEnd
        then do
          parsedPost <- patInfixLstEnd
          return $ parsedPre ++ parsedPost
        else do
          parsedPost <- pat False
          return $ parsedPre ++ parsedPost

patInfixLstEnd :: GenParser Char st [String]
patInfixLstEnd = do
  emptyTupleError
  input <- getInput
  let trimInput = trimL input
  case head trimInput of
    '('                    -> patInfixLstParEnd
    '['                    -> parTplLst (pat True) '[' ']'
    _ | elem '[' trimInput -> outOfTplLstErrorLeft input >> return []
    _ | elem '(' trimInput -> outOfTplLstErrorLeft input >> return []
    _                      -> do {name <- var; return [name]}

patInfixLstParEnd :: GenParser Char st [String]
patInfixLstParEnd = do
  spaces
  char '('

  input <- getInput
  let trimInput = trimR input
  if not $ last trimInput == ')'
    then outOfTplLstErrorRight trimInput ')' >> return []
    else do
      setInput $ init trimInput
      patInfixLstEnd

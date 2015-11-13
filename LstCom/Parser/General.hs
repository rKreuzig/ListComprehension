-- |
module LstCom.Parser.General (
-- * Functions
  emptyTuple, emptyList, parTplLst, var, getList
) where

import Prelude
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.Char (isSpace, generalCategory, GeneralCategory (..))

import LstCom.Utils
import LstCom.Constants
import LstCom.Parser.Helpers
import {-# SOURCE #-} LstCom.Parser.Errors

getList :: String
        -> Either String [String]
getList input =
  let eResult = parse getListParser "" input
  in case eResult of
    Right r         -> Right r
    Left parseError -> Left . messageString . last . errorMessages $ parseError

getListParser :: GenParser Char st [String]
getListParser = do
  let func = do {input <- getInput; return [input]}
      parserLst = parTplLst func '[' ']'

  let chr = do {c <- satisfy (\x -> x /= '"'); return $ "'" ++ [c] ++ "'"}
      parserStrg = do {
        char '"';
        lst <- many chr;
        char '"';
        return lst
      }

  try parserLst <|> parserStrg

emptyTuple :: GenParser Char st ()
emptyTuple = do
  spaces
  char '('
  spaces
  char ')'
  spaces
  return ()

emptyList :: GenParser Char st ()
emptyList = do
  spaces
  char '['
  spaces
  char ']'
  spaces
  return ()

parTplLst :: GenParser Char st [String]
          -> Char
          -> Char
          -> GenParser Char st [String]
parTplLst func open close = do
  spaces
  char open

  input <- getInput
  let trimInput = trimR input
  if not $ last trimInput == close
    then outOfTplLstErrorRight trimInput close >> return []
    else do
      let foldFunc = (\x y -> if isSpace y then x else False)
      let initInput = init trimInput

      if foldl foldFunc True initInput
        then return []
        else do
          sourcePos <- getPosition
          let pos = sourceColumn sourcePos
          setInput $ initInput

          lst <- splitByTopSign ","
          parTplLstElements True func lst pos

parTplLstElements :: Bool
                  -> GenParser Char st [String]
                  -> [String]
                  -> Int
                  -> GenParser Char st [String]
parTplLstElements _ _ [] _ = return []
parTplLstElements startFlag func (e:eles) pos = do
  if null $ trim e
    then
      if startFlag
        then do
          let failPos = pos + length e
          fail $ e_COMMA_WITHOUT_INPUT ++ show (failPos, failPos)
        else do
          let failPos = pos - 1
          fail $ e_COMMA_WITHOUT_INPUT ++ show (failPos, failPos)
    else do
      setInput e
      sourcePos <- getPosition
      let curPos = setSourceColumn sourcePos pos
      setPosition curPos

      parsed <- func
      rest <- parTplLstElements False func eles $ pos + length e + 1
      return $ parsed ++ rest

var :: GenParser Char st String
var = do
  spaces
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  let notSatFunc = (\x -> generalCategory x /= LowercaseLetter && x /= '_')
  let failMsg = e_SIGN_NOT_ALLOWED ++ show (pos, pos)
  optional $ try $ satisfy notSatFunc >> fail failMsg

  startSign <- satisfy (\x -> generalCategory x == LowercaseLetter || x == '_')
  rest <- many (satisfy varAllowedChars)
  spaces

  stop <- end
  if not stop
    then do
      sourcePos <- getPosition
      let pos = sourceColumn sourcePos
      fail $ e_SIGN_NOT_ALLOWED ++ show (pos, pos)
    else do
      let name = startSign:rest
      if name `elem` c_KEYWORDS || name `elem` c_PRELUDE_FUNCS_NAMES
        then
          fail $ e_FORBIDDEN_VAR_NAME ++ show (pos, pos + length name - 1)
        else
          return name

varAllowedChars :: Char
                -> Bool
varAllowedChars x
  | x == '\'' || x == '_' || cat == LowercaseLetter || cat == UppercaseLetter
    = True
  | cat == TitlecaseLetter || cat == DecimalNumber
    = True
  | otherwise
    = False
  where
    cat = generalCategory x

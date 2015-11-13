-- |
module LstCom.Parser.Errors (
-- * Functions
  notClosedError, noInputError, emptyTplLstError, emptyTupleError,
  emptyListError, outOfTplLstErrorRight, outOfTplLstErrorLeft
) where

import Prelude
import Text.ParserCombinators.Parsec

import LstCom.Constants
import LstCom.Parser.General
import LstCom.Parser.Helpers

notClosedError :: GenParser Char st ()
notClosedError = do
  startInput <- getInput
  startPos <- getPosition

  split "" (-1, "") ([], []) ([], [], [], [])

  setPosition startPos
  setInput startInput
  return ()

noInputError :: GenParser Char st ()
noInputError = do
  startInput <- getInput
  startPos <- getPosition
  let startPosNum = sourceColumn startPos

  spaces
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  isEnd <- end
  if isEnd
    then fail $ e_NO_INPUT ++ show (startPosNum, pos)
    else do
      setPosition startPos
      setInput startInput
      return ()

outOfTplLstErrorRight :: String
                      -> Char
                      -> GenParser Char st ()
outOfTplLstErrorRight input close = do
  let lstParTpl = reverse $ dropWhile (/= close) $ reverse input
  string lstParTpl
  rest <- getInput
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos
  let posTpl = show (pos, pos + length rest - 1)
  fail $ e_INPUT_OUT_OF_TPL_LST ++ posTpl

outOfTplLstErrorLeft :: String
                     -> GenParser Char st ()
outOfTplLstErrorLeft input = do
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos
  let errorTxt = takeWhile (\x -> x /= '(' && x /= '[') input
  let posTpl = show (pos, pos + length errorTxt - 1)
  fail $ e_INPUT_OUT_OF_TPL_LST ++ posTpl

emptyTupleError :: GenParser Char st ()
emptyTupleError = do
  emptyTplLstErrorWorker emptyTupleErrorWithPos emptyTplLstErrorFake

emptyListError :: GenParser Char st ()
emptyListError = do
  emptyTplLstErrorWorker emptyTplLstErrorFake emptyListErrorWithPos

emptyTplLstError :: GenParser Char st ()
emptyTplLstError = do
  emptyTplLstErrorWorker emptyTupleErrorWithPos emptyListErrorWithPos
  return ()

emptyTupleErrorWithPos :: String
                       -> Int
                       -> GenParser Char st ()
emptyTupleErrorWithPos input pos = do
  optional $ try emptyTuple >> emptyTplLstErrorThrow input pos '(' e_EMPTY_TUPLE
  return ()

emptyListErrorWithPos :: String
                      -> Int
                      -> GenParser Char st ()
emptyListErrorWithPos input pos = do
  optional $ try emptyList >> emptyTplLstErrorThrow input pos '[' e_EMPTY_LIST
  return ()

emptyTplLstErrorThrow :: String
                      -> Int
                      -> Char
                      -> String
                      -> GenParser Char st ()
emptyTplLstErrorThrow input pos sign msg = do
  setInput input
  spaces
  char sign
  sp <- many space
  let posTpl = show (pos, pos + length sp + 1)
  fail $ msg ++ posTpl

emptyTplLstErrorFake :: String
                     -> Int
                     -> GenParser Char st ()
emptyTplLstErrorFake input pos = return ()

emptyTplLstErrorWorker :: (String -> Int -> GenParser Char st ())
                       -> (String -> Int -> GenParser Char st ())
                       -> GenParser Char st ()
emptyTplLstErrorWorker tplFunc lstFunc = do
  startPos <- getPosition
  let startPosNum = sourceColumn startPos
  startInput <- getInput

  spaces
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  tplFunc startInput pos
  lstFunc startInput pos
  manyTill eof anyToken

  setPosition startPos
  setInput startInput
  return ()

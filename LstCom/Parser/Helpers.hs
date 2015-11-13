-- |
module LstCom.Parser.Helpers (
-- * Functions
  end, topElem, splitByTopSign, splitByTopLastSign, splitByTopLastOperator,
  split, splitByWords, splitByTopFirstOperator
) where

import Prelude
import Text.ParserCombinators.Parsec

import LstCom.Utils
import LstCom.Constants

end :: GenParser Char st Bool
end = do
  input <- getInput
  if length input > 0 then return False else return True

topElem :: String
        -> String
        -> Bool
topElem sign input = do
  let res = parse (splitByTopSign sign) "" input
  case res of
    Left _  -> False
    Right r -> if length r > 1 then True else False

splitByWords :: GenParser Char st [String]
splitByWords = do
  lst <- split "" (-1, "") ([" "], []) ([], [], [], [])
  let foldFunc = \eles ele -> if ele == " " || null ele
                                then init eles ++ [last eles ++ ele]
                                else eles ++ [ele]
  return $ foldl foldFunc [head lst] $ tail lst

splitByTopSign :: String
               -> GenParser Char st [String]
splitByTopSign sign = do
  lst <- split "" (-1, "") ([sign], []) ([], [], [], [])
  let foldFunc = \eles ele -> if ele == sign then eles else eles ++ [ele]
  return $ foldl foldFunc [] lst

splitByTopLastSign :: String
                   -> GenParser Char st (String, String)
splitByTopLastSign sign = do
  lst <- split "" (-1, "") ([sign], []) ([], [], [], [])
  if length lst > 1
    then do
      let left = concat . init . init $ lst
      let right = last lst
      return (left, right)
    else
      return ("", "")

splitByTopFirstOperator :: GenParser Char st (String, String, String)
splitByTopFirstOperator = do
  let lstTpl = ([], [], [], [])
  lst <- split "" (-1, "") (c_SYMBOL_FUNCS, c_PRELUDE_FUNCS_NAMES) lstTpl
  if length lst > 1
    then do
      let left = head lst
      let op' = head . tail $ lst
      let op = if head op' == '`' then init . tail $ op' else op'
      let right = concat . tail . tail $ lst
      return (left, op, right)
    else
      return ("", "", "")

splitByTopLastOperator :: GenParser Char st (String, String, String)
splitByTopLastOperator = do
  let lstTpl = ([], [], [], [])
  lst <- split "" (-1, "") (c_SYMBOL_FUNCS, c_PRELUDE_FUNCS_NAMES) lstTpl
  if length lst > 1
    then do
      let right = last lst
      let op' = last . init $ lst
      let op = if head op' == '`' then init . tail $ op' else op'
      let left = concat . init . init $ lst
      return (left, op, right)
    else
      return ("", "", "")

split :: String
      -> (Int, String)
      -> ([String], [String])
      -> ([Int], [Int], [Int], [Int])
      -> GenParser Char st [String]
split buffer wordTpl breaks@(signLst, _) posLists = do
  input <- getInput
  setInput $ input ++ "  "

  lst <- splitWorker buffer wordTpl breaks posLists
  let nLst = init lst ++ [init . init . last $ lst]

  if not $ ".." `elem` signLst
    then return $ foldl removePointPointFunc [] nLst
    else return nLst

splitWorker :: String
            -> (Int, String)
            -> ([String], [String])
            -> ([Int], [Int], [Int], [Int])
            -> GenParser Char st [String]
splitWorker buffer wordTpl breaks posLists = do
  stop <- end
  if stop
    then do
      splitEnd buffer wordTpl posLists
    else do
      curSign <- anyChar
      splitCases buffer curSign wordTpl breaks posLists

removePointPointFunc :: [String]
                     -> String
                     -> [String]
removePointPointFunc lst ele
  | ele == "..", null lst
    = lst ++ [ele]
  | ele == ".."
    = init lst ++ [last lst ++ ".."]
  | not $ null lst
    = let prev = last lst
      in if length prev > 1
        then
          let twoSigns = [last . init $ prev] ++ [last prev]
          in if twoSigns == ".."
            then init lst ++ [prev ++ ele]
            else lst ++ [ele]
        else lst ++ [ele]
  | otherwise
    = lst ++ [ele]

splitEnd :: String
         -> (Int, String)
         -> ([Int], [Int], [Int], [Int])
         -> GenParser Char st [String]
splitEnd buffer wordTpl posLists
  | (not $ null quotLst) || (not $ null apoLst)
    = do
        let quotApoLst = quotLst ++ apoLst
        let pos = head quotApoLst
        fail $ e_QUOTATION_APOSTROPHE_NOT_CLOSED ++ show (pos, pos)
  | (not $ null norBraLst) || (not $ null squBraLst)
    = do
        let braLst = norBraLst ++ squBraLst
        let pos = head braLst
        fail $ e_BRACKET_NOT_CLOSED ++ show (pos, pos)
  | pos /= -1
    = fail $ e_INV_COMMA_NOT_CLOSED ++ show (pos, pos)
  | otherwise
    = return [buffer]
  where
    (quotLst, apoLst, norBraLst, squBraLst) = posLists
    (pos, _) = wordTpl

splitCases :: String
           -> Char
           -> (Int, String)
           -> ([String], [String])
           -> ([Int], [Int], [Int], [Int])
           -> GenParser Char st [String]
splitCases buffer curSign wordTpl breaks posLists
  | wordStart == -1, curSign == '`', emptyLists
    = do
        sourcePos <- getPosition
        let pos = sourceColumn sourcePos - 1
        openSign '(' buffer (pos, "") breaks posLists
  | wordStart /= -1, curSign == '`'
    = checkWord buffer wordTpl breaks posLists
  | wordStart /= -1
    = let nWord = curWord ++ [curSign]
      in splitWorker buffer (wordStart, nWord) breaks posLists

  | curSign `elem` ['"', '\'', '(', '['], emptyQuotApo
    = openSign curSign buffer wordTpl breaks posLists
  | (curSign == ')' && null norBraLst) || (curSign == ']' && null squBraLst)
    , emptyQuotApo
    = throwBracketError
  | curSign `elem` [')', ']'], emptyQuotApo
    = closeSign curSign buffer wordTpl breaks posLists
  | (curSign == '"' && null apoLst) || (curSign == '\'' && null quotLst)
    = closeSign curSign buffer wordTpl breaks posLists

  | emptyLists
    = checkSymbols buffer curSign wordTpl breaks posLists
  | otherwise
    = let nBuffer = buffer ++ [curSign]
      in splitWorker nBuffer wordTpl breaks posLists
  where
    (wordStart, curWord) = wordTpl
    (quotLst, apoLst, norBraLst, squBraLst) = posLists
    emptyLists = (null quotLst) && (null apoLst) && (null norBraLst)
                 && (null squBraLst)
    emptyQuotApo = null quotLst && null apoLst

checkWord :: String
          -> (Int, String)
          -> ([String], [String])
          -> ([Int], [Int], [Int], [Int])
          -> GenParser Char st [String]
checkWord buffer wordTpl breaks posLists
  | null wordLst
    = let nBuffer = init buffer ++ "`" ++ curWord ++ "`"
      in closeSignEnd [] nBuffer (-1, "") breaks lstTpl
  | curWord `notElem` wordLst
    = fail $ e_UNKNOWN_FUNCTION ++ posTpl
  | args > 2
    = fail $ e_FUNCTION_NEED_TOO_MUCH_ARGS ++ posTpl
  | args < 2
    = fail $ e_FUNCTION_NEED_NOT_ENOUGH_ARGS ++ posTpl
  | otherwise
    = do
        rest <- splitWorker "" (-1, "") breaks lstTpl
        return $ [init buffer] ++ ["`" ++ curWord ++ "`"] ++ rest
  where
    (_, wordLst) = breaks
    (wordStart, curWord) = wordTpl
    wordEnd = wordStart + length curWord + 1
    args = getFuncArgNum curWord
    posTpl = show (wordStart, wordEnd)
    lstTpl = ([], [], [], [])

checkSymbols :: String
             -> Char
             -> (Int, String)
             -> ([String], [String])
             -> ([Int], [Int], [Int], [Int])
             -> GenParser Char st [String]
checkSymbols buffer curSign wordTpl breaks posLists
  | lenTwo, (twoSigns `elem` signLst) || twoSigns == ".."
    = do
        rest <- splitWorker [curSign] wordTpl breaks posLists
        return $ [init . init $ buffer] ++ [twoSigns] ++ rest
  | not emptyLists, lenTwo, oneSign `elem` signLst, oneSignLast `elem` signLst
    = do
        rest <- splitWorker [curSign] wordTpl breaks posLists
        return $ [init . init $ buffer] ++ [oneSign] ++ [oneSignLast] ++ rest
  | lenTwo, oneSign `elem` signLst
    = do
        let nBuffer = [last twoSigns] ++ [curSign]
        rest <- splitWorker nBuffer wordTpl breaks posLists
        return $ [init . init $ buffer] ++ [oneSign] ++ rest
  | not emptyLists, lenOne, oneSignLast `elem` signLst
    = do
        rest <- splitWorker [curSign] wordTpl breaks posLists
        return $ [init buffer] ++ [oneSignLast] ++ rest
  | otherwise
    = let nBuffer = buffer ++ [curSign]
      in splitWorker nBuffer wordTpl breaks posLists
  where
    (signLst, _) = breaks
    (quotLst, apoLst, norBraLst, squBraLst) = posLists
    emptyLists = (null quotLst) && (null apoLst) && (null norBraLst)
                 && (null squBraLst)

    twoSigns = if length buffer > 1
                then [last . init $ buffer] ++ [last buffer]
                else ""
    lenTwo = not $ null twoSigns
    oneSign = if lenTwo then [head twoSigns] else ""

    oneSignLast = if length buffer > 0
                    then [last buffer]
                    else ""
    lenOne = not $ null oneSignLast

openSign :: Char
         -> String
         -> (Int, String)
         -> ([String], [String])
         -> ([Int], [Int], [Int], [Int])
         -> GenParser Char st [String]
openSign sign buffer wordTpl breaks posLists
  | emptyLists
    = do
        lsts <- addPos sign posLists
        checkSymbols buffer sign wordTpl breaks lsts
  | otherwise
    = do
        let nBuffer = buffer ++ [sign]
        lsts <- addPos sign posLists
        splitWorker nBuffer wordTpl breaks lsts
  where
    (quotLst, apoLst, norBraLst, squBraLst) = posLists
    emptyLists = (null quotLst) && (null apoLst) && (null norBraLst)
                 && (null squBraLst)

addPos :: Char
       -> ([Int], [Int], [Int], [Int])
       -> GenParser Char st ([Int], [Int], [Int], [Int])
addPos sign (quotLst, apoLst, norBraLst, squBraLst) = do
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos - 1
  case sign of
    '"'  -> return (quotLst ++ [pos], apoLst, norBraLst, squBraLst)
    '\'' -> return (quotLst, apoLst ++ [pos], norBraLst, squBraLst)
    '('  -> return (quotLst, apoLst, norBraLst ++ [pos], squBraLst)
    '['  -> return (quotLst, apoLst, norBraLst, squBraLst ++ [pos])

closeSign :: Char
          -> String
          -> (Int, String)
          -> ([String], [String])
          -> ([Int], [Int], [Int], [Int])
          -> GenParser Char st [String]
closeSign sign buffer wordTpl breaks posLists = do
  let nBuffer = buffer ++ [sign]
  lsts@(quotLst, apoLst, norBraLst, squBraLst) <- initPos sign posLists
  case sign of
    '"'  -> closeSignEnd quotLst nBuffer wordTpl breaks lsts
    '\'' -> closeSignEnd apoLst nBuffer wordTpl breaks lsts
    ')'  -> closeSignEnd norBraLst nBuffer wordTpl breaks lsts
    ']'  -> closeSignEnd squBraLst nBuffer wordTpl breaks lsts

initPos :: Char
        -> ([Int], [Int], [Int], [Int])
        -> GenParser Char st ([Int], [Int], [Int], [Int])
initPos sign (quotLst, apoLst, norBraLst, squBraLst) =
  case sign of
    '"'  -> return (init quotLst, apoLst, norBraLst, squBraLst)
    '\'' -> return (quotLst, init apoLst, norBraLst, squBraLst)
    ')'  -> return (quotLst, apoLst, init norBraLst, squBraLst)
    ']'  -> return (quotLst, apoLst, norBraLst, init squBraLst)

closeSignEnd :: [Int]
             -> String
             -> (Int, String)
             -> ([String], [String])
             -> ([Int], [Int], [Int], [Int])
             -> GenParser Char st [String]
closeSignEnd lst buffer wordTpl breaks posLists
  | null lst
    = do
        rest <- splitWorker "" wordTpl breaks posLists
        return $ [buffer ++ head rest] ++ tail rest
  | otherwise
    = splitWorker buffer wordTpl breaks posLists

throwBracketError :: GenParser Char st [String]
throwBracketError = do
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos - 1
  fail $ e_BRACKET_NOT_OPENED ++ show (pos, pos)

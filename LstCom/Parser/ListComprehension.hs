-- |
module LstCom.Parser.ListComprehension (
-- * Functions
  parseListComprehension, parseLstCom, getTokens
) where

import Prelude
import Data.List (isInfixOf, isPrefixOf)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

import LstCom.Data
import LstCom.Utils
import LstCom.Constants
import LstCom.Parser.Errors
import LstCom.Parser.Pattern
import LstCom.Parser.Helpers
import LstCom.Parser.Expression

getTokens :: String
          -> Either String (String, [Qualifier])
getTokens input =
  let eResult = parse getTokensParser "" input
  in case eResult of
    Right r         -> Right r
    Left parseError -> Left . messageString . last . errorMessages $ parseError

getTokensParser :: GenParser Char st (String, [Qualifier])
getTokensParser = do
  spaces
  char '['

  sourcePos <- getPosition
  let pos = sourceColumn sourcePos
  input <- getInput
  setInput $ init . trimR $ input

  (leftExpr, r) <- splitByTopLastSign "|"

  setInput r
  preQuals <- splitByTopSign ","
  setPosition $ setSourceColumn sourcePos $ pos + length leftExpr + 1
  quals <- getQualTokens preQuals

  return (leftExpr, quals)

getQualTokens :: [String]
              -> GenParser Char st [Qualifier]
getQualTokens [] = return []
getQualTokens (qual:quals) = do
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos
  let nPos = pos + length qual + 1

  if topElem "<-" qual
    then do
      setInput qual
      (l, r) <- splitByTopLastSign "<-"
      setPosition $ setSourceColumn sourcePos nPos
      rest <- getQualTokens quals
      return $ (Qualifier r Generator pos l []):rest
    else do
      setPosition $ setSourceColumn sourcePos nPos
      rest <- getQualTokens quals
      return $ (Qualifier qual Guard pos "" []):rest

parseListComprehension :: String
                       -> Either String [String]
parseListComprehension input
  | isInfixOf "'''" input || isInfixOf "\\" input
    = Left $ e_BACKSLASH_NOT_ALLOWED ++ getBackslashTpl input 1
  | otherwise
    = let eResult = parse (parseLstCom False) "" input
      in case eResult of
        Right r     -> Right r
        Left pError -> Left . messageString . last . errorMessages $ pError

getBackslashTpl :: String
                -> Int
                -> String
getBackslashTpl input pos
  | isPrefixOf "'''" input
    = show (pos + 1, pos + 1)
  | isPrefixOf "\\" input
    = show (pos, pos)
  | otherwise
    = getBackslashTpl (tail input) $ pos + 1

parseLstCom :: Bool
            -> GenParser Char st [String]
parseLstCom varsF = do
  noInputError
  notClosedError
  let tpl = show (0, maxBound :: Int)

  spaces
  char '[' <|> fail (e_LST_COM_WRONG_START ++ tpl)
  spaces

  input <- getInput
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  let trimInput = trimR input
  let lastSign = last trimInput
  let noBraInput = init trimInput

  setInput noBraInput
  lst <- splitByTopSign "|"
  case True of
    _ | lastSign /= ']' -> fail (e_LST_COM_WRONG_END ++ tpl)
    _ | length lst == 1 -> fail (e_LST_COM_NO_VER_LINE ++ tpl)
    _ | length lst > 2  -> fail (e_LST_COM_MANY_VER_LINES ++ tpl)
    _ | otherwise       -> setPosition sourcePos >> parseLstComWorker varsF lst

parseLstComWorker :: Bool
                  -> [String]
                  -> GenParser Char st [String]
parseLstComWorker varsF lst = do
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  let resExpr = head lst
  let quals = last lst

  let tpl = show (0, maxBound :: Int)
  case (trim resExpr, trim quals) of
    ("", _) -> fail (e_LST_COM_NO_LEFT_EXPR ++ tpl)
    (_, "") -> fail (e_LST_COM_NO_QUAL ++ tpl)
    _       -> do
                setInput resExpr
                usedVars <- parseExpr

                setInput quals
                let curPos = pos + length resExpr + 1
                setPosition $ setSourceColumn sourcePos curPos
                (declVars, qUsedVars) <- parseQuals varsF curPos

                if varsF
                  then do
                    let nUvars = removeElements usedVars declVars
                    return $ nUvars ++ qUsedVars
                  else
                    let notDeclVar = containsList usedVars declVars
                    in if null notDeclVar
                      then
                        return []
                      else do
                        throwNotDeclaredError pos (head notDeclVar) resExpr
                        return []

parseQuals :: Bool
           -> Int
           -> GenParser Char st ([String], [String])
parseQuals varsF curPos = do
  qualLst <- splitByTopSign ","
  parseQualsWorker varsF True qualLst curPos [] []

parseQualsWorker :: Bool
                 -> Bool
                 -> [String]
                 -> Int
                 -> [String]
                 -> [String]
                 -> GenParser Char st ([String], [String])
parseQualsWorker _ _ [] _ dvars uvars = return (dvars, uvars)
parseQualsWorker varsF startF (qual:rest) pos dvars uvars
  | null $ trim qual, startF
    = do
        let failPos = pos + length qual
        fail $ e_COMMA_WITHOUT_INPUT ++ show (failPos, failPos)
  | null $ trim qual
    = do
        let failPos = pos - 1
        fail $ e_COMMA_WITHOUT_INPUT ++ show (failPos, failPos)
  | otherwise
    = do
        setInput qual
        sourcePos <- getPosition
        setPosition $ setSourceColumn sourcePos pos

        if topElem "<-" qual
          then do
            (nDVars, usedVs) <- parseGenerator pos
            let nUVars = uvars ++ usedVs
            parseQualsCases varsF (qual:rest) pos dvars nDVars nUVars
          else do
            usedVars <- parseExpr
            let nUVars = uvars ++ usedVars
            parseQualsCases varsF (qual:rest) pos dvars [] nUVars

parseQualsCases :: Bool
                -> [String]
                -> Int
                -> [String]
                -> [String]
                -> [String]
                -> GenParser Char st ([String], [String])
parseQualsCases varsF (qual:rest) pos dvars nDVars uvars
  | null notDeclVar
    = parseQualsWorker varsF False rest nPos (dvars ++ nDVars) []
  | varsF
    = do
        let nUvars = removeElements uvars dvars
        parseQualsWorker varsF False rest nPos (dvars ++ nDVars) nUvars
  | topElem "<-" qual
    = do
        setInput qual
        (pat, expr) <- splitByTopLastSign "<-"
        throwNotDeclaredError (pos + length pat + 2) (head notDeclVar) expr
        return ([], [])
  | otherwise
    = do
        throwNotDeclaredError pos (head notDeclVar) qual
        return ([], [])
  where
    nPos = pos + length qual + 1
    notDeclVar = containsList uvars dvars

parseGenerator :: Int
               -> GenParser Char st ([String], [String])
parseGenerator pos = do
  (l, r) <- splitByTopLastSign "<-"

  setInput l
  sourcePos <- getPosition
  let nSourcePos = setSourceColumn sourcePos pos
  setPosition nSourcePos
  declVars <- parsePat

  let eqEle = hasEqualElements declVars
  if not $ null eqEle
    then do
      let signLst = [' ', ':', '[', ']', '(', ')', ',']
      let firstPos = getStringPos (head eqEle) l signLst
      let varLen = length $ head eqEle
      let txtWithoutFirst = drop (firstPos + varLen - 1) l
      let secondPos = getStringPos (head eqEle) txtWithoutFirst signLst
      let varPos = pos + (firstPos + varLen - 1) + secondPos - 1
      fail $ e_VAR_ALREADY_DECLARED ++ show (varPos, varPos + varLen - 1)
    else do
      setInput r
      let nSourcePos = setSourceColumn sourcePos $ pos + length l + 2
      setPosition nSourcePos
      usedVars <- parseExpr

      return (declVars, usedVars)

throwNotDeclaredError :: Int
                      -> String
                      -> String
                      -> GenParser Char st ()
throwNotDeclaredError pos notDeclVar input = do
  let varLen = length notDeclVar
  let signLst = [' ', ':', '[', ']', '(', ')', ',', '<', '-', '`', '!'
               , '*', '/', '+', '-', '^', '+', '&', '|', '$', '=', '>'
               , '.']
  let varPos = pos + getStringPos notDeclVar input signLst - 1
  fail $ e_VAR_NOT_DECLARED ++ show (varPos, varPos + varLen - 1)

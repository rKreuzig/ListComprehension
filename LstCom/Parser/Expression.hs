-- |
module LstCom.Parser.Expression (
-- * Functions
  parseExpression, parseExpr
) where

import Prelude
import Data.List (isPrefixOf)
import Text.ParserCombinators.Parsec
import Data.Char (generalCategory, GeneralCategory (..), isPunctuation, isSymbol)

import LstCom.Utils
import LstCom.Constants
import LstCom.Parser.Errors
import LstCom.Parser.General
import LstCom.Parser.Helpers
import {-# SOURCE #-} LstCom.Parser.ListComprehension

parseExpression :: String
                -> Either ParseError [String]
parseExpression input = parse parseExpr "" input

parseExpr :: GenParser Char st [String]
parseExpr = do
  notClosedError
  expr 0

expr :: Int
     -> GenParser Char st [String]
expr secMinus = do
  noInputError
  spaces

  input <- getInput
  let secF = secMinus < 0
  let ifF = isPrefixOf "if" input
  case head input of
    _ | secF -> oexpr secMinus
    '-'      -> minus
    _ | ifF  -> ifThenElse
    _        -> oexpr 0

minus :: GenParser Char st [String]
minus = do
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  char '-'
  spaces

  isEnd <- end
  if isEnd
    then
      fail $ e_MINUS_WITHOUT_INPUT ++ show (pos, pos)
    else
      oexpr 0

ifThenElse :: GenParser Char st [String]
ifThenElse = do
  sourcePos <- getPosition
  let startPos = sourceColumn sourcePos
  let ifPos = show (startPos, startPos + 1)

  string "if"

  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  lst <- splitByWords
  (ifExpr, lstRest) <- ifThenElseSplit ifPos "then" lst 1 ""
  (thenExpr, elseRest) <- ifThenElseSplit ifPos "else" lstRest 1 ""
  let elseExpr = concat elseRest

  let thenStart = startPos + 1 + length ifExpr
  let thenPos = show (thenStart, thenStart + 4)
  let elseStart = thenStart + 4 + length thenExpr
  let elsePos = show (elseStart, elseStart + 4)

  case (trim ifExpr, trim thenExpr, trim elseExpr) of
    ("", _, _) -> fail $ e_IF_WITHOUT_EXPR ++ ifPos
    (_, "", _) -> fail $ e_THEN_WITHOUT_EXPR ++ thenPos
    (_, _, "") -> fail $ e_ELSE_WITHOUT_EXPR ++ elsePos
    _          -> do
                    setPosition sourcePos
                    setInput ifExpr
                    ifLst <- expr 0

                    let nPos = pos + length ifExpr + 4
                    setPosition $ setSourceColumn sourcePos nPos
                    setInput thenExpr
                    thenLst <- expr 0

                    let nPos2 = nPos + length thenExpr + 4
                    setPosition $ setSourceColumn sourcePos nPos2
                    setInput elseExpr
                    elseLst <- expr 0

                    return $ ifLst ++ thenLst ++ elseLst

ifThenElseSplit :: String
           -> String
           -> [String]
           -> Int
           -> String
           -> GenParser Char st (String, [String])
ifThenElseSplit ifPos _ [] _ _ =
  fail $ e_IF_WITHOUT_THEN_ELSE ++ ifPos
ifThenElseSplit ifPos word (ele:rest) count buffer
  | tEle == "if"
    = ifThenElseSplit ifPos word rest (count + 1) (buffer ++ ele)
  | tEle == word
    = let nCount = count - 1
      in if nCount == 0
        then do
          let rightSpaces = dropWhile (/= ' ') $ dropWhile (== ' ') ele
          return (buffer, [rightSpaces] ++ rest)
        else
          ifThenElseSplit ifPos word rest nCount (buffer ++ ele)
  | otherwise
    = ifThenElseSplit ifPos word rest count (buffer ++ ele)
  where
    tEle = trim ele

oexpr :: Int
      -> GenParser Char st [String]
oexpr secMinus = do
  input <- getInput
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  (left, func, right) <- splitByTopFirstOperator

  let funcStart = pos + length left
  let funcEnd = funcStart + getFuncLen func - 1
  let funcPos = show (funcStart, funcEnd)

  setPosition sourcePos

  let secF = secMinus < 0
  case (trim left, func, trim right) of
    ("", "", "")        -> setInput input >> lexpr secMinus
    ("", "-", _) | secF -> fail $ e_FUNCTION_NOT_ALLOWED_AS_LEFT_SEC ++ funcPos
    (l, _, r) | secF    -> sec funcPos secMinus left func right
    ("", _, _)          -> fail $ e_FUNCTION_NOT_ENOUGH_ARGS ++ funcPos
    (_, _, "")          -> fail $ e_FUNCTION_NOT_ENOUGH_ARGS ++ funcPos
    (l, _, r)           -> op left func right

sec :: String
    -> Int
    -> String
    -> String
    -> String
    -> GenParser Char st [String]
sec funcPos secMinus left func right
  | sub > 0
    = fail $ e_FUNCTION_NOT_ENOUGH_ARGS ++ funcPos
  | sub < 0
    = fail $ e_FUNCTION_TOO_MUCH_ARGS ++ funcPos
  | leftFlag
    = do
        setInput left
        aexpr 0
  | rightFlag
    = do
        sourcePos <- getPosition
        let pos = sourceColumn sourcePos
        let nPos = pos + length left + getFuncLen func
        setPosition $ setSourceColumn sourcePos nPos

        setInput right
        aexpr 0
  | otherwise
    = return []
  where
    leftFlag = if null $ trim left then False else True
    rightFlag = if null $ trim right then False else True
    argNum = if leftFlag && rightFlag
              then 2
              else if leftFlag || rightFlag then 1 else 0
    sub = 2 + secMinus - argNum

op :: String
   -> String
   -> String
   -> GenParser Char st [String]
op left func right = do
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  let secMinus = if func == "$" then -1 else 0
  setInput left
  leftExpr <- lexpr secMinus

  let nPos = pos + length left + getFuncLen func
  setPosition $ setSourceColumn sourcePos nPos
  setInput right
  rightExpr <- oexpr 0

  return $ leftExpr ++ rightExpr

lexpr :: Int
      -> GenParser Char st [String]
lexpr secMinus = do
  spaces
  input <- getInput
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  lst' <- splitByWords
  let lst = if length lst' > 1 && (null . trim $ head lst')
              then
                [head lst' ++ (head . tail $ lst')] ++ (tail . tail $ lst')
              else
                lst'

  let secFlag = secMinus < 0
  case True of
    _ | secFlag        -> fexpr secMinus lst pos
    _ | length lst > 1 -> fexpr secMinus lst pos
    _                  -> setInput input >> setPosition sourcePos >> aexpr 0

fexpr :: Int
      -> [String]
      -> Int
      -> GenParser Char st [String]
fexpr secMinus lst pos
  | func `elem` c_REJECTED_PRELUDE_FUNCS
    = fail $ e_FUNCTION_NOT_ALLOWED ++ funcPos
  | func `notElem` c_PRELUDE_FUNCS_NAMES, secFlag
    = fail $ e_FUNCTION_NEEDS_FUNC_AS_ARG ++ funcPos
  | func `notElem` c_PRELUDE_FUNCS_NAMES
    = fail $ e_UNKNOWN_FUNCTION ++ funcPos
  | sub > 0
    = fail $ e_FUNCTION_NOT_ENOUGH_ARGS ++ funcPos
  | sub < 0
    = fail $ e_FUNCTION_TOO_MUCH_ARGS ++ funcPos
  | func `elem` c_FUNCS_WITH_1_FUNC_ARG_NAMES
    = let minus = getFuncArgMinusNum func
      in fele [minus] nPos $ tail lst
  | func `elem` c_FUNCS_WITH_2_FUNC_ARGS_NAMES
    = fele [-1, -1] nPos $ tail lst
  | otherwise
    = fele [] nPos $ tail lst
  where
    rightSpaces = takeWhile (== ' ') $ reverse $ head lst
    func = trim $ head lst
    funcEnd = pos + length func - 1
    funcPos = show (pos, funcEnd)
    nPos = funcEnd + length rightSpaces + 1
    secFlag = secMinus < 0
    funcArgs = getFuncArgNum func
    sub = funcArgs + secMinus - (length $ tail lst)

fele :: [Int]
     -> Int
     -> [String]
     -> GenParser Char st [String]
fele _ _ [] = return []
fele [] pos (ele:eles) = fele [0] pos (ele:eles)
fele (minus:rest) pos (ele:eles) = feleWorker (minus:rest) pos (ele:eles)

feleWorker :: [Int]
           -> Int
           -> [String]
           -> GenParser Char st [String]
feleWorker (minus:rest) pos (ele:eles) = do
  setInput ele
  sourcePos <- getPosition
  let curPos = setSourceColumn sourcePos pos
  setPosition curPos
  let nPos = pos + length ele

  lst <- aexpr minus
  rest <- fele rest nPos eles
  return $ lst ++ rest

aexpr :: Int
      -> GenParser Char st [String]
aexpr secMinus = do
  input <- getInput
  let trimInput = trim input
  let lastSign = last trimInput
  let noBraIn = tail . init $ trimInput

  case head trimInput of
    '[' | lastSign /= ']'      -> outOfTplLstErrorRight input ']' >> return []
    '(' | lastSign /= ')'      -> outOfTplLstErrorRight input ')' >> return []
    '[' | topElem "|" noBraIn  -> parseLstCom True
    '[' | topElem ".." noBraIn -> arithSeq noBraIn
    '['                        -> parTplLst (expr 0) '[' ']'
    '('                        -> parTplLst (expr secMinus) '(' ')'
    '"'                        -> litVar
    '\''                       -> litVar
    _ | elem '(' trimInput     -> outOfTplLstErrorLeft input >> return []
    _ | elem '[' trimInput     -> outOfTplLstErrorLeft input >> return []
    _ | secMinus < 0           -> expr secMinus
    _ | trimInput == "True"    -> do {spaces; string "True"; spaces; return []}
    _ | trimInput == "False"   -> do {spaces; string "False"; spaces; return []}
    _                          -> litVar

arithSeq :: String
         -> GenParser Char st [String]
arithSeq noBraInput = do
  spaces
  sourcePos <- getPosition
  let start = sourceColumn sourcePos
  let len = length noBraInput + 1
  let posTpl = show (start, start + len)

  setInput noBraInput
  (left, right) <- splitByTopLastSign ".."
  case (trim left, trim right) of
    ("", _) -> fail $ e_ARITH_SEQ_NO_START ++ posTpl
    (_, "") -> fail $ e_ARITH_SEQ_NO_END ++ posTpl
    (_, _)  -> do
                setInput left
                setPosition $ setSourceColumn sourcePos $ start + 1
                leftEles <- splitByTopSign ","
                if length leftEles > 2
                  then
                    fail $ e_ARITH_SEQ_TOO_MUCH_ARGS ++ posTpl
                  else do
                    setInput $ "[" ++ left ++ "]"
                    setPosition sourcePos
                    leftExpr <- parTplLst (expr 0) '[' ']'

                    setInput right
                    let nPos = start + length left + 3
                    setPosition $ setSourceColumn sourcePos nPos
                    rightExpr <- expr 0

                    return $ leftExpr ++ rightExpr

litVar :: GenParser Char st [String]
litVar = do
  spaces
  input <- getInput
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos

  let headSign = head input
  let cat = generalCategory headSign
  case headSign of
    _ | cat == DecimalNumber -> numLit >> return []
    '.'                      -> fail $ e_POINT_WITHOUT_NUM ++ show (pos, pos)
    '\''                     -> charLit >> return []
    '"'                      -> stringLit >> return []
    otherwise                -> do {name <- var; return [name]}

numLit :: GenParser Char st ()
numLit = do
  let satFunc = satisfy (\x -> generalCategory x == DecimalNumber)
  many1 satFunc

  sourcePos <- getPosition
  let pos = sourceColumn sourcePos
  let tpl = show (pos, pos)
  optional $ char '.' >> (many1 satFunc <|> fail (e_POINT_WITHOUT_NUM ++ tpl))

  spaces
  stop <- end
  if not stop
    then do
      sourcePos <- getPosition
      let pos = sourceColumn sourcePos
      fail $ e_NUM_SIGN_NOT_ALLOWED ++ show (pos, pos)
    else
      return ()

charLit :: GenParser Char st ()
charLit = do
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos
  input <- getInput
  let charTpl = show (pos, pos + length input - 1)
  let errorMsg = e_INVALID_CHAR ++ charTpl

  char '\'' <|> fail errorMsg
  let charFunc = (\x -> x /= '\'' && stringChar x)
  satisfy charFunc <|> fail errorMsg
  char '\'' <|> fail errorMsg

  spaces
  stop <- end
  if not stop
    then do
      sourcePos <- getPosition
      let pos = sourceColumn sourcePos
      input <- getInput
      fail $ e_INPUT_OUT_OF_CHAR ++ show (pos, pos + length input - 1)
    else
      return ()

stringLit :: GenParser Char st ()
stringLit = do
  sourcePos <- getPosition
  let pos = sourceColumn sourcePos
  input <- getInput
  let stringTpl = show (pos, pos + length input - 1)
  let errorMsg = e_INVALID_STRING ++ stringTpl

  char '"' <|> fail errorMsg
  let stringFunc = (\x -> x /= '"' && stringChar x)
  many (satisfy stringFunc) <|> fail errorMsg
  char '"' <|> fail errorMsg

  spaces
  stop <- end
  if not stop
    then do
      sourcePos <- getPosition
      let pos = sourceColumn sourcePos
      input <- getInput
      fail $ e_INPUT_OUT_OF_STRING ++ show (pos, pos + length input - 1)
    else
      return ()

stringChar :: Char
           -> Bool
stringChar x
  | graphic x || x == ' '
    = True
  | otherwise
    = False

graphic :: Char
        -> Bool
graphic x
  | x `elem` ['\'', '"', '_']
    = True
  | cat == LowercaseLetter || cat == UppercaseLetter || cat == TitlecaseLetter
    = True
  | cat == DecimalNumber || isPunctuation x || isSymbol x
    = True
  | otherwise
    = False
  where
    cat = generalCategory x

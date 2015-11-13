-- |
module LstCom.Utils (
-- * Functions
  trim, trimL, trimR, getFuncArgNum, getFuncLen, getFuncArgMinusNum,
  containsList, hasEqualElements, removeElements, getStringPos
) where

import Prelude
import LstCom.Constants
import Data.Char (isSpace)

trim :: String
     -> String
trim input = trimL . trimR $ input

trimL :: String
      -> String
trimL input = dropWhile isSpace input

trimR :: String
      -> String
trimR input = reverse . trimL . reverse $ input

getStringPos :: String
             -> String
             -> [Char]
             -> Int
getStringPos string input signLst = getStringPosWorker string input signLst "" 1

getStringPosWorker :: String
                   -> String
                   -> [Char]
                   -> String
                   -> Int
                   -> Int
getStringPosWorker string [] _ buffer pos
  | buffer == string
    = pos
  | otherwise
    = -1
getStringPosWorker string (sign:rest) signLst buffer pos
  | sign `elem` signLst, buffer == string
    = pos
  | sign `elem` signLst
    = getStringPosWorker string rest signLst "" $ pos + length buffer + 1
  | sign `elem` ['"', '\'']
    = let dropLen = 1 + (length $ takeWhile (/= sign) rest)
          nRest = drop dropLen rest
          nPos = pos + length buffer + dropLen + 1
      in getStringPosWorker string nRest signLst "" $ nPos
  | otherwise
    = getStringPosWorker string rest signLst (buffer ++ [sign]) pos

hasEqualElements :: Eq a => [a]
                 -> [a]
hasEqualElements [] = []
hasEqualElements (x:lst)
  | x `elem` lst
    = [x]
  | otherwise
    = hasEqualElements lst

containsList :: Eq a => [a]
             -> [a]
             -> [a]
containsList [] _ = []
containsList (x:lst1) lst2
  | x `elem` lst2
    = containsList lst1 lst2
  | otherwise
    = [x]

removeElements :: Eq a => [a]
               -> [a]
               -> [a]
removeElements lst1 lst2 =
  let f = \lst ele -> if ele `elem` lst2 then lst else lst ++ [ele]
      nlst = foldl f [] lst1
  in nlst

getFuncArgNum :: String
              -> Int
getFuncArgNum input =
  let foldFunc = \(name, num) (func, args) -> if func == name
                                                then (name, args)
                                                else (name, num)
      (_, argNum) = foldl foldFunc (input, -1) c_PRELUDE_FUNCS
  in argNum

getFuncArgMinusNum :: String
                   -> Int
getFuncArgMinusNum input =
  let foldFunc = \(name, num) (func, args) -> if func == name
                                                then (name, args)
                                                else (name, num)
      (_, argNum) = foldl foldFunc (input, 0) c_FUNCS_WITH_1_FUNC_ARG
  in argNum

getFuncLen :: String
           -> Int
getFuncLen input =
  if input `elem` c_SYMBOL_FUNCS
    then length input
    else length input + 2

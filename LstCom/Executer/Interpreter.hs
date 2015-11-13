-- | Module to execute prelude expressions with variables
module LstCom.Executer.Interpreter (
-- * Types
  ServerHandle,
-- * Functions
  evalExpr, evalExpr'
) where

import Prelude
import Data.List (isPrefixOf)
import Data.Map (deleteFindMin, empty)

import Control.Monad.Trans.Either
import Language.Haskell.Interpreter (eval)
import Language.Haskell.Interpreter.Server (ServerHandle, start, stop, runIn)

import LstCom.Data
import LstCom.Utils
import LstCom.Constants

-- | Function to evaluate a prelude expression with variables.
--   Returns an error message or the evaluated expression.
evalExpr' :: ServerHandle
          -> Scope -- ^ current variables
          -> String -- ^ expression to execute
          -> IO (Either String String)
evalExpr' handle scope expr = runEitherT $ evalExpr handle scope expr

-- | Function to evaluate a prelude expression with variables.
--   Returns an error message or the evaluated expression.
evalExpr :: ServerHandle
         -> Scope -- ^ current variables
         -> String -- ^ expression to execute
         -> EitherT String IO String
evalExpr handle scope expr
  | trim expr == "[]"
    = EitherT $ return $ Right "[]"
  | otherwise
    = do
        let nExpr = addVariables scope expr
        EitherT $ runCode handle expr nExpr

-- | Function adds variables of the scope with let statements to the expression.
--   Returns the expression with variable declarations of the scope.
addVariables :: Scope -- ^ current variables
             -> String -- ^ expression to execute
             -> String
addVariables scope expr
  | scope == empty
    = expr
  | null vars || null val
    = addVariables nScope expr
  | otherwise
    = let letExpr = " let " ++ vars ++ "=" ++ val ++ " in "
      in letExpr ++ addVariables nScope expr
  where
    ((_, (vars, val)), nScope) = deleteFindMin scope

-- | Function to evaluate a prelude expression.
--   Returns an error or the evaluated expression.
runCode :: ServerHandle
        -> String -- ^ expression to execute
        -> String -- ^ expression with scope variables
        -> IO (Either String String)
runCode handle oldCode code = do
  evalList <- isList
  if evalList
    then do
      let maxElements = show $ c_MAX_LIST_ELEMENTS + 1
          preCode = "let x = take " ++ maxElements ++ " "
          postCode = " in if length x < " ++ maxElements ++ "then Just x else Nothing"
          code' = preCode ++ code ++ postCode
      res <- runIn handle $ eval code'
      case res of
        Left l  -> return $ Left $ searchError handle oldCode
        Right r -> if isPrefixOf "Just " r
                    then return $ Right $ drop 5 r
                    else return $ Left c_MAX_LIST_ELEMENTS_TXT
    else do
      res <- runIn handle $ eval code
      case res of
        Left l  -> return $ Left $ searchError handle oldCode
        Right r -> return $ Right r
  where
    isList = do
      let idFunc = "let myId :: [a] -> Int; myId x = 1 in myId "
          code' = idFunc ++ code
      res <- runIn handle $ eval code'
      case res of
        Left _  -> return False
        Right _ -> return True

-- | Function to search a error in the expression.
--   Returns a better error message.
searchError :: ServerHandle
            -> String -- ^ expression to execute
            -> String
searchError handle code =
  let modErrorMsg = init . init $ e_CANNOT_EXECUTE
  in modErrorMsg ++ ": " ++ code ++ ": " ++ show (0, 0)

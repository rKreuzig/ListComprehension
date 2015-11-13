-- |
module LstCom.Executer.Helpers (
-- * Types
  ServerHandle,
-- * Functions
  hasFalseGuard, evalLeftExpr, updateGenerator, updateGuard, addStep,
  updateGeneratorWithPatMatch
) where

import Prelude hiding (lookup)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Map (filterWithKey, empty, delete, alter, findWithDefault, lookup)

import LstCom.Data
import LstCom.Utils
import LstCom.Constants
import LstCom.Parser.General
import LstCom.Executer.Interpreter

-- |
hasFalseGuard :: Scope -- ^
              -> [Qualifier] -- ^
              -> Bool -- ^
hasFalseGuard _ [] = False
hasFalseGuard scope (qual@(Qualifier _ qType qPos qVars _):quals)
  | qType == Guard
    = let (_, value) = findWithDefault ("", "True") qPos scope
      in if "True" == value
        then hasFalseGuard scope quals
        else True
  | qType == Generator
    = let mValue = lookup qPos scope
      in case mValue of
        Just (_, []) -> True
        _       -> hasFalseGuard scope quals

-- | Function to evaluate a list element of the List Comprehension.
--   Returns an error message or the step list with the new step and
--   the List Comprehension result with the new result.
evalLeftExpr :: ServerHandle
             -> String -- ^ the expression to evaluate
             -> Scope -- ^ current variables
             -> [Step] -- ^ current steps
             -> [String] -- ^ current List Comprehension result
             -> EitherT String IO ([Step], [String])
evalLeftExpr handle expr scope steps lstComResult = do
  res <- evalExpr handle scope expr
  let step1 = (c_LST_COM_EXPR_NAME, res)
  let step2 = (c_COMMENT, c_COM_NEW_LST_COM_EXPR ++ res)
  let nSteps = steps ++ [step1, step2]
  let nLstComResult = lstComResult ++ [res]
  return (nSteps, nLstComResult)

updateGeneratorWithPatMatch :: ServerHandle
                            -> Qualifier -- ^ generator to update
                            -> Scope -- ^ current variables
                            -> EitherT String IO (Scope, Qualifier, String)
updateGeneratorWithPatMatch handle qual scope
  = updateGeneratorWorker handle qual scope True

-- | Function to update a generator.
--   Returns an error message or the variable scope with the new declarations,
--   the first element of the generator list and the updated generator.
updateGenerator :: ServerHandle
                -> Qualifier -- ^ generator to update
                -> Scope -- ^ current variables
                -> EitherT String IO (Scope, Qualifier, String)
updateGenerator handle qual scope
  = updateGeneratorWorker handle qual scope False

updateGeneratorWorker :: ServerHandle
                      -> Qualifier -- ^ generator to update
                      -> Scope -- ^ current variables
                      -> Bool
                      -> EitherT String IO (Scope, Qualifier, String)
updateGeneratorWorker handle qual@(Qualifier qExpr _ qPos qVars qGenLst) scope initFlag
  | null qGenLst
    = do
        res <- evalExpr handle scope qExpr
        let eLst = getList res

        let start = qPos + length qVars + 2
        let tpl = show (start, start + length qExpr - 1)

        case eLst of
          Right lst -> upd lst
          _         -> EitherT $ return $ Left $ e_WRONG_GENERATOR ++ tpl
  | otherwise
    = upd qGenLst
  where
    checkPattern value
      | initFlag
        = do
          let posTpl = show (qPos, qPos + length qVars - 1)
          let expr = "let " ++ qVars ++ "=" ++ value ++ " in 1"
          patternCheck <- liftIO $ evalExpr' handle empty expr
          case patternCheck of
            Right _ -> EitherT $ return $ Right $ ""
            _       -> EitherT $ return $ Left $ e_WRONG_PATTERN ++ posTpl
      | otherwise
        = EitherT $ return $ Right $ ""

    updateScope value =
      let nScope = filterWithKey (\k _ -> k <= qPos) scope
      in alter (\_ -> Just (qVars, value)) qPos nScope

    upd lst
      | null lst, trim qExpr == "[]"
        = let nScope = updateScope []
          in return (nScope, qual{qualGenLst = []}, [])
      | null lst
        = do
            checkPattern "()"
            let nScope = updateScope []
            return (nScope, qual{qualGenLst = []}, [])
      | otherwise
        = do
            let value = head lst
            checkPattern value
            let lstRest = tail lst
                nScope = updateScope value
            return (nScope, qual{qualGenLst = lstRest}, value)

-- | Function to update a guard.
--   Returns an error message or the variable scope with the new guard result
--   and the result of the guard.
updateGuard :: ServerHandle
            -> Qualifier -- ^ guard to update
            -> Scope -- ^ current variables
            -> EitherT String IO (Scope, String)
updateGuard handle qual@(Qualifier qExpr _ qPos _ _) scope = do
  res <- evalExpr handle scope qExpr
  if res `elem` ["True", "False"]
    then do
      let nScope = alter (\_ -> Just ("", res)) qPos scope
      return (nScope, res)
    else
      let tpl = show (qPos, qPos + length qExpr - 1)
      in EitherT $ return $ Left $ e_WRONG_GUARD ++ tpl

-- | Function to add a qualifier step to the step list.
--   Guards use the expression as name, generators the pattern.
--   Returns the step list with the new step.
addStep :: Qualifier -- ^ qualifier of the step
        -> String -- ^ result of the step
        -> [Step] -- ^ current step list
        -> [Step]
addStep (Qualifier qExpr qType qPos qVars _) res steps =
  steps ++ [(show qPos, getRes), getComment]
  where
    getRes
      | qType == Generator && null res
        = "False"
      | otherwise
        = res
    name = case qType of Guard -> qExpr; _ -> qVars
    getComment
      | res == c_NO_RESULT
        = (c_COMMENT, name ++ c_COM_DONT_EVAL_EXPR)
      | otherwise
        = (c_COMMENT, c_COM_EVAL_EXPR_1 ++ name ++ c_COM_EVAL_EXPR_2 ++ getRes)

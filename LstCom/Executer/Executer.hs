-- | Module to get steps and result of a List Comprehension
module LstCom.Executer.Executer (
-- * Types
  ServerHandle,
-- * Functions
  getLstComStepsAndResult
) where

import Prelude
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Data.Map (insert, member, empty)

import LstCom.Data
import LstCom.Constants
import LstCom.Executer.Helpers

-- |
getLstComStepsAndResult :: ServerHandle -- ^
                        -> String         -- ^
                        -> [Qualifier]         -- ^
                        -> EitherT String IO ([Step], [String])
getLstComStepsAndResult handle leftExpr quals = do
  initRes <- initQuals handle leftExpr quals [] empty False 0
  let (iLstComResult, iSteps, iScope, iQuals, iStepCount) = initRes
  executeSteps handle leftExpr iLstComResult iSteps iScope iQuals iStepCount

-- | Function iterates over all qualifiers from left to right.
--   Returns an error message or the first result of the left expression, a list
--   with the current steps, a scope with the current variable declarations and
--   the qualifiers with updated generator lists.
initQuals :: ServerHandle
          -> String -- ^ left expression of the List Comprehension
          -> [Qualifier] -- ^ current qualifiers
          -> [Step] -- ^ current steps
          -> Scope -- ^ current variables
          -> Bool -- ^ True if a guard is False
          -> Int
          -> EitherT String IO ([String], [Step], Scope, [Qualifier], Int)
initQuals handle leftExpr [] steps scope guaFalseFlag stepCount
  | guaFalseFlag
    = do
        let step1 = (c_LST_COM_EXPR_NAME, c_NO_RESULT)
            step2 = (c_COMMENT, c_COM_NO_LST_COM_EXPR)
            nSteps = steps ++ [step1, step2]
        return ([], nSteps, scope, [], stepCount + 1)
  | otherwise
    = do
        (nSteps, res) <- evalLeftExpr handle leftExpr scope steps []
        return (res, nSteps, scope, [], stepCount + 1)

initQuals handle leftExpr qualifiers@(qual:quals) steps scope guaFalseFlag stepCount
  | guaFalseFlag
    = rec scope qual c_NO_RESULT guaFalseFlag
  | qualType qual == Generator
    = do
        (nScope, nQual, res) <- updateGeneratorWithPatMatch handle qual scope
        case res of
          "" -> rec nScope nQual res True
          _  -> rec nScope nQual res False
  | qualType qual == Guard
    = do
        (nScope, res) <- updateGuard handle qual scope
        case res of
          "False" -> rec nScope qual res True
          _       -> rec nScope qual res False
  where
    rec recScope recQual recRes recGuaFalseFlag = do
      let nSteps = addStep recQual recRes steps
      rest <- initQuals handle leftExpr quals nSteps recScope recGuaFalseFlag (stepCount + 1)
      let (rRes, rSteps, rScope, rQuals, rStepCount) = rest
      return (rRes, rSteps, rScope, rQuals ++ [recQual], rStepCount)

-- |
executeSteps :: ServerHandle -- ^
             -> String         -- ^
             -> [String] -- ^
             -> [Step]            -- ^
             -> Scope              -- ^
             -> [Qualifier]         -- ^
             -> Int
             -> EitherT String IO ([Step], [String]) -- ^
executeSteps handle leftExpr lstComResult steps scope quals stepCount = do
  (nSteps, nScope, nQuals, nStepCount) <- nextQualSteps handle steps scope quals stepCount
  rec nSteps nScope nQuals nStepCount
  where
    rec nSteps nScope nQuals nStepCount
      | member (-999) nScope
        = return (nSteps, lstComResult)
      | not $ hasFalseGuard nScope nQuals
        = do
            res <- evalLeftExpr handle leftExpr nScope nSteps lstComResult
            let (nSteps', nLstComResult) = res
            executeSteps handle leftExpr nLstComResult nSteps' nScope nQuals (nStepCount + 1)
      | otherwise
        = let step1 = (c_LST_COM_EXPR_NAME, c_NO_RESULT)
              step2 = (c_COMMENT, c_COM_NO_LST_COM_EXPR)
              nSteps' = nSteps ++ [step1, step2]
          in executeSteps handle leftExpr lstComResult nSteps' nScope nQuals (nStepCount + 1)

-- |
nextQualSteps :: ServerHandle -- ^
              -> [Step] -- ^
              -> Scope -- ^
              -> [Qualifier] -- ^
              -> Int
              -> EitherT String IO ([Step], Scope, [Qualifier], Int) -- ^
nextQualSteps _ steps scope [] _ = do
  let nScope = insert (-999) ("", "") scope
  return (steps, nScope, [], 0)

nextQualSteps handle steps scope (qual:quals) stepCount
  | stepCount >= c_MAX_STEPS
    = EitherT $ return $ Left c_MAX_STEPS_TXT
  | (qType == Generator && null (qualGenLst qual)) || qType == Guard
    = do
        (pSteps, pScope, pQuals, pStepCount) <- nextQualSteps handle steps scope quals stepCount
        rec pSteps pScope pQuals pStepCount
  | qType == Generator
    = do
        (uScope, uQual, uRes) <- updateGenerator handle qual scope
        let nSteps = addStep uQual uRes steps
        return (nSteps, uScope, [uQual] ++ quals, stepCount + 1)
  where
    qType = qualType qual
    getUpdate pScope
      | qType == Generator
        = updateGenerator handle qual pScope
      | otherwise
        = do
            (s, r) <- updateGuard handle qual pScope
            return (s, qual, r)
    rec pSteps pScope pQuals pStepCount
      | member (-999) pScope
        = return (pSteps, pScope, pQuals, 0)
      | pStepCount >= c_MAX_STEPS
        = EitherT $ return $ Left c_MAX_STEPS_TXT
      | hasFalseGuard pScope pQuals
        = do
            let nSteps = addStep qual c_NO_RESULT pSteps
            return (nSteps, pScope, [qual] ++ pQuals, pStepCount + 1)
      | otherwise
        = do
            (uScope, uQual, uRes) <- getUpdate pScope
            let nSteps = addStep uQual uRes pSteps
            return (nSteps, uScope, [uQual] ++ pQuals, pStepCount + 1)

module Handler.LstCom where

import Import hiding (unpack, null, last)
import Database.Persist.Sql (toSqlKey)

import Data.Map (empty)
import Data.Text (unpack)
import System.Random (randomRIO)
import Prelude ((!!), null, tail, init, read, last)

import LstCom.LstCom
import LstCom.Executer.Interpreter
import DatabaseConnection

c_TITLE :: Html
c_TITLE = "ListComprehension"

c_WRONG_EXPECTED_RESULT :: String
c_WRONG_EXPECTED_RESULT = "Die vorgefertigte Lösung kann nicht ausgewertet werden"

getLstComResult :: (String -> String -> String -> [Qualifier] -> String
                    -> Handler Html)
                -> (String -> String -> HandlerT App IO Html)
                -> Handler Html
getLstComResult successFunc failFunc = do
  app <- getYesod
  let handle = serverHandle app

  lstComTxt <- runInputPost $ ireq textField "lstComInput"
  let lstCom = unpack lstComTxt

  eResult <- liftIO $ getLstComData handle lstCom
  case eResult of
    Left result -> failFunc result lstCom
    Right result -> do
                      let (lstComRes, leftExpr, quals, json) = result
                      successFunc lstCom leftExpr lstComRes quals json

getErrorText :: String
             -> String
             -> (String, String, String, String)
getErrorText errorTxt lstCom
  | isPrefixOf "Fehler" errorTxt
    = let revTxt = reverse errorTxt
          nErrorTxt = init . reverse $ dropWhile (/= ':') revTxt
          tplTxt = tail . reverse $ takeWhile (/= ':') revTxt

          (startPos, endPos) = read tplTxt :: (Int, Int)
          left = take (startPos - 1) lstCom
          red = drop (startPos - 1) $ take endPos lstCom
          right = drop endPos lstCom
      in (nErrorTxt, left, red, right)
  | otherwise
    = (errorTxt, lstCom, "", "")

getLstComR :: Handler Html
getLstComR = loadLayoutWithoutParams

loadLayoutWithoutParams :: Handler Html
loadLayoutWithoutParams =
  let lstCom = "" :: String
      result = "" :: String
  in loadLayoutWithResultAndLstCom result lstCom

postLstComResultWithoutExerciseR :: Handler Html
postLstComResultWithoutExerciseR =
  getLstComResult loadLayout loadLayoutWithResultAndLstCom

loadLayoutWithResultAndLstCom :: String
                              -> String
                              -> Handler Html
loadLayoutWithResultAndLstCom result lstCom =
  let leftExpr = "" :: String
      quals = []
      json = ""  :: String
  in loadLayout lstCom leftExpr result quals json

loadLayout :: String
           -> String
           -> String
           -> [Qualifier]
           -> String
           -> Handler Html
loadLayout lstCom leftExpr pResult quals json = do
  let (result, leftResult, redResult, rightResult) = getErrorText pResult lstCom
      exeName = "" :: String
      exeTask = "" :: String
      expectedResult = "" :: String
      boxes = [] :: [String]
      exerciseId = toSqlKey 0 :: ExerciseId

  exercises <- getActiveExercises
  defaultLayout $ do
    setTitle c_TITLE
    $(widgetFile "lstcom")

getLstComExerciseR :: ExerciseId -> Handler Html
getLstComExerciseR exerciseId = do
  mExercise <- getExercise exerciseId
  case mExercise of
    Just exercise -> loadExerciseLayout exerciseId exercise
    _             -> loadLayoutWithoutParams

loadExerciseLayout :: ExerciseId
                   -> Exercise
                   -> Handler Html
loadExerciseLayout exerciseId exercise = do
  initExerciseResult (loadExerciseLayoutWorker exerciseId) exercise

initExerciseResult :: (Exercise -> String -> [String] -> Handler Html)
                   -> Exercise
                   -> Handler Html
initExerciseResult layoutFunc exercise = do
  expectedResult <- getExpectedResult exercise
  boxes <- liftIO $ mixExerciseBoxes $ exerciseBoxes exercise
  layoutFunc exercise expectedResult boxes

getExpectedResult :: Exercise
                  -> Handler String
getExpectedResult exercise = do
  app <- getYesod
  let handle = serverHandle app

  let solLstCom = unpack . exerciseSolution $ exercise
  eExpectedResult <- liftIO $ evalExpr' handle empty solLstCom
  case eExpectedResult of
    Left l  -> return c_WRONG_EXPECTED_RESULT
    Right r -> return r

mixExerciseBoxes :: [Text]
                 -> IO [String]
mixExerciseBoxes boxes
  | maxRange == 0
    = return []
  | otherwise
    = do
        num <- randomRIO (1, maxRange)
        rest <- mixExerciseBoxes $ take (num - 1) boxes ++ drop num boxes
        return $ [unpack $ boxes !! (num - 1)] ++ rest
  where
    maxRange = length boxes

loadExerciseLayoutWorker :: ExerciseId
                         -> Exercise
                         -> String
                         -> [String]
                         -> Handler Html
loadExerciseLayoutWorker exerciseId exercise expectedResult boxes = do
  let lstCom = "" :: String
      json = "" :: String
      leftExpr = "" :: String
      quals = []
      result = "" :: String
      leftResult = "" :: String
      redResult = "" :: String
      rightResult = "" :: String
      exeName = unpack $ exerciseName exercise
      exeTask = unpack $ exerciseTask exercise

  exercises <- getActiveExercises
  defaultLayout $ do
    setTitle c_TITLE
    $(widgetFile "lstcom")

postLstComResultR :: ExerciseId -> Handler Html
postLstComResultR exerciseId =
  let successFunc = loadResultLayout exerciseId
      failFunc = loadResultLayoutWithResultAndLstCom exerciseId
  in getLstComResult successFunc failFunc

loadResultLayoutWithResultAndLstCom :: ExerciseId
                                    -> String
                                    -> String
                                    -> Handler Html
loadResultLayoutWithResultAndLstCom exerciseId result lstCom =
  let leftExpr = "" :: String
      quals = []
      json = "" :: String
  in loadResultLayout exerciseId lstCom leftExpr result quals json

loadResultLayout :: ExerciseId
                 -> String
                 -> String
                 -> String
                 -> [Qualifier]
                 -> String
                 -> Handler Html
loadResultLayout exerciseId lstCom leftExpr result quals json = do
  mExercise <- getExercise exerciseId
  case mExercise of
    Just exercise -> do
                      let layoutFunc = loadResultLayoutWorker lstCom leftExpr
                      let layoutFunc' = layoutFunc result quals json exerciseId
                      initExerciseResult layoutFunc' exercise
    _             -> loadLayout lstCom leftExpr result quals json

loadResultLayoutWorker :: String
                       -> String
                       -> String
                       -> [Qualifier]
                       -> String
                       -> ExerciseId
                       -> Exercise
                       -> String
                       -> [String]
                       -> Handler Html
loadResultLayoutWorker lstCom leftExpr pResult quals json
                       exerciseId exercise expectedResult boxes = do
  let (result, leftResult, redResult, rightResult) = getErrorText pResult lstCom
      exeName = unpack $ exerciseName exercise
      exeTask = unpack $ exerciseTask exercise

  exercises <- getActiveExercises
  defaultLayout $ do
    setTitle c_TITLE
    $(widgetFile "lstcom")

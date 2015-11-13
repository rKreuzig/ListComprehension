module Handler.LstComAdmin where

import Import hiding (pack, head, tail)

import Prelude (read, head, tail)
import Data.Text (pack)

import DatabaseConnection
import Control.Monad.IO.Class (liftIO)

getLstComAdminR :: Handler Html
getLstComAdminR = do
  showExercises

postLstComAdminExportExercisesR :: Handler Html
postLstComAdminExportExercisesR = do
  exportExercises
  showExercises

postLstComAdminImportExercisesR :: Handler Html
postLstComAdminImportExercisesR = do
  name <- runInputPost $ ireq textField "date"
  importExercises $ unpack name
  showExercises

postLstComAdminDeleteExercisesR :: Handler Html
postLstComAdminDeleteExercisesR = do
  deleteExercises
  showExercises

postLstComAdminAddExerciseR :: Handler Html
postLstComAdminAddExerciseR = do
  addExercise
  showExercises

addExercise :: Handler ()
addExercise = do
  (name, task, sol, boxes) <- runInputPost $ (,,,) <$> ireq textField "name"
                                                   <*> ireq textareaField "task"
                                                   <*> ireq textField "solution"
                                                   <*> ireq textField "boxes"
  let boxLst = read (unpack boxes) :: [Text]
  insertExercise name (unTextarea task) sol boxLst

postLstComAdminDeleteExerciseR :: ExerciseId -> Handler Html
postLstComAdminDeleteExerciseR eid = do
  deleteExercise eid
  showExercises

postLstComAdminUpdateExerciseR :: ExerciseId -> Handler Html
postLstComAdminUpdateExerciseR eid = do
  updExercise eid
  showExercises

postLstComAdminActiveExerciseR :: ExerciseId -> Handler Html
postLstComAdminActiveExerciseR eid = do
  active' <- runInputPost $ ireq textField "active"
  let active = read (unpack active') :: Bool
  setActive eid active
  showExercises

updExercise :: ExerciseId -> Handler Html
updExercise eid = do
  (name, task, sol, boxes) <- runInputPost $ (,,,) <$> ireq textField "name"
                                                   <*> ireq textareaField "task"
                                                   <*> ireq textField "solution"
                                                   <*> ireq textField "boxes"
  let boxLst = read (unpack boxes) :: [Text]
  updateExercise eid name (unTextarea task) sol boxLst
  showExercises

showExercises :: Handler Html
showExercises = do
  backupFiles <- liftIO getBackupFiles
  exercises <- getAllExercises
  defaultLayout $ do
    setTitle "Admin"
    $(widgetFile "lstcom-admin")

module DatabaseConnection
where

import Import hiding (ByteString, writeFile, readFile)

import Control.Monad.IO.Class (liftIO)
import System.FilePath (pathSeparator)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory
                       , getDirectoryContents)

import Data.Time
import Prelude (init)
import qualified Data.List as L (delete)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (ByteString, readFile, writeFile)

insertExercise :: Text -> Text -> Text -> [Text] -> Handler ()
insertExercise name task sol boxes = do
  let nExe = Exercise name task sol boxes True
  runDB $ insert nExe
  return ()

getExercise :: ExerciseId -> Handler (Maybe Exercise)
getExercise eid =
  runDB $ get eid

deleteExercise :: ExerciseId -> Handler ()
deleteExercise eid =
  runDB $ delete eid

deleteExercises :: Handler ()
deleteExercises =
  runDB $ deleteWhere ([] :: [Filter Exercise])

updateExercise :: ExerciseId -> Text -> Text -> Text -> [Text] -> Handler ()
updateExercise eid name task sol boxes =
  runDB $ update eid [ExerciseName =. name
                    , ExerciseTask =. task
                    , ExerciseSolution =. sol
                    , ExerciseBoxes =. boxes]

setActive :: ExerciseId -> Bool -> Handler ()
setActive eid active =
  runDB $ update eid [ExerciseActive =. active]

getActiveExercises :: Handler ([Entity Exercise])
getActiveExercises =
  runDB $ selectList [ExerciseActive ==. True] [Asc ExerciseName]

getAllExercises :: Handler ([Entity Exercise])
getAllExercises =
  runDB $ selectList [] [Asc ExerciseName]

importExercises :: String
                -> Handler ()
importExercises name = do
  exercises <- liftIO $ jsonToExercises name
  mapM insertEntityExercise exercises
  return ()

jsonToExercises :: String
                -> IO ([Entity Exercise])
jsonToExercises name = do
  backupDir <- getBackupDir
  content <- readFile $ backupDir ++ [pathSeparator] ++ name ++ ".txt"
  let mExercises = decode content
  case mExercises of
    Just exercises -> return exercises
    _              -> error $ name ++ ".txt konnte nicht geladen werden"

insertEntityExercise :: (Entity Exercise)
                     -> Handler ()
insertEntityExercise (Entity eid e) = do
  runDB $ insert e
  return ()

getBackupFiles :: IO [String]
getBackupFiles = do
  backupDir <- getBackupDir
  content <- getDirectoryContents backupDir
  let content' = L.delete ".." content
  let content'' = L.delete "." content'
  let content''' = map (init . init . init . init) content''
  return content'''

exportExercises :: Handler ()
exportExercises = do
  backupDir <- liftIO getBackupDir
  date <- liftIO $ getDate
  let name = date ++ ".txt"
  let path = backupDir ++ [pathSeparator] ++ name

  content <- exercisesToJson
  liftIO $ writeFile path content

getBackupDir :: IO String
getBackupDir = do
  curDir <- getCurrentDirectory
  let backupDir = curDir ++ [pathSeparator] ++ "backup"
  createDirectoryIfMissing False backupDir
  return backupDir

getDate :: IO String
getDate = do
  now' <- getZonedTime
  let now = zonedTimeToLocalTime now'
  let date = formatTime defaultTimeLocale "%d-%m-%y(%H.%M.%S)" now
  return date

exercisesToJson :: Handler ByteString
exercisesToJson = do
  exercises <- getAllExercises
  let jsonLst = encode exercises
  return jsonLst

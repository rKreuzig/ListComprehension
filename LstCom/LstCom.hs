{-# LANGUAGE DeriveGeneric #-}

-- |
module LstCom.LstCom (
-- * Types
  Qualifier (..), QualifierType (..),
-- * Functions
  getLstComData
) where

import Prelude
import GHC.Generics
import Data.Map (empty)
import Data.Text (unpack)
import System.Timeout (timeout)
import Data.Text.Lazy (toStrict)
import Control.Monad.Trans.Either
import Data.Aeson (ToJSON, toJSON)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson.Encode (fromValue, encodeToTextBuilder)

import LstCom.Data
import LstCom.Constants
import LstCom.Executer.Executer
import LstCom.Executer.Interpreter
import LstCom.Parser.General
import LstCom.Parser.ListComprehension

data JsonObj = JsonObj {
  key :: String,
  value :: String
} deriving (Show, Generic)
instance ToJSON JsonObj

getLstComData :: ServerHandle
              -> String -- ^ the List Comprehension
              -> IO (Either String (String, String, [Qualifier], String))
getLstComData handle input = do
  mTimeout <- timeout c_TIMEOUT_LST_COM $ do
    let eParse = parseListComprehension input
    case eParse of
      Left l -> return $ Left l
      _      -> runEitherT $ do
                  let Right (leftExpr, quals) = getTokens input
                  res <- getLstComStepsAndResult handle leftExpr quals
                  let (steps, lstComResult) = res
                  let json = getJson steps

                  let eleCount = length lstComResult
                  let input' = "take " ++ show eleCount ++ " " ++ input
                  directRes <- evalExpr handle empty input'
                  let Right dirRes = getList directRes
                  if dirRes == lstComResult
                    then
                      return (directRes, leftExpr, quals, json)
                    else
                      EitherT $ return . Left $ e_LOGIC_ERROR
  case mTimeout of
    Nothing -> return $ Left c_TIMEOUT_LST_COM_TXT
    Just x  -> return x

getJson :: [Step]
        -> String
getJson steps =
  "{\"steps\":" ++ (preFunc $ getJsonSteps steps) ++ "}"
  where
    preFunc = unpack . toStrict . toLazyText . encodeToTextBuilder . toJSON

getJsonSteps :: [Step]
             -> [JsonObj]
getJsonSteps [] = []
getJsonSteps ((name, value):steps) =
  let obj = JsonObj name value
  in [obj] ++ getJsonSteps steps

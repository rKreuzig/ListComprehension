module LstCom.Data where

import Prelude
import Data.Map (Map)

type Scope = Map Int (String, String)

type Step = (String, String)

data Qualifier = Qualifier {
  qualExpr :: String,
  qualType :: QualifierType,
  qualPos :: Int,
  qualVars :: String,
  qualGenLst :: [String]
} deriving (Show)

data QualifierType = Generator | Guard
  deriving (Show, Eq)

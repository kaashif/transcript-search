{-# LANGUAGE DeriveGeneric #-}
module Data.Stargate where

import GHC.Generics
import Data.Aeson
import qualified Data.Set as S

type Character = String

data ScriptExpr = Place String
                | Annotation String
                | Speech Character String
                | Junk String
                  deriving Show

data Scene = Scene {
      place :: String,
      present :: S.Set String,
      speech :: [(Character, String)]
    } deriving (Generic, Show)
instance ToJSON Scene

type Episode = [Scene]

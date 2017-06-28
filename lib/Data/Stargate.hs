{-# LANGUAGE DeriveGeneric #-}
module Data.Stargate where

import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Text as T

type Character = T.Text

data ScriptExpr = Place T.Text
                | Annotation T.Text
                | Speech Character T.Text
                | Junk T.Text
                  deriving Show

data Scene = Scene {
      place :: T.Text,
      present :: S.Set T.Text,
      speech :: V.Vector (Character, T.Text)
    } deriving (Generic, Show)

type Episode = V.Vector Scene

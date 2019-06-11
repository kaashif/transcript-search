{-# LANGUAGE DeriveGeneric #-}
module Transcript where

import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Text as T

type Character = T.Text

newtype SpeechLine = SpeechLine (Character, T.Text)
  deriving (Show, Generic)

data IntExt = Interior
            | Exterior
              deriving (Show, Generic, Eq, Ord)

data ScriptExpr = Place IntExt T.Text
                | Annotation T.Text
                | Speech Character T.Text
                | Junk T.Text
                | Title T.Text
                  deriving (Show, Generic, Eq)

data Scene = Scene {
      intext :: IntExt,
      place :: T.Text,
      present :: S.Set T.Text,
      speech :: V.Vector SpeechLine
    } deriving (Show, Generic)

data Episode = Episode {
      scenes :: V.Vector Scene,
      title :: T.Text,
      exprs :: [ScriptExpr]
    } deriving (Show, Generic)

emptyScene :: Scene
emptyScene = Scene Exterior (T.pack "nowhere") S.empty V.empty

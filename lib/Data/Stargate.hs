{-# LANGUAGE DeriveGeneric #-}
module Data.Stargate where

import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Text as T

type Character = T.Text

data IntExt = Interior
            | Exterior
              deriving (Show, Generic)

data ScriptExpr = Place IntExt T.Text
                | Annotation T.Text
                | Speech Character T.Text
                | Junk T.Text
                | Title T.Text
                  deriving (Show, Generic)

data Scene = Scene {
      intext :: IntExt,
      place :: T.Text,
      present :: S.Set T.Text,
      speech :: V.Vector (Character, T.Text),
      upperspeech :: V.Vector (Character, T.Text)
    } deriving (Show, Generic)

data Episode = Episode {
      scenes :: V.Vector Scene,
      title :: T.Text
    } deriving (Show, Generic)

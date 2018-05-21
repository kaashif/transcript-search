{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Transcript.JSON where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Transcript as D
import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Transcript.IO
import Text.Read (readMaybe)

data LineRecord = LineRecord { series :: T.Text
                             , season_number :: Int
                             , episode_number :: Int
                             , episode_title :: T.Text
                             , scene_number :: Int
                             , line_number :: Int
                             , person :: T.Text
                             , present :: T.Text
                             , place :: T.Text
                             , speech :: T.Text
                             } deriving (Show, Generic)
instance ToJSON LineRecord

data IDRecord = IDRecord { _index :: T.Text
                         , _type :: T.Text
                         , _id :: Int
                         } deriving (Show, Generic)
instance ToJSON IDRecord

toLineRecords :: T.Text -> T.Text -> D.Episode -> V.Vector LineRecord
toLineRecords serie epcode ep = let
    [seasnum', epnum'] :: [Int] = case T.splitOn "." epcode of
                                    [x, y] -> map (read . T.unpack) [x, y]
                                    [x] -> [0, maybe 0 id $ readMaybe $ T.unpack x]
                                    _ -> [0, 0]
    sceneToLineRecords (sceneno', scene) = V.map (lineToLineRecord sceneno' (D.place scene) (D.present scene))
                                     (V.indexed $ D.speech scene)
    lineToLineRecord sceneno' pl pr (lineno', D.SpeechLine speec) = LineRecord { series = serie
                                                                               , season_number = seasnum'
                                                                               , episode_number = epnum'
                                                                               , episode_title = D.title ep
                                                                               , scene_number = sceneno'
                                                                               , line_number = lineno'
                                                                               , person = fst speec
                                                                               , present = T.intercalate " " $ S.toList pr
                                                                               , place = pl
                                                                               , speech = snd speec
                                                                               }
    in V.concat $ V.toList $ V.map sceneToLineRecords (V.indexed $ D.scenes ep)

toJSON :: V.Vector (SeriesName, SeasonEpisodeCode, D.Episode) -> BS.ByteString
toJSON v = BS.concat $ snd $ V.foldl' convert' (0, []) v
    where convert' (currentID, result) (serie, epcode, ep) = V.foldl' addID (currentID, result) $ toLineRecords serie epcode ep
          addID (currentID, result) linerec = ( currentID+1
                                              , (BSL.toStrict $ encode $ M.singleton ("index" :: T.Text) $ IDRecord "transcripts" "line" currentID)
                                              : "\n"
                                              : (BSL.toStrict $ encode linerec)
                                              : "\n"
                                              : result
                                              )

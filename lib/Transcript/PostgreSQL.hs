{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Transcript.PostgreSQL where

import GHC.Generics
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Transcript as D
import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Transcript.IO
import Text.Read (readMaybe)
import TextShow

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


toPostgreSQL :: V.Vector (SeriesName, SeasonEpisodeCode, D.Episode) -> T.Text
toPostgreSQL v = T.concat $ V.toList $ V.concat $ V.toList $ V.map toTabSeparated v
    where toTabSeparated (series, epcode, ep) = V.map recToTab $ toLineRecords series epcode ep
          recToTab r = T.append (T.filter (/='\n') $ T.concat [ series r, "\t",
                                  showt $ season_number r, "\t",
                                  showt $ episode_number r, "\t",
                                  episode_title r, "\t",
                                  showt $ scene_number r, "\t",
                                  showt $ line_number r, "\t",
                                  person r, "\t",
                                  present r, "\t",
                                  place r, "\t",
                                  speech r
                                ]) "\n"

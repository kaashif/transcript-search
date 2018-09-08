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
import Transcript.JSON
import TextShow

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

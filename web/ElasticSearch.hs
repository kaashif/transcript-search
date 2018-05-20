{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module ElasticSearch where

import Data.Aeson
import GHC.Generics
import Text.Printf
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M

data TextOrList = Text T.Text
                | List [T.Text]
                  deriving (Show, Generic)

data ResultsOrText = RText T.Text
                   | Results [M.HashMap T.Text TextOrList]
                     deriving (Show, Generic)

removePrefix :: String -> String
removePrefix (_:_:rest) = rest
removePrefix _ = "bad name I guess"

data Shards = Shards {
      s_total :: Int
    , s_successful :: Int
    , s_skipped :: Int
    , s_failed :: Int
    } deriving (Show, Generic)

instance FromJSON Shards where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removePrefix }

data Source = Source {
      s_scene_number :: Int
    , s_season_number :: Int
    , s_person :: String
    , s_speech :: String
    , s_series :: String
    , s_episode_number :: Int
    , s_episode_title :: String
    , s_line_number :: Int
    , s_present :: String
    , s_place :: String
    } deriving (Show, Generic)

instance FromJSON Source where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removePrefix }

data Hit = Hit {
      h__index :: String
    , h__type :: String
    , h__score :: Float
    , h__source :: Source
    } deriving (Show, Generic)

instance FromJSON Hit where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removePrefix }

data Hits = Hits {
      h_total :: Int
    , h_max_score :: Float
    , h_hits :: [Hit]
    } deriving (Show, Generic)

instance FromJSON Hits where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removePrefix }

data SearchResults = SearchResults {
      r_took :: Int
    , r_timed_out :: Bool
    , r__shards :: Shards
    , r_hits :: Hits
    } deriving (Show, Generic)

instance FromJSON SearchResults where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removePrefix }

hitToText :: Hit -> T.Text
hitToText h = T.concat [person, ": ", speech]
    where src = h__source h
          person = T.pack $ s_person src
          speech = T.pack $ s_speech src

hitToResult :: ([Hit], Hit, [Hit]) -> M.HashMap T.Text TextOrList
hitToResult (before, hit, after) = M.fromList [ ("url", Text $ T.concat ["/transcripts/", series, "/", filename])
                                              , ("context_before", List $ map hitToText before)
                                              , ("match", Text $ hitToText hit)
                                              , ("context_after", List $ map hitToText after)
                                              , ("episode", Text $ T.concat [T.toUpper series,  ": ", shorttext])
                                              , ("epraw", Text $ T.concat [series, ": ", T.pack $ printf "%d" seasnum, ".", epnum])
                                              , ("place", Text place)
                                              ]
    where src = h__source hit
          series = T.pack $ s_series src
          seasnum = s_season_number src
          epnum = T.pack $ printf "%d" $ s_episode_number src
          place = T.pack $ s_place src
          title = T.pack $ s_episode_title src
          filename = if seasnum == 0
                     then epnum
                     else T.concat [T.pack $ printf "%d" seasnum, ".", epnum]
          shorttext = if seasnum == 0
                      then title
                      else T.concat ["Season ", T.pack $ printf "%d" seasnum, " Episode ", epnum, ": ", title]

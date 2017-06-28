{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.Stargate.Search where

import qualified Data.Text as T
import Text.Ginger hiding (length)
import Text.Ginger.Parse
import qualified Data.Stargate as D
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.Vector.Algorithms.Merge as V
import qualified Data.HashMap.Lazy as M

data TextOrList = Text T.Text
                | List [T.Text]

instance ToGVal m TextOrList where
    toGVal torm = case torm of
                    Text t -> toGVal t
                    List l -> toGVal l

data ResultsOrText = RText T.Text
                   | Results [M.HashMap T.Text TextOrList]

instance ToGVal m ResultsOrText where
    toGVal rort = case rort of
                    RText t -> toGVal t
                    Results t -> toGVal t


match :: T.Text -> T.Text -> Bool
match query body = (T.null query) || ((T.toUpper query) `T.isInfixOf` (T.toUpper body))

search :: V.Vector (T.Text, T.Text, D.Episode) -> T.Text -> T.Text -> T.Text -> T.Text -> M.HashMap T.Text ResultsOrText
search eps query place person present = let
      toomany = if (length results) >= 500 then "yes" else "no"
      results = V.foldl' epResults [] eps
      epResults res (series, episode, ep) = let
          newmatches = V.foldl' sceneResults [] ep
          sceneResults ress scene = let
              newSceneResults = if or [ not $ any (match present) (D.present scene)
                                      , not $ match place (D.place scene)
                                      ]
                        then []
                        else lineMatches (D.speech scene)
              lineMatches speech = let
                  resultOrEmpty i = if (match person (fst $ speech ! i)) && (match query (snd $ speech ! i))
                        then [makeResult i]
                        else []
                  makeResult i = M.fromList [ ("url", Text $ T.concat ["/transcripts/", series, "/", episode])
                                            , ("context_before", List $ V.toList $ strline (max 0 (i-2)) i)
                                            , ("match", Text $ V.head $ strline i (i+1))
                                            , ("context_after", List $ V.toList $ strline (min (i+1) (length speech)) (min (i+3) (length speech)))
                                            , ("episode", Text $ T.concat [T.toUpper series, " Season ", season, " Episode ", epnum])
                                            , ("place", Text $ D.place scene)
                                            ]
                  [season, epnum] = T.splitOn "." episode
                  strline x y = V.map (\pair -> T.concat [fst pair, ": ", snd pair]) (V.slice x y speech)
                in concat $ map resultOrEmpty [0..(V.length speech)-1]
              in concat [newSceneResults, ress]
          in concat [newmatches, res]
  in M.fromList [("results", Results results)
                ,("toomany", RText toomany)
                ]

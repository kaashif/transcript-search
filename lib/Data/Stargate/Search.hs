{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Stargate.Search where

import qualified Data.Text as T
import qualified Data.Stargate as D
import qualified Data.Vector as V
import Data.Vector ((!))
import qualified Data.HashMap.Lazy as M
import Control.Monad.ST
import Data.STRef
import Control.Monad

data TextOrList = Text T.Text
                | List [T.Text]

data ResultsOrText = RText T.Text
                   | Results [M.HashMap T.Text TextOrList]

match :: T.Text -> T.Text -> Bool
match query body = (T.null query) || query `T.isInfixOf` body

makeResult :: Int -> T.Text -> T.Text -> T.Text -> D.Scene -> M.HashMap T.Text TextOrList
makeResult i series season epnum scene = M.fromList [ ("url", Text $ T.concat ["/transcripts/", series, "/", season, ".", epnum])
                                                    , ("context_before", List $ strline speech (i-2) i)
                                                    , ("match", Text $ head $ strline speech i (i+1))
                                                    , ("context_after", List $ strline speech (i+1) (i+3))
                                                    , ("episode", Text $ T.concat [T.toUpper series, " Season ", season, " Episode ", epnum])
                                                    , ("place", Text $ D.place scene)
                                                    ]
    where speech = D.speech scene

strline :: V.Vector (D.Character, T.Text) -> Int -> Int -> [T.Text]
strline speech x y
        | x > y = []
        | x < 0 = strline speech 0 y
        | x >= V.length speech = []
        | y > V.length speech = strline speech x (V.length speech)
        | otherwise =  V.toList $ V.map (\(c, l) -> T.concat [c, ": ", l]) (V.slice x (y-x) speech)

sceneMatches :: D.Scene -> T.Text -> T.Text -> T.Text -> T.Text -> [Int]
sceneMatches scene query place person present = if or [ not $ any (match present) (D.present scene)
                                                      , not $ match place (D.place scene)
                                                      ]
                                                then []
                                                else runST $ do
  matches <- newSTRef []
  forM_ [0..((V.length $ D.speech scene)-1)] $ \k ->
      when ((match person (fst $ (D.speech scene) ! k)) && (match query $ snd $ (D.upperspeech scene) ! k))
           (modifySTRef matches (k:))
  readSTRef matches

search :: V.Vector (T.Text, T.Text, D.Episode) -> T.Text -> T.Text -> T.Text -> T.Text -> M.HashMap T.Text ResultsOrText
search eps q place person present = runST $ do
  let query = T.toUpper q                                      
  results <- newSTRef []
  found <- newSTRef (0 :: Int)
  forM_ [0..((V.length eps)-1)] $ \i -> do
      let (series, epstr, episode) = eps ! i
          [season, epnum] = T.splitOn "." epstr
      forM_ [0..((V.length episode)-1)] $ \j -> do
          let matchks = sceneMatches (episode ! j) query place person present
          case matchks of
            [] -> return ()
            ks -> do
              let newresults = map (\k -> makeResult k series season epnum (episode ! j)) ks
              modifySTRef results (newresults++)
              modifySTRef found (+1)
  nfound <- readSTRef found
  let toomany = if nfound >= 500 then "yes" else "no"
  finalres <- readSTRef results
  return $ M.fromList [("results", Results finalres)
                      ,("toomany", RText toomany)
                      ]
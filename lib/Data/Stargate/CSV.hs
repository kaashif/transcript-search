{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Stargate.CSV where

import Data.Csv
import Data.List.Split
import Data.List
import GHC.Generics
import Text.Printf
import qualified Data.Set as S
import qualified Data.Stargate as D
import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as V

data LineRecord = LineRecord { epid :: Int
                             , person :: T.Text
                             , present :: T.Text
                             , place :: T.Text
                             , speech :: T.Text
                             } deriving (Show, Generic)
instance ToRecord LineRecord

toId :: T.Text -> Int -> Int -> Int -> Int -> Int
toId series seasnum epnum sceneno lineno = read $ concat [ printf "%d" serienum
                                                 , printf "%02d" seasnum
                                                 , printf "%02d" epnum
                                                 , printf "%02d" sceneno
                                                 , printf "%04d" lineno
                                                 ]
  where serienum = (if series == "sg1" then 1 else 2) :: Int

toLineRecords :: T.Text -> T.Text -> D.Episode -> V.Vector LineRecord
toLineRecords serie epcode ep = let
    [seasnum, epnum] :: [Int] = map (read . T.unpack) $ T.splitOn "." epcode
    sceneToLineRecords (sceneno, scene) = V.map (lineToLineRecord sceneno (D.place scene) (D.present scene))
                                     (V.indexed $ D.speech scene)
    lineToLineRecord sceneno pl pr (lineno, D.SpeechLine speec) = LineRecord { epid = toId serie seasnum epnum sceneno lineno
                                                        , person = fst speec
                                                        , present = T.intercalate " " $ S.toList pr
                                                        , place = pl
                                                        , speech = snd speec
                                                        }
    in V.concat $ V.toList $ V.map sceneToLineRecords (V.indexed $ D.scenes ep)

toCsv :: V.Vector (T.Text, T.Text, D.Episode) -> BSL.ByteString
toCsv v = encode $ V.foldl' convert' [] v
    where convert' recs (serie, epcode, ep) = (V.toList $ toLineRecords serie epcode ep) ++ recs

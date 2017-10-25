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

toId :: T.Text -> Int -> Int -> Int -> Int
toId series seasnum epnum lineno = read $ concat [ printf "%02d" serienum
                                                 , printf "%02d" seasnum
                                                 , printf "%02d" epnum
                                                 , printf "%04d" lineno
                                                 ]
  where serienum = (if series == "sg1" then 0 else 1) :: Int

toLineRecords :: T.Text -> T.Text -> D.Episode -> V.Vector LineRecord
toLineRecords serie epcode ep = let
    [seasnum, epnum] :: [Int] = map (read . T.unpack) $ T.splitOn "." epcode
    sceneToLineRecords scene = V.map (lineToLineRecord (D.place scene) (D.present scene))
                                     (V.indexed $ D.speech scene)
    lineToLineRecord pl pr (lineno, speec) = LineRecord { epid = toId serie seasnum epnum lineno
                                                        , person = fst speec
                                                        , present = T.intercalate " " $ S.toList pr
                                                        , place = pl
                                                        , speech = snd speec
                                                        }
    in V.concat $ V.toList $ V.map sceneToLineRecords (D.scenes ep)

toCsv :: V.Vector (T.Text, T.Text, D.Episode) -> BSL.ByteString
toCsv v = encode $ V.foldl' convert' [] v
    where convert' recs (serie, epcode, ep) = (V.toList $ toLineRecords serie epcode ep) ++ recs

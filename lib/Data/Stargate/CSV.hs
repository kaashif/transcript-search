{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Stargate.CSV where

import Data.Csv
import Data.List.Split
import Data.List
import GHC.Generics
import qualified Data.Set as S
import qualified Data.Stargate as D
import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector as V

data LineRecord = LineRecord { season :: Int
                             , episode :: Int
                             , series :: T.Text
                             , person :: T.Text
                             , present :: T.Text
                             , place :: T.Text
                             , speech :: T.Text
                             } deriving (Show, Generic)
instance ToRecord LineRecord

toLineRecords :: T.Text -> T.Text -> D.Episode -> V.Vector LineRecord
toLineRecords serie epcode ep = let
    [seasnum, epnum] :: [Int] = map (read . T.unpack) $ T.splitOn "." epcode
    sceneToLineRecords scene = V.map (lineToLineRecord seasnum epnum (D.place scene) (D.present scene)) (D.speech scene)
    lineToLineRecord sn en pl pr speec = LineRecord { season = seasnum
                                                    , episode = epnum
                                                    , series = serie
                                                    , person = fst speec
                                                    , present = T.intercalate " " $ S.toList pr
                                                    , place = pl
                                                    , speech = snd speec
                                                    }
    in V.concat $ V.toList $ V.map sceneToLineRecords (D.scenes ep)

toCsv :: V.Vector (T.Text, T.Text, D.Episode) -> BSL.ByteString
toCsv v = encode $ V.foldl' convert' [] v
    where convert' recs (serie, epcode, ep) = (V.toList $ toLineRecords serie epcode ep) ++ recs

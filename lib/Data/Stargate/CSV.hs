{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Stargate.CSV where

import Data.Csv
import Data.List.Split
import Data.List
import GHC.Generics
import qualified Data.Set as S
import qualified Data.Stargate as D
import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString.Lazy as BSL

data LineRecord = LineRecord { season :: Int
                             , episode :: Int
                             , series :: String
                             , person :: String
                             , present :: String
                             , place :: String
                             , speech :: String
                             } deriving (Show, Generic)
instance ToRecord LineRecord

toLineRecords :: String -> String -> D.Episode -> [LineRecord]
toLineRecords serie epcode ep = let
    [seasnum, epnum] :: [Int] = map read $ splitOn "." epcode
    sceneToLineRecords scene = map (lineToLineRecord seasnum epnum (D.place scene) (D.present scene)) (D.speech scene)
    lineToLineRecord sn en pl pr speec = LineRecord { season = seasnum
                                                    , episode = epnum
                                                    , series = serie
                                                    , person = fst speec
                                                    , present = intercalate " " $ S.toList pr
                                                    , place = pl
                                                    , speech = snd speec
                                                    }
    in concat $ map sceneToLineRecords ep

toCsv :: M.HashMap (String, String) D.Episode -> BSL.ByteString
toCsv m = encode $ M.foldlWithKey' convert' [] m
    where convert' recs (serie, epcode) ep = (toLineRecords serie epcode ep) ++ recs

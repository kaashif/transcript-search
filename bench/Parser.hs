{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.Stargate.Search
import Data.Stargate.IO
import Data.Stargate
import Control.DeepSeq
import GHC.IO.Encoding
import qualified Data.Vector as V

instance NFData Episode
instance NFData ResultsOrText
instance NFData TextOrList
instance NFData Scene
instance NFData IntExt

main :: IO ()
main = do
  setLocaleEncoding utf8
  ts <- fmap (V.map fst) readAllTranscripts
  defaultMain [ bench "parse everything" $ nfIO readAllTranscripts
              , bench "find indeed" $ nf (search ts "indeed" "" "") ""
              , bench "find nothing" $ nf (search ts "" "" "") ""
              ]

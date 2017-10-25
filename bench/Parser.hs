{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.Stargate.Search
import Data.Stargate.IO
import Data.Stargate
import Control.DeepSeq

instance NFData Episode
instance NFData ResultsOrText
instance NFData TextOrList
instance NFData Scene
instance NFData IntExt

main = do
  ts <- readAllTranscripts
  defaultMain [ bench "parse everything" $ nfIO readAllTranscripts
              , bench "find indeed" $ nf (search ts "indeed" "" "") ""
              , bench "find nothing" $ nf (search ts "" "" "") ""
              ]

{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main
import Data.Stargate.Search
import Data.Stargate.IO
import Data.Stargate
import Data.Stargate.Markov
import Control.DeepSeq
import Text.Wrap
import GHC.IO.Encoding
import System.Random
import qualified Data.Vector as V

instance NFData Episode
instance NFData ScriptExpr
instance NFData ResultsOrText
instance NFData TextOrList
instance NFData SpeechLine
instance NFData Scene
instance NFData IntExt

main :: IO ()
main = do
  setLocaleEncoding utf8
  ts <- readAllTranscripts
  rand <- getStdGen
  let thd (_,_,c) = c
  let wordlist = V.fromList $ exprsToMarkov $ concat $ V.map (exprs . thd) ts
  defaultMain [
                bench "generate 1 transcript" $ nf ((wrapText defaultWrapSettings 80) . (generateTrans rand)) wordlist 
              , bench "parse everything" $ nfIO readAllTranscripts
              , bench "find indeed" $ nf (search ts "indeed" "" "") ""
              , bench "find nothing" $ nf (search ts "" "" "") ""
              ]

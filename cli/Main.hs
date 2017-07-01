{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Stargate.IO
import Data.Stargate.Search
import qualified Data.Stargate as D
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
  ts <- readAllTranscripts
  forever $ do
    querystr <- getLine
    putStrLn $ doSearch ts querystr

doSearch :: V.Vector (T.Text, T.Text, D.Episode) -> String -> String
doSearch ts querystr = case (fromJust $ M.lookup "results" $ search ts (T.toUpper $ T.pack querystr) "" "" "") of
                         Results rs -> T.unpack $ T.concat $ map makeNiceResult rs
                         RText t -> T.unpack t

makeNiceResult :: M.HashMap T.Text TextOrList -> T.Text
makeNiceResult m = fromJust $ do
  mx <- fmap fromText $ M.lookup "match" m
  ep <- fmap fromText $ M.lookup "epraw" m
  return $ T.concat [ep, ": ", mx, "\n"]
    where fromText x = case x of
                         Text t -> t
                         _ -> error "makeNiceResult.fromText: wasn't text"

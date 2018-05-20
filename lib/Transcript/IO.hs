{-# LANGUAGE OverloadedStrings #-}
module Transcript.IO where

import qualified Data.Vector as V
import System.FilePath
import qualified Transcript as D
import qualified Transcript.Parse.Stargate as Gate (scriptp, convert)
import qualified Transcript.Parse.Trek as Trek (scriptp, convert)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Attoparsec.Text (parseOnly)
import Control.Monad
import System.FilePath.Glob
import qualified Data.ByteString as BS
import qualified Data.Set as S
import System.IO

type SeriesName = T.Text
type SeasonCode = T.Text
type EpisodeCode = T.Text
type EpisodeTitle = T.Text
type SeasonEpisodeCode = T.Text

parseStargate :: T.Text -> D.Episode
parseStargate raw = case parseOnly Gate.scriptp raw of
                      Right exprs -> Gate.convert exprs
                      _ -> D.Episode (V.singleton noScene) T.empty []
    where noScene = D.Scene D.Exterior "nowhere" S.empty V.empty

parseStarTrek :: T.Text -> D.Episode
parseStarTrek raw = case parseOnly Trek.scriptp raw of
                      Right exprs -> Trek.convert exprs
                      _ -> D.Episode (V.singleton noScene) T.empty []
    where noScene = D.Scene D.Exterior "nowhere" S.empty V.empty

readTranscript :: (T.Text -> D.Episode) -> FilePath -> IO D.Episode
readTranscript p = fmap (p . T.decodeLatin1) . BS.readFile

seriesParsers :: [(String, T.Text -> D.Episode)]
seriesParsers = [("sg1", parseStargate)
                ,("atl", parseStargate)
                ,("tos", parseStarTrek)
                ,("tng", parseStarTrek)
                ,("ds9", parseStarTrek)
                ,("voy", parseStarTrek)
                ,("ent", parseStarTrek)
                ]

readAllTranscripts :: IO (V.Vector (SeriesName, SeasonEpisodeCode, D.Episode))
readAllTranscripts = fmap V.concat $ forM seriesParsers $ \(series, parser) -> do
  fnames <- globDir1 (compile $ joinPath ["transcripts", series, "*"]) "."
  let readT f = readTranscript parser f >>= \t -> return (T.pack series, T.pack $ last $ splitPath f, t)
  V.mapM readT (V.fromList fnames)

type Entry = (SeriesName, SeasonCode, EpisodeCode, EpisodeTitle)

readAllEntries :: IO [Entry]
readAllEntries = fmap concat $ forM seriesParsers $ \(series, parser) -> do
  fnames <- globDir1 (compile $ joinPath ["pretty", series, "*"]) "."
  let readTitle f = openFile f ReadMode >>= T.hGetLine
  let makeEntry fname = do
        title <- readTitle fname
        let [seasnum, epnum] = case T.splitOn "." $ T.pack $ last $ splitPath fname of
                                 [x, y] -> [x, y]
                                 [x] -> [T.empty, x]
                                 _ -> [T.empty, T.empty]
        return (T.pack series, seasnum, epnum, title)
  mapM makeEntry fnames

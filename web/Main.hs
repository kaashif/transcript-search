{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Web.Scotty
import Text.Ginger hiding (length)
import Text.Ginger.Html hiding (html)
import System.IO.Error
import System.IO
import System.FilePath
import System.Random
import Control.Monad.IO.Class
import Data.List
import qualified Data.HashMap.Lazy as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Stargate as D
import qualified Data.Vector as V
import Data.Stargate.Search
import Data.Stargate.IO
import Data.Stargate.Markov
import Data.Stargate.Format
import Data.Maybe
import Control.Monad.Trans.Writer.Lazy
import System.Environment
import Text.Wrap as TW
import TextShow

instance ToGVal m TextOrList where
    toGVal torm = case torm of
                    Text t -> toGVal t
                    List l -> toGVal l

instance ToGVal m ResultsOrText where
    toGVal rort = case rort of
                    RText t -> toGVal t
                    Results t -> toGVal t

main :: IO ()
main = do
  eps <- readAllTranscripts
  let thd (_,_,c) = c
  let wordlist = exprsToMarkov $ concat $ V.map (D.exprs . thd) eps
  port <- fmap (fromMaybe "5000") $ lookupEnv "PORT"
  scotty (read port :: Int) $ do
  get "/" indexR
  get "/transcripts/:series/:episode" $ do
    series <- param "series"                      
    episode <- param "episode"
    transR (findTrans eps series episode) series episode
  get "/random" $ do
    rand <- liftIO getStdGen
    genR $ TW.wrapText TW.defaultWrapSettings 80 $ generateTrans rand wordlist
  get "/search" $ do
    query <- param "query"
    place <- param "place"
    person <- param "person"
    present <- param "present"
    searchR eps query place person present
  get "/about" aboutR
  get "/style.css" $ do
    setHeader "Content-Type" "text/css"
    file "style.css"
  get "/transcripts" $ do
    transIndexR $ sort $ makeEntries eps

findTrans :: V.Vector (T.Text, T.Text, D.Episode) -> T.Text -> T.Text -> D.Episode
findTrans eps series episode = thd $ fromJust $ V.find (\(a, b, _) -> series == a && episode == b) eps
  where thd (_,_,c) = c

-- | Table entry identifying episodes
type Entry = (SeriesName, SeasonCode, EpisodeCode, EpisodeTitle)

-- | Converts episode vector to list of identifying table entries
makeEntries :: V.Vector (SeriesName, SeasonEpisodeCode, D.Episode) -> [Entry]
makeEntries eps = let
    oneEntry entries (series, episode, ep) = let
          newentry = (series, season, epnum, D.title ep)
          [season, epnum] = T.splitOn "." episode
        in newentry:entries
  in V.foldl' oneEntry [] eps

templateFromFile :: SourceName -> IO (Template SourcePos)
templateFromFile fp = (parseGingerFile resolver fp) >>= \x -> case x of
                                                                Right t -> return t
                                                                Left e -> fail $ show e
    where resolver fn = tryIOError (loadFile fn) >>= \e -> case e of
                                                             Right contents -> return (Just contents)
                                                             Left _ -> return Nothing
          loadFile fn = openFile fn ReadMode >>= hGetContents

veryEasyRender :: (ToGVal (Run p (Writer Html) Html) v,
                   ToGVal (Run p (Writer Html) Html) p) =>
                  v -> Template p -> ActionM ()
veryEasyRender ctx tpl = html $ TL.fromStrict $ htmlSource $ easyRender ctx tpl

indexR :: ActionM ()
indexR = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "index.html"])
  veryEasyRender (M.empty :: M.HashMap T.Text T.Text) tpl

transR :: D.Episode -> T.Text -> T.Text -> ActionM ()
transR ep series episode = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "transcript.html"])
  ctx <- liftIO $ transcriptCtx ep series episode ""
  veryEasyRender ctx tpl

genR :: T.Text -> ActionM ()
genR raw = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "random.html"])
  ctx <- liftIO $ transcriptCtx (parseRaw "") "AI" "0.0" raw
  veryEasyRender ctx tpl

transcriptCtx :: D.Episode -> T.Text -> T.Text -> T.Text -> IO (M.HashMap T.Text T.Text)
transcriptCtx ep series episode debug = do
  let [season, epnum] = T.splitOn "." episode
  return $ M.fromList [("episode",
                        T.concat [T.toUpper $ series
                                 ," Season ", season
                                 ," Episode ", epnum
                                 ,": ", D.title ep])
                      ,("text", debug)
                      ,("parsed_transcript", showt ep)
                      ]



searchR :: V.Vector (T.Text, T.Text, D.Episode) -> T.Text -> T.Text -> T.Text -> T.Text -> ActionM ()
searchR eps query place person present = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "results.html"])
  let ctx = search eps query place person present
  veryEasyRender ctx tpl

aboutR :: ActionM ()
aboutR = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "about.html"])
  veryEasyRender (M.empty :: M.HashMap T.Text T.Text) tpl

transIndexR :: [Entry] -> ActionM ()
transIndexR epentries = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "transcript_index.html"])
  let ctx :: M.HashMap T.Text [M.HashMap T.Text T.Text] =
             M.fromList [("entries", map (\(p,q,r,s) -> M.fromList [ ("series", T.toUpper p)
                                                                   , ("seriesurl", p)
                                                                   , ("season", q) 
                                                                   , ("episode", r)
                                                                   , ("title", s)
                                                                   ]) epentries)]
  veryEasyRender ctx tpl


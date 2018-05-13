{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import ElasticSearch
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
import qualified Data.Text.IO as T
import qualified Data.Stargate as D
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Stargate.Parse as P
import Data.Stargate.IO hiding (Entry)
import Data.Stargate.Format
import Data.Maybe
import Data.Aeson
import Control.Monad.Trans.Writer.Lazy
import System.Environment
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import qualified Network.URI.Encode as U
import qualified Data.ByteString.Char8 as B8
import Text.Printf
import GHC.Exts
import Data.Either

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
  entries <- fmap sort readAllEntries
  let thd (_,_,c) = c
--let wordlist1 = V.fromList $ exprsToMarkov $ concat $ V.map (D.exprs . thd) eps
--let (toInt, toMarkov) = makeLookup wordlist1
--let wordlist = VU.convert $ V.map toInt wordlist1
--let succmap = id $! createMap2 wordlist

  port <- fmap (fromMaybe "5000") $ lookupEnv "PORT"
  scotty (read port :: Int) $ do
  get "/" $ indexR "index.html"
  get "/advanced" $ indexR "advanced.html"
  get "/transcripts/:series/:episode" $ do
    series <- param "series"                      
    episode <- param "episode"
    transR series episode
  get "/random" $ do
  --rand <- liftIO getStdGen
  --liftIO newStdGen
  --genR $ TW.wrapText TW.defaultWrapSettings 80 $ generateTrans rand wordlist succmap toInt toMarkov
    status serviceUnavailable503
  get "/adv_search" $ do
    query <- param "query"
    ctx <- liftIO $ advSearchCtx query
    searchR ctx
  get "/search" $ do
    query <- param "query"
    place <- param "place"
    person <- param "person"
    present <- param "present"
    ctx <- liftIO $ searchCtx False query place person present
    searchR ctx
  get "/about" aboutR
  get "/style.css" $ do
    setHeader "Content-Type" "text/css"
    file "style.css"
  get "/transcripts" $ do
    transIndexR entries

-- | Table entry identifying episodes
type Entry = (SeriesName, SeasonCode, EpisodeCode, EpisodeTitle)

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

indexR :: String -> ActionM ()
indexR t = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", t])
  veryEasyRender (M.empty :: M.HashMap T.Text T.Text) tpl

transR :: T.Text -> T.Text -> ActionM ()
transR series episode = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "transcript.html"])
  ctx <- liftIO $ transcriptCtx series episode
  veryEasyRender ctx tpl

genR :: T.Text -> ActionM ()
genR raw = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "random.html"])
  ctx <- liftIO $ transcriptCtx "AI" "0.0"
  veryEasyRender ctx tpl

transcriptCtx :: T.Text -> T.Text -> IO (M.HashMap T.Text T.Text)
transcriptCtx series episode = do
  let [season, epnum] = T.splitOn "." episode
  h <- openFile (joinPath ["pretty", T.unpack series, T.unpack episode]) ReadMode
  title <- T.hGetLine h
  transcript <- T.hGetContents h
  return $ M.fromList [("episode",
                        T.concat [T.toUpper $ series
                                 ," Season ", season
                                 ," Episode ", epnum
                                 ,": ", title])
                      ,("text", "")
                      ,("parsed_transcript", transcript)
                      ]

advSearchCtx :: T.Text -> IO (M.HashMap T.Text ResultsOrText)
advSearchCtx q = searchCtx True q "" "" ""

searchCtx :: Bool -> T.Text -> T.Text -> T.Text -> T.Text -> IO (M.HashMap T.Text ResultsOrText)
searchCtx adv query place person present = do
  let qstrs = [if not $ T.null query then ["speech: ", query] else []
              ,if not $ T.null place then ["place: ", place] else []
              ,if not $ T.null person then ["person: ", person] else []
              ,if not $ T.null present then ["present: ", present] else []
              ]
  let qstring = if adv
                then B8.pack $ U.encode $ T.unpack query
                else B8.pack $ U.encode $ T.unpack $ T.concat $ intercalate [" AND "] $ filter (not . null) qstrs
  initReq <- parseRequest "http://localhost:9200/stargate/_search"
  let myreq = initReq {
                queryString = if not $ B8.null qstring
                              then B8.concat ["?size=1000&q=", qstring]
                              else B8.empty
              }
  re :: Response (Either JSONException SearchResults) <- httpJSONEither myreq
  let r = getResponseBody re
  let err = case r of
        Left _ -> "yes"
        Right _ -> "no"
  let results = case r of
        Left _ -> SearchResults 0 False (Shards 0 0 0 0) (Hits 0 0.0 [])
        Right res -> res
  hits <- mapM (getHitContext 2) $ h_hits $ r_hits results
  let finalres = map hitToResult hits
  return $ M.fromList [("results", Results finalres)
                      ,("toomany", RText err)
                      ]

searchR :: M.HashMap T.Text ResultsOrText -> ActionM ()
searchR ctx = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "results.html"])
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

-- | Gets n hits before and after the hit, from the same scene. Asks ElasticSearch.
getHitContext :: Int -> Hit -> IO ([Hit], Hit, [Hit])
getHitContext n hit = do
  let src = h__source hit
  let qstring = B8.pack $ U.encode $ concat ["series:", s_series src, " AND "
                                            ,"seasnum:", printf "%d" $ s_seasnum src, " AND "
                                            ,"epnum:", printf "%d" $ s_epnum src, " AND "
                                            ,"sceneno:", printf "%d" $ s_sceneno src, " AND "
                                            ,"lineno:[", printf "%d" (s_lineno src - n)
                                            , " TO ", printf "%d" (s_lineno src + n), "]"
                                            ]
  initReq <- parseRequest "http://localhost:9200/stargate/_search"
  let myreq = initReq {
                queryString = if not $ B8.null qstring
                              then B8.concat ["?size=1000&q=", qstring]
                              else B8.empty
              }
  results <- httpJSON myreq
  let hits = h_hits $ r_hits $ getResponseBody results
  let l = s_lineno . h__source
  let lefthits = sortWith l $ filter (\h -> l h < s_lineno src) hits
  let righthits = sortWith l $ filter (\h -> l h > s_lineno src) hits
  return (lefthits, hit, righthits)

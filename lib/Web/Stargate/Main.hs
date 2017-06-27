{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.Stargate.Main (main) where
import Web.Scotty
import Text.Ginger hiding (length)
import Text.Ginger.Run
import Text.Ginger.Parse
import Text.Ginger.Html hiding (html)
import System.FilePath
import System.IO.Error
import System.IO
import Control.Monad.IO.Class
import Data.Either
import Data.List
import Data.Either.Unwrap
import qualified Data.HashMap.Lazy as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Web.Stargate.Parse (readTranscript)
import qualified Web.Stargate.Data as D
import System.FilePath.Glob hiding (match)
import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Char

main :: IO ()
main = readAllTranscripts >>= \eps -> scotty 5000 $ do
  let epentries = makeEntries eps
  get "/" indexR
  get "/transcripts/:series/:episode" $ do
    series <- param "series"                      
    episode <- param "episode"
    transR series episode
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
  get "/transcripts" $ transIndexR epentries

readAllTranscripts :: IO (M.HashMap (String, String) D.Episode)
readAllTranscripts = (forM ["sg1", "atl"] $ \series -> do
  fnames <- globDir1 (compile $ joinPath ["transcripts", series, "*"]) "."
  let readT f = readTranscript f >>= \t -> return ((series, last $ splitPath f), t)
  mapM readT fnames) >>= \m -> return $ M.fromList $ concat m

type Entry = (T.Text, T.Text, T.Text, T.Text)

makeEntries :: M.HashMap (String, String) D.Episode -> [Entry]
makeEntries eps = let
    oneEntry entries (series, episode) ep = let
          newentry = (T.pack series, season, epnum, T.pack "unknown")
          [season, epnum] = T.splitOn "." $ T.pack episode
        in newentry:entries
  in sort $ M.foldlWithKey' oneEntry [] eps

templateFromFile :: FilePath -> IO Template
templateFromFile fp = (parseGingerFile resolver fp) >>= \x -> case x of
                                                                Right t -> return t
                                                                Left e -> fail $ show e
    where resolver fn = tryIOError (loadFile fn) >>= \e -> case e of
                                                             Right contents -> return (Just contents)
                                                             Left err -> return Nothing
          loadFile fn = openFile fn ReadMode >>= hGetContents

indexR :: ActionM ()
indexR = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "index.html"])
  html $ TL.fromStrict $ htmlSource $ easyRender (M.empty :: M.HashMap T.Text T.Text) tpl

transR :: String -> String -> ActionM ()
transR series episode = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "transcript.html"])
  ctx <- liftIO $ transcriptCtx series episode
  html $ TL.fromStrict $ htmlSource $ easyRender ctx tpl

transcriptCtx :: String -> String -> IO (M.HashMap T.Text T.Text)
transcriptCtx series episode = do
  text <- T.readFile $ joinPath ["transcripts", series, episode]
  let [season, epnum] = T.splitOn "." $ T.pack episode
  return $ M.fromList [("episode", T.concat [T.toUpper $ T.pack series, " Season ", season, " Episode ", epnum])
                      ,("text", text)
                      ]

match :: String -> String -> Bool
match query body = (null query) || ((map toUpper query) `isInfixOf` (map toUpper body))

data TextOrList = Text T.Text
               | List [T.Text]

instance ToGVal m TextOrList where
    toGVal torm = case torm of
                    Text t -> toGVal t
                    List l -> toGVal l

data ResultsOrText = RText T.Text
                   | Results [M.HashMap T.Text TextOrList]

instance ToGVal m ResultsOrText where
    toGVal rort = case rort of
                    RText t -> toGVal t
                    Results t -> toGVal t

resultCtx :: M.HashMap (String, String) D.Episode -> String -> String -> String -> String -> M.HashMap T.Text ResultsOrText
resultCtx eps query place person present =
  let toomany = if (length results) >= 500 then "yes" else "no"
      results = M.foldlWithKey' epResults [] eps
      epResults res (series, episode) ep = let
          newmatches = foldl' sceneResults [] ep
          sceneResults ress scene = let
              newSceneResults = if or [ not $ any (match present) (D.present scene)
                                      , not $ match place (D.place scene)
                                      ]
                        then []
                        else lineMatches (D.speech scene)
              lineMatches speech = let
                  resultOrEmpty i = if (match person (fst $ speech !! i)) && (match query (snd $ speech !! i))
                        then [makeResult i]
                        else []
                  makeResult i = M.fromList [ ("url", Text $ T.pack $ concat ["/transcripts/", series, "/", episode])
                                            , ("context_before", List $ strline (max 0 (i-2)) i)
                                            , ("match", Text $ head $ strline i (i+1))
                                            , ("context_after", List $ strline (min (i+1) (length speech)) (min (i+3) (length speech)))
                                            , ("episode", Text $ T.pack $ concat [map toUpper series, " Season ", season, " Episode ", epnum])
                                            , ("place", Text $ T.pack $ D.place scene)
                                            ]
                  [season, epnum] = map T.unpack $ T.splitOn "." $ T.pack episode
                  strline x y = map (\pair -> T.pack $ concat [fst pair, ": ", snd pair]) (drop x $ take y speech)
                in concat $ map resultOrEmpty [0..(length speech)-1]
              in concat [newSceneResults, ress]
          in concat [newmatches, res]
  in M.fromList [("results", Results results)
                ,("toomany", RText toomany)
                ]

searchR :: M.HashMap (String, String) D.Episode -> String -> String -> String -> String -> ActionM ()
searchR eps query place person present = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "results.html"])
  let ctx = resultCtx eps query place person present
  html $ TL.fromStrict $ htmlSource $ easyRender ctx tpl

aboutR :: ActionM ()
aboutR = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "about.html"])
  html $ TL.fromStrict $ htmlSource $ easyRender (M.empty :: M.HashMap T.Text T.Text) tpl

transIndexR :: [Entry] -> ActionM ()
transIndexR epentries = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "transcript_index.html"])
  let ctx :: M.HashMap T.Text [M.HashMap T.Text T.Text] = M.fromList [("entries", map (\(p,q,r,s) -> M.fromList [ ("series", T.toUpper p)
                                                                                                                , ("seriesurl", p)
                                                                                                                , ("season", q) 
                                                                                                                , ("episode", r)
                                                                                                                , ("title", s)
                                                                                                                ]) epentries)]
  html $ TL.fromStrict $ htmlSource $ easyRender ctx tpl

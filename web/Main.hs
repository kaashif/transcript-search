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
import System.FilePath
import System.IO.Error
import System.IO
import Control.Monad.IO.Class
import Data.List
import qualified Data.HashMap.Lazy as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Stargate.Parse (readTranscript)
import qualified Data.Stargate as D
import System.FilePath.Glob hiding (match)
import Control.Monad
import qualified Data.Vector as V
import Data.Stargate.Search
import Data.Maybe
import Control.Monad.Trans.Writer.Lazy

instance ToGVal m TextOrList where
    toGVal torm = case torm of
                    Text t -> toGVal t
                    List l -> toGVal l

instance ToGVal m ResultsOrText where
    toGVal rort = case rort of
                    RText t -> toGVal t
                    Results t -> toGVal t

main :: IO ()
main = readAllTranscripts >>= \eps -> scotty 5000 $ do
  let epentries = makeEntries eps
  get "/" indexR
  get "/transcripts/:series/:episode" $ do
    series <- param "series"                      
    episode <- param "episode"
    transR (findTrans eps series episode) series episode
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
  get "/transcripts" $ transIndexR $ sort epentries

readAllTranscripts :: IO (V.Vector (T.Text, T.Text, D.Episode))
readAllTranscripts = fmap V.concat $ forM ["sg1", "atl"] $ \series -> do
  fnames <- globDir1 (compile $ joinPath ["transcripts", series, "*"]) "."
  let readT f = readTranscript f >>= \t -> return (T.pack series, T.pack $ last $ splitPath f, t)
  V.mapM readT (V.fromList fnames)

findTrans :: V.Vector (T.Text, T.Text, D.Episode) -> T.Text -> T.Text -> D.Episode
findTrans eps series episode = thd $ fromJust $ V.find (\(a, b, _) -> series == a && episode == b) eps
  where thd (_,_,c) = c

type Entry = (T.Text, T.Text, T.Text, T.Text)

makeEntries :: V.Vector (T.Text, T.Text, D.Episode) -> [Entry]
makeEntries eps = let
    oneEntry entries (series, episode, _) = let
          newentry = (series, season, epnum, "unknown")
          [season, epnum] = T.splitOn "." episode
        in newentry:entries
  in V.foldl' oneEntry [] eps

templateFromFile :: FilePath -> IO Template
templateFromFile fp = (parseGingerFile resolver fp) >>= \x -> case x of
                                                                Right t -> return t
                                                                Left e -> fail $ show e
    where resolver fn = tryIOError (loadFile fn) >>= \e -> case e of
                                                             Right contents -> return (Just contents)
                                                             Left _ -> return Nothing
          loadFile fn = openFile fn ReadMode >>= hGetContents

veryEasyRender :: ToGVal (Run (Writer Html) Html) v => v -> Template -> ActionM ()
veryEasyRender ctx tpl = html $ TL.fromStrict $ htmlSource $ easyRender ctx tpl

indexR :: ActionM ()
indexR = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "index.html"])
  veryEasyRender (M.empty :: M.HashMap T.Text T.Text) tpl

transR :: D.Episode -> T.Text -> T.Text -> ActionM ()
transR ep series episode = do
  tpl <- liftIO $ templateFromFile (joinPath ["templates", "transcript.html"])
  ctx <- liftIO $ transcriptCtx ep series episode
  veryEasyRender ctx tpl

transcriptCtx :: D.Episode -> T.Text -> T.Text -> IO (M.HashMap T.Text T.Text)
transcriptCtx ep series episode = do
  t <- T.readFile $ joinPath ["transcripts", T.unpack series, T.unpack episode]
  let [season, epnum] = T.splitOn "." episode
  return $ M.fromList [("episode", T.concat [T.toUpper $ series, " Season ", season, " Episode ", epnum])
                      ,("text", t)
                      ,("parsed_transcript", makeText ep)
                      ]

makeText :: D.Episode -> T.Text
makeText ep = T.concat ["TEASER\n", V.foldl' makeSceneText T.empty ep, "\nEND CREDITS"]
  where makeSceneText soFar scene = if D.place scene == "nowhere"
                                    then soFar
                                    else T.concat [soFar, "\nLOCATION--", D.place scene, "\n\n", T.intercalate "\n" (V.toList $ V.map lineText $ D.speech scene)]
        lineText (c,l) = T.concat ["  ", c, "\n", indent 5 50 l, "\n"]
        indent n col l = T.intercalate "\n" $ map (\t -> T.concat [T.replicate n " ", t]) $ wordChunk col l

wordChunk :: Int -> T.Text -> [T.Text]
wordChunk col t = foldl' newOrAdd [] $ T.words t
  where newOrAdd ls word = if null ls || ((T.length $ T.concat [last ls, " ", word]) > col)
                           then ls ++ [word]
                           else init ls ++ [T.concat [last ls, " ", word]]

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
  let ctx :: M.HashMap T.Text [M.HashMap T.Text T.Text] = M.fromList [("entries", map (\(p,q,r,s) -> M.fromList [ ("series", T.toUpper p)
                                                                                                                , ("seriesurl", p)
                                                                                                                , ("season", q) 
                                                                                                                , ("episode", r)
                                                                                                                , ("title", s)
                                                                                                                ]) epentries)]
  veryEasyRender ctx tpl
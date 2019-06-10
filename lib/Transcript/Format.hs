{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Transcript.Format where
import TextShow
import Data.Monoid
import qualified Transcript as D
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.List

instance TextShow D.Episode where
    showb e = mconcat [fromText "TEASER\n"
                      ,showb (D.scenes e)
                      ,fromText "\nEND CREDITS"]

instance TextShow (V.Vector D.Scene) where
    showb ss = V.foldl' showSceneConcat mempty ss
        where showSceneConcat soFar scene = if D.place scene == "nowhere"
                                            then soFar
                                            else soFar <> (showb scene)

instance TextShow D.Scene where
    showb scene = mconcat [fromText "\nLOCATION--"
                          ,fromText $ D.place scene
                          ,fromText "\n\n"
                          ,unlinesB (V.toList $ V.map showb $ D.speech scene)]

instance TextShow D.SpeechLine where
    showb (D.SpeechLine (c,l)) = mconcat [fromText "  "
                                         ,fromText c
                                         ,fromText "\n"
                                         ,fromText $ indent 5 50 l
                                         ,fromText "\n"]

-- | Converts a line of text to a block indented n spaces, wrapped to col characters
indent :: Int -> Int -> T.Text -> T.Text
indent n col l = T.intercalate "\n" $ map (\t -> T.concat [T.replicate n " ", t]) $ wordChunk col l

-- | Cuts a line of text into lines of length <= col characters,
-- | not cutting in the middle of a word
wordChunk :: Int -> T.Text -> [T.Text]
wordChunk col t = foldl' newOrAdd [] $ T.words t
  where newOrAdd ls word = if null ls || ((T.length $ T.concat [last ls, " ", word]) > col)
                           then ls ++ [word]
                           else init ls ++ [T.concat [last ls, " ", word]]

-- We can also show an episode in a fancier HTML format, making each
-- line a code block so we can display it with line numbers and with
-- links to each line
class HtmlShow a where
    showh :: a -> T.Text

instance HtmlShow D.Episode where
    showh e = T.concat [ "TEASER\n"
                       , showh (D.scenes e)
                       , "\nEND CREDITS"]

instance HtmlShow (V.Vector D.Scene) where
    showh ss = V.foldl' showSceneConcat mempty ss
        where showSceneConcat soFar scene = if D.place scene == "nowhere"
                                            then soFar
                                            else soFar <> (showh scene)

instance HtmlShow D.Scene where
    showh scene = T.concat [ "\nLOCATION--"
                           , D.place scene
                           , "\n\n"
                           , T.unlines (V.toList $ V.map showh $ D.speech scene)]

instance HtmlShow D.SpeechLine where
    showh (D.SpeechLine (c,l)) = T.concat [ "<code id=\"Lxx\">xx\n"
                                          , "  "
                                          , c
                                          , "\n"
                                          , indent 5 50 l
                                          , "\n"
                                          , "</code>\n"]

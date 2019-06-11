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

tr :: T.Text -> T.Text -> T.Text -> T.Text
tr d1 d2 c = T.concat [if T.null c then "<tr>" else T.concat ["<tr class=\"",c,"\">"]
                      ,"<td>",d1,"</td>"
                      ,"<td>",d2,"</td>"
                      ,"</tr>\n"]

lineno :: T.Text -> T.Text -> T.Text
lineno no line = T.concat ["<tr id=\"L",no,"\">"
                          ,"<td><a href=\"#L",no,"\">",no,"</a></td>"
                          ,"<td>",line,"</td>"
                          ,"</tr>\n"]

instance HtmlShow D.Episode where
    showh e = T.concat [ "<table class=\"table table-striped\">\n"
                       , tr "" (D.title e) ""
                       , tr "" "TEASER" ""
                       , showh $ V.zip (V.init $ V.scanl (+) 1 $ V.map countlines scenes) scenes
                       , tr "" "END CREDITS" ""
                       , "</table>\n"
                       ]
        where scenes = V.filter (\s -> (D.place s) /= "nowhere") $ D.scenes e
              countlines = V.length . D.speech

instance HtmlShow (V.Vector (Int, D.Scene)) where
    showh ss = T.concat $ V.toList $ V.map showh ss

instance HtmlShow (Int, D.Scene) where
    showh (one, scene) = T.concat [ tr "" (T.concat ["LOCATION--", D.place scene]) ""
                                  , T.unlines (V.toList $ V.map showh lineSpeech)]
        where lineSpeech = V.zip (V.fromList [one..one+(V.length sp)]) sp
              sp = D.speech scene

instance HtmlShow (Int, D.SpeechLine) where
    showh (n, D.SpeechLine(c,l)) = lineno (showt n) (T.concat [c, ": ", l])

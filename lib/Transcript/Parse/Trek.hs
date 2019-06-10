{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Transcript.Parse.Trek where

import qualified Data.Text as T
import qualified Data.Set as S
import Transcript
import qualified Data.Vector as V

parse :: T.Text -> Maybe [ScriptExpr]
parse raw = if length lines < 3
  then Nothing -- we expect 3 lines, the title, airdate, then stardate
  else Just $ (Title $ head lines):(concat $ map parseRest $ drop 3 lines)
  where lines = T.lines raw

-- Takes exactly 1 line, changes it into a scriptexpr
parseRest :: T.Text -> [ScriptExpr]
parseRest l
  | T.null l = []
  | T.head l == '(' && T.last l == ')' = [Annotation $ T.drop 1 $ T.init l]
  | T.head l == '[' && T.last l == ']' = [Place Exterior $ T.drop 1 $ T.init l]
  | ":" `T.isInfixOf` l = [Speech (head splat) (T.intercalate ":" $ tail splat)]
  -- sometimes there is an unattributed line (e.g. some captains logs,
  -- that text at the start of Emissary or Caretaker), and we need to
  -- include it but the script doesn't tell us who said it
  | otherwise = [Speech "SOMEONE" l]
  where splat = T.splitOn ":" l

convert :: [ScriptExpr] -> Episode
convert es = reversed { scenes = V.reverse $ scenes reversed, exprs = es }
    where empty = Scene Exterior "nowhere" S.empty V.empty
          reversed = convert' "" (V.singleton empty) es

convert' :: T.Text -> V.Vector Scene -> [ScriptExpr] -> Episode
convert' title ep [] = Episode ep title []
convert' title scs' (ex:exs) = let
    sc = V.head scs'
    scs = V.tail scs'
  in case ex of
  Title t -> convert' t scs' exs
  Speech c l -> convert' title (V.cons newsc scs) exs
      where newsc = sc { present = S.insert c (present sc)
                       , speech = V.concat [speech sc, V.singleton $ SpeechLine (c, l)]
                       }
  Place intext p -> convert' title (V.cons newsc $ V.cons sc scs) exs
      where newsc = Scene intext p S.empty V.empty
  Annotation ann -> convert' title (V.cons newsc scs) exs
      where newsc = sc { speech = V.concat [speech sc, V.singleton $ SpeechLine ("ANNOTATION", ann)]
                       }
  _ -> convert' title (V.cons sc scs) exs

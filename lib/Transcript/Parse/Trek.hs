{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Transcript.Parse.Trek where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Char
import qualified Data.Set as S
import Transcript
import qualified Data.Vector as V

scriptp :: Parser [ScriptExpr]
scriptp = do
  title <- takeTill (=='\n')
  char '\n'
  stardate <- takeTill (=='\n')
  char '\n'
  airdate <- takeTill (=='\n')
  char '\n'
  exprs <- many (choice [annotationp, placep, speechp])
  return $ (Title title):exprs

annotationp :: Parser ScriptExpr
annotationp = do
  string "("
  ann <- fmap (T.map (\c -> if c=='\n' then ' ' else c)) $ (takeWhile1 (\c -> isLatin1 c && c /= ')'))
  string ")\n"
  return $ Annotation ann

placep :: Parser ScriptExpr
placep = do
  string "["
  place <- fmap (T.map (\c -> if c=='\n' then ' ' else c)) $ (takeWhile1 (\c -> isLatin1 c && c /= ']'))
  string "]\n"
  return $ Place Exterior place

speechp :: Parser ScriptExpr
speechp = do
  cname <- takeTill (`elem` ['\n', ':'])
  c <- peekChar
  case c of
    Just ':' -> do
      string ": "
      speech <- takeTill (=='\n')
      char '\n'
      return $ Speech cname speech
    _ -> do
      -- Occasionally there is a line that no-one is saying, like the
      -- on-screen text at the start of Caretaker or Emissary. This is
      -- rare. More common are captain's logs, which *should* have a person
      -- attributed, but often don't. We can't tell programmatically what
      -- the situation is, who is doing the log, so we just say "someone" is
      -- saying it.
      char '\n'
      return $ Speech "SOMEONE" cname

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

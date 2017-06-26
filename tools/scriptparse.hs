#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Environment (getArgs)
import Data.Char
import Data.List
import qualified Data.Set as S
import GHC.Generics
import Data.Aeson
import Data.Aeson.Encode.Pretty

type Character = String

data ScriptExpr = Place String
                | Annotation String
                | Speech Character String
                | Junk String
                  deriving Show

main :: IO ()
main = do
  args <- getArgs
  fname <- fmap head getArgs
  let wantconvert = (length args) >= 2
  rawscript <- fmap T.decodeLatin1 $ BS.readFile fname
  case (parse scriptp fname rawscript) of
    Left err -> print err
    Right exprs -> if wantconvert
                   then BSL.putStr (encode $ convert exprs)
                   else print exprs

-- Here are the parsers for converting transcripts into a
-- machine-readable representation. The ScriptExpr format is not
-- really fully done yet, though, we need to associate speech with
-- places, who else was present, etc.

scriptp :: Parser [ScriptExpr]
scriptp = many (choice [try junkp, try annotationp, try placep, try speechp])

junkp :: Parser ScriptExpr
junkp = do
  j <- choice $ try excerptp : map (try . string) [ "ROLL CREDITS"
                                                  , "TEASER"
                                                  , "TO BE CONTINUED"
                                                  , "FADE OUT"
                                                  , "THE END"
                                                  , "BEGIN EXCERPTS"
                                                  , "END CREDITS"
                                                  , "ROLL END CREDITS"
                                                  , "CREDITS"
                                                  , "END TEASER"
                                                  , "END EXCERPT"
                                                  , "END EXCERPTS"
                                                  , "END FLASHBACK"
                                                  , "OPENING CREDITS"
                                                  , "CLOSING CREDITS"
                                                  , "END TEASER--OPENING CREDITS"
                                                  , "END OF TEASER--OPENING CREDITS"
                                                  ]
  skipMany $ string "\n"
  return $ Junk j

excerptp :: Parser String
excerptp = fmap concat $ sequence [ try (string "EXCERPT") <|> try (string "EXCERPTS") <|> try (string "FLASHBACK")
                                  , many (noneOf "\n")
                                  ]

annotationp :: Parser ScriptExpr
annotationp = do
  string "["
  ann <- fmap (map (\c -> if c=='\n' then ' ' else c)) $ many (satisfy (\c -> isLatin1 c && c /= ']'))
  string "]\n"
  return $ Annotation ann

headerChar :: Parser Char
headerChar = satisfy (\c -> isLatin1 c && not (isLower c) && c/='\n')

namep :: Parser String
namep = many headerChar

placep :: Parser ScriptExpr
placep = do
  try (string "INT--") <|> (string "EXT--")
  name <- namep
  skipMany $ string "\n"
  return $ Place name
  
speechlinep :: Parser String
speechlinep = do
  l <- many $ satisfy (\c -> isLatin1 c && c /= '\n')
  skipMany $ string "\n"
  return l

speechp :: Parser ScriptExpr
speechp = do
  cname <- namep
  string "\n"
  ls <- manyTill speechlinep (try $ lookAhead ps)
  return $ Speech cname (intercalate " " ls)
      where namelinep = (Junk . concat) <$> sequence [namep, string "\n"]
            ps = choice $ map try [junkp, placep, annotationp, namelinep]

-- Here we transform [ScriptExpr] into an episode, where all the data
-- is properly associated and we can later serialize it.

data Scene = Scene {
      place :: String,
      present :: S.Set String,
      speech :: [(Character, String)]
    } deriving (Generic, Show)
instance ToJSON Scene

type Episode = [Scene]

convert :: [ScriptExpr] -> Episode
convert es = reverse $ convert' [empty] (filter p es)
    where p e = case e of
                  Junk s -> False
                  _ -> True
          empty = Scene "nowhere" S.empty []

convert' :: Episode -> [ScriptExpr] -> Episode
convert' ep [] = ep
convert' (sc:scs) (ex:exs) = case ex of
  Speech c l -> convert' (newsc:scs) exs
      where newsc = sc { present = S.insert c (present sc)
                       , speech = speech sc ++ [(c, l)]
                       }
  Place p -> convert' (newsc:sc:scs) exs
      where newsc = Scene p S.empty []
  Annotation ann -> convert' (newsc:scs) exs
      where newsc = sc { speech = speech sc ++ [("ANNOTATION", ann)] }
  _ -> convert' (sc:scs) exs

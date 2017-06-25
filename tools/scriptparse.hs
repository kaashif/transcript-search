{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Text.Parsec
import Text.Parsec.Char
import System.IO (readFile)
import System.Environment (getArgs)
import Data.Char
import Data.List

type Character = String

data ScriptExpr = Place String
                | Annotation String
                | Speech Character String
                | Junk String
                  deriving Show

main :: IO ()
main = do
  fname <- fmap head getArgs
  rawscript <- readFile fname
  case (parse scriptp fname rawscript) of
    Left err -> print err
    Right exprs -> mapM_ print exprs

scriptp :: Stream s m Char => ParsecT s u m [ScriptExpr]
scriptp = many (choice [try junkp, try annotationp, try placep, try speechp])

junkp :: Stream s m Char => ParsecT s u m ScriptExpr
junkp = do
  j <- choice $ concat [map (try . string) [ "ROLL CREDITS"
                                           , "TEASER"
                                           , "FADE OUT"
                                           , "BEGIN EXCERPTS"
                                           , "END CREDITS"
                                           , "END TEASER"
                                           , "END EXCERPT"
                                           , "END EXCERPTS"
                                           , "OPENING CREDITS"
                                           , "END TEASER--OPENING CREDITS"
                                           , "END OF TEASER--OPENING CREDITS"
                                           ]
                       , [try excerptp]
                       ]
  skipMany $ string "\n"
  return $ Junk j

excerptp :: Stream s m Char => ParsecT s u m String
excerptp = do
  a <- try (string "EXCERPT") <|> string "EXCERPTS"
  b <- many (noneOf "\n")
  return (a ++ b)

annotationp :: Stream s m Char => ParsecT s u m ScriptExpr
annotationp = do
  string "["
  ann <- fmap (map (\c -> if c=='\n' then ' ' else c)) $ many (satisfy (\c -> isLatin1 c && c /= ']'))
  string "]\n"
  return $ Annotation ann

headerChar :: Stream s m Char => ParsecT s u m Char
headerChar = upper <|> digit <|> oneOf " '-#(),."

namep :: Stream s m Char => ParsecT s u m String
namep = many headerChar

placep :: Stream s m Char => ParsecT s u m ScriptExpr
placep = do
  try (string "INT--") <|> (string "EXT--")
  name <- namep
  skipMany $ string "\n"
  return $ Place name
  
speechlinep :: Stream s m Char => ParsecT s u m String
speechlinep = do
  l <- many $ satisfy (\c -> isLatin1 c && not (elem c "[]\n"))
  skipMany $ string "\n"
  return l

speechp :: Stream s m Char => ParsecT s u m ScriptExpr
speechp = do
  cname <- namep
  string "\n"
  ls <- manyTill speechlinep (try $ lookAhead ps)
  return $ Speech cname (intercalate " " ls)
      where namelinep = (Junk . concat) <$> sequence [namep, string "\n"]
            ps = choice $ map try [junkp, placep, annotationp, namelinep]

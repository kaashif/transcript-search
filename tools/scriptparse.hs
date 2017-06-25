{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Text.Parsec
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
  parseTest scriptp rawscript

scriptp :: Stream s m Char => ParsecT s u m [ScriptExpr]
scriptp = many (choice [try junkp, try annotationp, try placep, try speechp])

junkp :: Stream s m Char => ParsecT s u m ScriptExpr
junkp = do
  j <- string "ROLL CREDITS"
        <|> string "TEASER"
        <|> string "FADE OUT"
        <|> string "END CREDITS"
  skipMany $ string "\n"
  return $ Junk j

annotationp :: Stream s m Char => ParsecT s u m ScriptExpr
annotationp = do
  string "["
  ann <- fmap (map (\c -> if c=='\n' then ' ' else c)) $ many (satisfy (\c -> isLatin1 c && c /= ']'))
  string "]\n"
  return $ Annotation ann

isHeaderChar :: Char -> Bool
isHeaderChar c = elem c "ABCDEFGHIJKLMNOPQRSTUVYXYZ-' "

namep :: Stream s m Char => ParsecT s u m String
namep = many (satisfy isHeaderChar)

placep :: Stream s m Char => ParsecT s u m ScriptExpr
placep = do
  try (string "INT--") <|> string "EXT--"
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
      where namelinep = fmap Junk (namep >> string "\n")
            ps = choice [junkp, placep, annotationp, namelinep]

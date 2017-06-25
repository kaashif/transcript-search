{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import System.Environment (getArgs)
import Data.Char
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

type Character = String

data ScriptExpr = Place String
                | Annotation String
                | Speech Character String
                | Junk String
                  deriving Show

main :: IO ()
main = do
  fname <- fmap head getArgs
  rawscript <- fmap T.decodeLatin1 $ BS.readFile fname
  case (parse scriptp fname rawscript) of
    Left err -> print err
    Right exprs -> mapM_ print exprs

scriptp :: Parser [ScriptExpr]
scriptp = many (choice [try junkp, try annotationp, try placep, try speechp])

junkp :: Parser ScriptExpr
junkp = do
  j <- choice $ try excerptp : map (try . string) [ "ROLL CREDITS"
                                                  , "TEASER"
                                                  , "FADE OUT"
                                                  , "THE END"
                                                  , "BEGIN EXCERPTS"
                                                  , "END CREDITS"
                                                  , "END TEASER"
                                                  , "END EXCERPT"
                                                  , "END EXCERPTS"
                                                  , "OPENING CREDITS"
                                                  , "END TEASER--OPENING CREDITS"
                                                  , "END OF TEASER--OPENING CREDITS"
                                                  ]
  skipMany $ string "\n"
  return $ Junk j

excerptp :: Parser String
excerptp = fmap concat $ sequence [ try (string "EXCERPT") <|> string "EXCERPTS"
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

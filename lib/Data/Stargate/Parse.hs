{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Stargate.Parse (readTranscript) where

import Text.Parsec hiding (Parser, string)
import qualified Text.Parsec as P (string)
import Text.Parsec.Text (Parser)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Environment (getArgs)
import Data.Char
import Data.List
import qualified Data.Set as S
import System.IO
import Data.Stargate
import qualified Data.Vector as V
import Data.Vector ((!))

readTranscript :: FilePath -> IO Episode
readTranscript fname = do
  rawscript <- fmap T.decodeLatin1 $ BS.readFile fname
  case (parse scriptp fname rawscript) of
    Left err -> fail $ show err
    Right exprs -> return $ convert exprs

scriptp :: Parser [ScriptExpr]
scriptp = many (choice [try junkp, try annotationp, try placep, try speechp])

string :: String -> Parser T.Text
string s = fmap T.pack (P.string s)

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

excerptp :: Parser T.Text
excerptp = fmap T.concat $ sequence [ try (string "EXCERPT") <|> try (string "EXCERPTS") <|> try (string "FLASHBACK")
                                  , fmap T.pack $ many (noneOf "\n")
                                  ]

annotationp :: Parser ScriptExpr
annotationp = do
  string "["
  ann <- fmap (map (\c -> if c=='\n' then ' ' else c)) $ many (satisfy (\c -> isLatin1 c && c /= ']'))
  string "]\n"
  return $ Annotation $ T.pack ann

headerChar :: Parser Char
headerChar = satisfy (\c -> isLatin1 c && not (isLower c) && c/='\n')

namep :: Parser T.Text
namep = fmap T.pack $ many headerChar

placep :: Parser ScriptExpr
placep = do
  try (string "INT--") <|> (string "EXT--")
  name <- namep
  skipMany $ string "\n"
  return $ Place name
  
speechlinep :: Parser T.Text
speechlinep = do
  l <- many $ satisfy (\c -> isLatin1 c && c /= '\n')
  skipMany $ string "\n"
  return $ T.pack l

speechp :: Parser ScriptExpr
speechp = do
  cname <- namep
  string "\n"
  ls <- manyTill speechlinep (try $ lookAhead ps)
  return $ Speech cname (T.intercalate " " ls)
      where namelinep = (Junk . T.concat) <$> sequence [namep, string "\n"]
            ps = choice $ map try [junkp, placep, annotationp, namelinep]

convert :: [ScriptExpr] -> Episode
convert es = V.reverse $ convert' (V.singleton empty) (filter p es)
    where p e = case e of
                  Junk s -> False
                  _ -> True
          empty = Scene "nowhere" S.empty V.empty V.empty

convert' :: Episode -> [ScriptExpr] -> Episode
convert' ep [] = ep
convert' scs' (ex:exs) = let
    sc = V.head scs'
    scs = V.tail scs'
  in case ex of
  Speech c l -> convert' (V.cons newsc scs) exs
      where newsc = sc { present = S.insert c (present sc)
                       , speech = V.concat [speech sc, V.singleton (c, l)]
                       , upperspeech = V.concat [upperspeech sc, V.singleton (c, T.toUpper l)]
                       }
  Place p -> convert' (V.cons newsc $ V.cons sc scs) exs
      where newsc = Scene p S.empty V.empty V.empty
  Annotation ann -> convert' (V.cons newsc scs) exs
      where newsc = sc { speech = V.concat [speech sc, V.singleton ("ANNOTATION", ann)]
                       , upperspeech = V.concat [speech sc, V.singleton ("ANNOTATION", T.toUpper ann)]
                       }
  _ -> convert' (V.cons sc scs) exs

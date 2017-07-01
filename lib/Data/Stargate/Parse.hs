{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Stargate.Parse where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
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

scriptp :: Parser [ScriptExpr]
scriptp = many (choice [junkp, annotationp, placep, speechp])

junkp :: Parser ScriptExpr
junkp = do
  j <- choice $ try excerptp : map string [ "ROLL CREDITS"
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
  char '\n'
  return $ Junk j

excerptp :: Parser T.Text
excerptp = fmap T.concat $ sequence [ (string "EXCERPT") <|> (string "EXCERPTS") <|> (string "FLASHBACK")
                                    , takeWhile1 (/='\n')
                                    ]

annotationp :: Parser ScriptExpr
annotationp = do
  string "["
  ann <- fmap (T.map (\c -> if c=='\n' then ' ' else c)) $ (takeWhile1 (\c -> isLatin1 c && c /= ']'))
  string "]\n"
  return $ Annotation ann

headerChar :: Char -> Bool
headerChar c = isLatin1 c && not (isLower c) && c/='\n'

namep :: Parser T.Text
namep = takeWhile1 headerChar

placep :: Parser ScriptExpr
placep = do
  intext <- (string "INT--") <|> (string "EXT--")
  name <- namep
  skip (=='\n')
  return $ Place (if "INT" `T.isPrefixOf` intext then Interior else Exterior) name
  
speechlinep :: Parser T.Text
speechlinep = do
  l <- takeWhile1 (\c -> isLatin1 c && c /= '\n')
  skip (=='\n')
  return l

speechp :: Parser ScriptExpr
speechp = do
  cname <- namep
  string "\n"
  ls <- manyTill speechlinep (lookAhead ps)
  return $ Speech cname (T.intercalate " " ls)
      where namelinep = (Junk . T.concat) <$> sequence [namep, string "\n"]
            ps = choice [junkp, placep, annotationp, namelinep]

convert :: [ScriptExpr] -> Episode
convert es = V.reverse $ convert' (V.singleton empty) es
    where empty = Scene Exterior "nowhere" S.empty V.empty V.empty

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
  Place intext p -> convert' (V.cons newsc $ V.cons sc scs) exs
      where newsc = Scene intext p S.empty V.empty V.empty
  Annotation ann -> convert' (V.cons newsc scs) exs
      where newsc = sc { speech = V.concat [speech sc, V.singleton ("ANNOTATION", ann)]
                       , upperspeech = V.concat [speech sc, V.singleton ("ANNOTATION", T.toUpper ann)]
                       }
  _ -> convert' (V.cons sc scs) exs

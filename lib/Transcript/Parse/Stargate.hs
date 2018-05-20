{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Transcript.Parse.Stargate where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator
import qualified Data.Text as T
import Data.Char
import qualified Data.Set as S
import Transcript
import qualified Data.Vector as V

scriptp :: Parser [ScriptExpr]
scriptp = do
  title <- titlep
  exprs <- many (choice [junkp, annotationp, placep, speechp])
  return $ title:exprs

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

titlep :: Parser ScriptExpr
titlep = do
  string "TITLE\n"
  t <- takeWhile1 (/='\n')
  string "\n"
  return $ Title t

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

{-# LANGUAGE OverloadedStrings #-}
module Data.Stargate.Markov where
import qualified Data.Text as T
import qualified Data.Stargate as D
import System.Random
import qualified Data.MarkovChain as MC

-- | Lists of these are used to train the model
data MarkovExpr = Word T.Text
                | Place D.IntExt T.Text
                | Speech D.Character
                  deriving (Ord, Eq)


-- | Converts expressions to transcript text
markovToText :: [MarkovExpr] -> T.Text
markovToText l = T.replace "\n\n" "\n" $ T.concat ["TITLE\nMarkov Chain Generated Transcript", markovToText' l]

markovToText' :: [MarkovExpr] -> T.Text
markovToText' [] = T.empty
markovToText' (e:es) = T.concat [one e, markovToText' es]
    where one (Word t) = T.concat [t, " "]
          one (Place _ t) = T.concat ["\nINT--", t]
          one (Speech c) = T.concat ["\n", c, "\n"]

-- | Cuts up fully parsed expressions into trainable chunks
exprToMarkov :: D.ScriptExpr -> [MarkovExpr]
exprToMarkov (D.Place i t) = [Place i t]
exprToMarkov (D.Annotation t) = (Speech "STAGE DIRECTION"):(map Word $ T.words t)
exprToMarkov (D.Speech c t) = (Speech c):(map Word $ T.words t)
exprToMarkov _ = []

-- | Convenience concat map ified version of exprToMarkov
exprsToMarkov :: [D.ScriptExpr] -> [MarkovExpr]
exprsToMarkov = concat . map exprToMarkov

-- | Trains and runs a new Markov chain model, generating raw transcript text
generateTrans :: StdGen -> [MarkovExpr] -> T.Text
generateTrans rand wordlist = markovToText $ take 5000 $ MC.run 2 wordlist 0 rand


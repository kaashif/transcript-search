{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Stargate.Markov where
import qualified Data.Text as T
import qualified Data.Stargate as D
import System.Random
import qualified Data.HashMap.Lazy as M
import qualified Data.Hashable as M
import System.Random (RandomGen, randomR)
import qualified Data.Vector as V
import Data.Vector ((!))
import Control.Monad.ST
import Data.STRef
import Control.Monad
import Data.Maybe
import qualified Data.Sequence as S
import Data.Sequence ((|>))
import Data.Foldable (toList)
import GHC.Generics

-- | Lists of these are used to train the model
data MarkovExpr = Word T.Text
                | Place D.IntExt T.Text
                | Speech D.Character
                  deriving (Ord, Eq, Generic)

instance M.Hashable D.IntExt
instance M.Hashable MarkovExpr

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
generateTrans :: StdGen -> V.Vector MarkovExpr -> M.HashMap (MarkovExpr, MarkovExpr) (V.Vector MarkovExpr) -> T.Text
generateTrans rand wordlist succmap = markovToText $ run 2 wordlist 0 rand 5000 succmap

run :: (Eq a, M.Hashable a, RandomGen g) =>
      Int  -- ^ size of prediction context
   -> V.Vector a  -- ^ training sequence, the one to walk through randomly
   -> Int  -- ^ index to start the random walk within the training sequence
   -> g    -- ^ random generator state
   -> Int -- length of sequence to generate
   -> M.HashMap (a, a) (V.Vector a)
   -> [a] -- random result
run n wordlist start g len succmap = runST $ do
  randgen <- newSTRef g
  result <- newSTRef S.empty
  -- start off with some initial words
  modifySTRef result (\s -> s |> (wordlist ! 0))
  modifySTRef result (\s -> s |> (wordlist ! 1))
  forM_ [1..len] $ \i -> do
    s <- readSTRef result
    let (w1, w2) = (S.index s (S.length s - 2), S.index s (S.length s -1))
    gen <- readSTRef randgen
    let (next,newgen) = randomElem gen $ fromJust $ M.lookup (w1,w2) succmap
    modifySTRef result (\s -> s |> next)
    writeSTRef randgen newgen
  readSTRef result >>= return . toList

randomElem :: RandomGen g => g -> V.Vector a -> (a, g)
randomElem g v = (v ! index, newg)
  where (index, newg) = randomR (0, (V.length v) - 1) g

-- Creates a map of [n words] -> [possible successor words]
createMap2 :: (Eq a, M.Hashable a) => V.Vector a -> M.HashMap (a, a) (V.Vector a)
createMap2 wordlist = runST $ do
  succmap <- newSTRef (M.empty :: M.HashMap (a, a) (V.Vector a))
  forM_ [0..(V.length wordlist)-1-2] $ \i -> do
    let (w1, w2, successor) = (wordlist ! i, wordlist ! (i+1), wordlist ! (i+2))
    modifySTRef succmap (M.insertWith (V.++) (w1,w2) (V.singleton successor))
  readSTRef succmap >>= return

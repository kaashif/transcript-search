{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
module Data.Stargate.Markov where
import qualified Data.Text as T
import qualified Data.Stargate as D
import System.Random
import qualified Data.HashMap.Strict as M
import qualified Data.Hashable as M
import System.Random (RandomGen, randomR)
import qualified Data.Vector as V
import Data.Vector ((!))
import Control.Monad.ST.Strict
import Data.STRef
import Control.Monad
import Data.Maybe
import GHC.Generics
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as VU

-- | Lists of these are used to train the model
data MarkovExpr = Word T.Text
                | Place D.IntExt T.Text
                | Speech D.Character
                  deriving (Ord, Eq, Generic)

instance M.Hashable D.IntExt
instance M.Hashable MarkovExpr

-- | Converts expressions to transcript text
markovToText :: MarkovExpr -> T.Text
markovToText (Word t) = T.concat [t, " "]
markovToText (Place _ t) = T.concat ["\nINT--", t]
markovToText (Speech c) = T.concat ["\n", c, "\n"]

-- | Cuts up fully parsed expressions into trainable chunks
exprToMarkov :: D.ScriptExpr -> [MarkovExpr]
exprToMarkov (D.Place i t) = [Place i t]
exprToMarkov (D.Annotation t) = (Speech "STAGE DIRECTION"):(map Word $ T.words t)
exprToMarkov (D.Speech c t) = (Speech c):(map Word $ T.words t)
exprToMarkov _ = []

-- | Convenience concat map ified version of exprToMarkov
exprsToMarkov :: [D.ScriptExpr] -> [MarkovExpr]
exprsToMarkov = concat . map exprToMarkov

-- | Runs a given Markov chain model, generating raw transcript text
generateTrans :: StdGen
              -> VU.Vector Int
              -> M.HashMap (Int, Int) (VU.Vector Int)
              -> (MarkovExpr -> Int)
              -> (Int -> MarkovExpr)
              -> T.Text
generateTrans rand wordlist succmap toInt toMarkov = T.replace "\n\n" "\n" $ T.append title trans
    where title = "TITLE\nMarkov Chain Generated Transcript\n"
          trans = T.concat $ V.toList $ V.map (markovToText . toMarkov) $ VU.convert $ run 2 wordlist 0 rand 5000 succmap

-- | Makes a lookup table and its inverse (saves space, lets us use unboxed stuff)
makeLookup :: V.Vector MarkovExpr -> (MarkovExpr -> Int, Int -> MarkovExpr)
makeLookup v = runST $ do
  toIntMap <- newSTRef M.empty
  toMarkovMap <- newSTRef M.empty
  count <- newSTRef 0
  forM_ [0..(V.length v)-1] $ \i -> do
    let expr = v ! i
    toInt <- readSTRef toIntMap
    if not $ M.member expr toInt
      then do
        c <- readSTRef count
        modifySTRef' toIntMap (M.insert expr c)
        modifySTRef' toMarkovMap (M.insert c expr)
        modifySTRef' count (+1)
      else return ()
  toInt <- readSTRef toIntMap
  toMarkov <- readSTRef toMarkovMap
  let toIntF m = fromJust $ M.lookup m toInt
  let toMarkovF i = fromJust $ M.lookup i toMarkov
  return (toIntF, toMarkovF)

run :: (RandomGen g)
    => Int  -- ^ size of prediction context
    -> VU.Vector Int  -- ^ training sequence, the one to walk through randomly
    -> Int  -- ^ index to start the random walk within the training sequence
    -> g    -- ^ random generator state
    -> Int -- length of sequence to generate
    -> M.HashMap (Int, Int) (VU.Vector Int)
    -> VU.Vector Int -- random result, unboxed for performance
run n wordlist start g len succmap = runST $ do
  randgen <- newSTRef g
  result <- VM.new len
  -- start off with some initial words
  VU.indexM wordlist 0 >>= VM.write result 0 
  VU.indexM wordlist 1 >>= VM.write result 1
  forM_ [2..len-1] $ \i -> do
    w1 <- VM.read result (i-2)
    w2 <- VM.read result (i-1)
    gen <- readSTRef randgen
    (next,newgen) <- randomElem gen $ fromJust $ M.lookup (w1,w2) succmap
    VM.write result i next
    writeSTRef randgen newgen
  VU.unsafeFreeze result

randomElem :: Monad m => RandomGen g => g -> VU.Vector Int -> m (Int, g)
randomElem g v = do
  elt <- VU.indexM v index
  return (elt, newg)
    where (index, newg) = randomR (0, (VU.length v) - 1) g

-- Creates a map of [n words] -> [possible successor words] (for now n=2 always)
createMap2 :: VU.Vector Int -> M.HashMap (Int, Int) (VU.Vector Int)
createMap2 wordlist = runST $ do
  succmap <- newSTRef (M.empty :: M.HashMap (Int, Int) (VU.Vector Int))
  forM_ [0..(VU.length wordlist)-1-2] $ \i -> do
    w1 <- VU.indexM wordlist i
    w2 <- VU.indexM wordlist (i+1)
    successor <- VU.indexM wordlist (i+2)
    modifySTRef' succmap (\m -> case M.lookup (w1,w2) m of
                                  Nothing -> M.insert (w1,w2) (VU.singleton successor) m
                                  Just succs -> M.insert (w1,w2) (VU.cons successor succs) m)
  seqmap <- readSTRef succmap
  return seqmap

{-# LANGUAGE OverloadedStrings #-}
module Data.Stargate.Markov where
import qualified Data.Text as T
import qualified Data.Stargate as D
import System.Random
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (RandomGen, randomR)
import Control.Monad.Trans.State (State, state, evalState)
import qualified Data.Vector as V

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
generateTrans rand wordlist = markovToText $ take 5000 $ run 2 wordlist 0 rand


{- |
Creates a chain of elements
respecting to the probabilities of possible successors.
The list is considered being cyclic in order
to have successors for the last elements.

Example:

> take 100 $ run 2 "The sad cat sat on the mat. " 0 (Random.mkStdGen 123)

-}
run :: (Ord a, RandomGen g) =>
      Int  -- ^ size of prediction context
   -> [a]  -- ^ training sequence, the one to walk through randomly
   -> Int  -- ^ index to start the random walk within the training sequence
   -> g    -- ^ random generator state
   -> [a]
run n dict start g = 
   let keyError = error "key is not contained in dictionary"
       fm = createMap n dict
       {- This is the main function of this program.
          It is quite involved.
          If you want to understand it,
          imagine that the list 'y' completely exists
          before the computation. -}
       y = take n (drop start dict) ++
           -- run them on the initial random generator state
           (flip evalState g $
            -- this turns the list of possible successors
            -- into an action that generate a list
            -- of randomly chosen items
            mapM
               (randomItem .
                -- lookup all possible successors of an infix
                flip (Map.findWithDefault keyError) fm .
                -- turn suffix into an infixes of length n
                take n) $
            iterate tail y)
   in  y


runMulti :: (Ord a, RandomGen g) =>
      Int    -- ^ size of prediction context
   -> [[a]]  -- ^ training sequences, the order is relevant
   -> Int    -- ^ index of starting training sequence
   -> g      -- ^ random generator state
   -> [[a]]
runMulti n dicts i g =
   let wrappedDicts = map ((Nothing :) . map Just) dicts
       k  = sum (map length (take i wrappedDicts))
       xs = run n (concat wrappedDicts) k g
       ([], ys) = segment (maybe (Left ()) Right) xs
   in  map snd ys

{-
runMulti :: (Ord a, RandomGen g) =>
      Int        -- ^ size of prediction context
   -> [[a]]      -- ^ training sequences, the order is relevant
   -> (Int,Int)  -- ^ index to start the random walk within a training sequence
   -> g          -- ^ random generator state
   -> [[a]]
runMulti n dicts (i,j) g =
   let wrappedDicts = map ((Nothing :) . map Just) dicts
       k  = sum (map length (take i wrappedDicts)) + j
       xs = run n (concat wrappedDicts) k g
       (y, ys) = segment (maybe (Left ()) Right) xs
   in  y : map snd ys
-}

segment :: (a -> Either b c) -> [a] -> ([c], [(b,[c])])
segment p =
   foldr (\ x ~(y,ys) ->
      either
         (\b -> ([], (b,y):ys))
         (\c -> (c:y, ys))
         (p x)) ([], [])

{- |
Choose a random item from a list.
-}
randomItem :: (RandomGen g) => [a] -> State g a
randomItem x = fmap (x!!) (randomRState (0, length x - 1))

{- |
'System.Random.randomR' wrapped in a State monad.
-}
randomRState :: (RandomGen g) => (Int,Int) -> State g Int
randomRState bnds = state (randomR bnds)

{- |
Create a map that lists for each string all possible successors.
-}
createMap :: (Ord a) => Int -> [a] -> Map [a] [a]
createMap n x =
   let xc = cycle x
       -- list of the map keys
       sufxs   = map (take n) (iterate tail xc)
       -- list of the map images, i.e. single element lists
       imgxs   = map (:[]) (drop n xc)
       mapList = takeMatch x (zip sufxs imgxs)
   in  Map.fromListWith (++) mapList

{- |
Lazy variant of 'take'.
-}
takeMatch :: [b] -> [a] -> [a]
takeMatch = zipWith (flip const)

{-# LANGUAGE OverloadedStrings #-}
module Main where
import Transcript.IO
import qualified Transcript as D
import qualified Data.Text.IO as T
import Transcript.Format ()
import System.IO
import System.Environment
import TextShow
import Transcript.PostgreSQL
import Transcript.Format (showh)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->  putStrLn "Need 1 argument, 'postgresql', 'elasticsearch', 'single_trek' or 'single_gate'"
    (x:_) -> case x of
        "postgresql" -> do
          eps <- readAllTranscripts
          T.putStr $ toPostgreSQL eps
        "single_gate" -> do
           raw <- T.hGetContents stdin
           let ep = parseStargate raw
           T.putStr $ showh ep
        "single_trek" -> do
           raw <- T.hGetContents stdin
           let ep = parseStarTrek raw
           T.putStr $ showh ep
        s -> putStrLn $ "Bad argument: " ++ s

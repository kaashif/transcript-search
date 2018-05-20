{-# LANGUAGE OverloadedStrings #-}
module Main where
import Transcript.IO
import qualified Transcript as D
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Transcript.Format ()
import System.IO
import System.Environment
import TextShow
import Transcript.JSON
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->  putStrLn "Need 1 argument, 'elasticsearch', 'single_trek' or 'single_gate'"
    (x:_) -> case x of
        "elasticsearch" -> do
          eps <- readAllTranscripts
          BS.putStr $ toJSON eps
        "single_gate" -> do
           raw <- T.hGetContents stdin
           let ep = parseStargate raw
           T.putStrLn $ D.title ep
           T.putStr $ showt ep
        "single_trek" -> do
           raw <- T.hGetContents stdin
           let ep = parseStarTrek raw
           T.putStrLn $ D.title ep
           T.putStr $ showt ep
        s -> putStrLn $ "Bad argument: " ++ s

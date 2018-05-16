{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Stargate.IO
import qualified Data.Stargate as D
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Stargate.Format ()
import System.IO
import System.Environment
import TextShow
import Data.Stargate.JSON
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] ->  putStrLn "Need 1 argument, 'elasticsearch' or 'single'"
    (x:_) -> case x of
        "elasticsearch" -> do
          eps <- readAllTranscripts
          BS.putStr $ toJSON eps
        "single" -> do
           raw <- T.hGetContents stdin
           let ep = parseRaw raw
           T.putStrLn $ D.title ep
           T.putStr $ showt ep
        s -> putStrLn $ "Bad argument: " ++ s

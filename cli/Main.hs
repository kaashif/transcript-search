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
  if length args == 0
    then putStrLn "Need 1 argument, 'elasticsearch' or 'single'"
    else do
      case (head args) of
        "elasticsearch" -> do
          eps <- readAllTranscripts
          BS.putStr $ toJSON eps
        "single" -> do
           raw <- T.hGetContents stdin
           let ep = parseRaw raw
           T.putStrLn $ D.title ep
           T.putStr $ showt ep

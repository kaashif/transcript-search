{-# LANGUAGE OverloadedStrings #-}
module Data.Stargate.IO where

import qualified Data.Vector as V
import System.FilePath
import qualified Data.Stargate as D
import Data.Stargate.Parse (scriptp, convert)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Attoparsec.Text (parseOnly)
import Control.Monad
import System.FilePath.Glob
import qualified Data.ByteString as BS
import qualified Data.Set as S

readTranscript :: FilePath -> IO D.Episode
readTranscript fname = do
  rawscript <- fmap T.decodeLatin1 $ BS.readFile fname
  case parseOnly scriptp rawscript of
    Right exprs -> return $ convert exprs
    _ -> return $ D.Episode (V.singleton noScene) T.empty
    where noScene = D.Scene D.Exterior "nowhere" S.empty V.empty V.empty

readAllTranscripts :: IO (V.Vector (T.Text, T.Text, D.Episode))
readAllTranscripts = fmap V.concat $ forM ["sg1", "atl"] $ \series -> do
  fnames <- globDir1 (compile $ joinPath ["transcripts", series, "*"]) "."
  let readT f = readTranscript f >>= \t -> return (T.pack series, T.pack $ last $ splitPath f, t)
  V.mapM readT (V.fromList fnames)

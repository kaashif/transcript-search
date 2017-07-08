module Data.Stargate.JSON where

import GHC.Generics
import Data.Aeson
import Data.Stargate
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import System.FilePath

instance ToJSON Scene
instance ToJSON Episode
instance ToJSON IntExt

dump :: V.Vector (T.Text, T.Text, Episode) -> FilePath -> IO ()
dump v fp = BS.writeFile fp (encode v)



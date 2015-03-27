--------------------------------------------------------------------------------
module Ipseity.Init
  ( incept
  ) where
--------------------------------------------------------------------------------

import           Control.Applicative
import           Data.Either
import           Data.Maybe
import qualified Data.HashMap.Strict as M
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as D (readFile, putStrLn)
import           Text.Toml           (parseTomlDoc)
import           Text.Toml.Types
import           System.IO           (putStrLn)
import           System.Exit

import           Ipseity.Types
import           Ipseity.Init.Precept

--------------------------------------------------------------------------------

incept :: FilePath -> IO (Either Err Precept)
incept c = do
  p <- createPrecept c
  case p of
    Left err -> return $ Left err
    Right precept -> return $ Right precept


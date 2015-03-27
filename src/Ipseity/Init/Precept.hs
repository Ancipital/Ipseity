--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Ipseity.Init.Precept
  ( Precept
  ) where
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

createPrecept :: FilePath -> Either Err Precept
createPrecept c
  -- The config is mandatory.

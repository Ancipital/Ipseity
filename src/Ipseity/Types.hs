--------------------------------------------------------------------------------
module Ipseity.Types
  ( Err
  , Precept
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

--------------------------------------------------------------------------------

-- | Contains the nick, user, realname strings.
data Precept = Precept String String String

data Inception = Inception

-- | Contains an error integer and a message
type Err = (Int, String)

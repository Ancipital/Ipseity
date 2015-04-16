--------------------------------------------------------------------------------
module Ipseity.Types
  ( Err
  , Precept
  , Incept(..)
  , preceptKeys
  , inceptKeys
  ) where
--------------------------------------------------------------------------------

import qualified Data.Text           as T
import           Data.Text           (Text)
import qualified Data.HashMap.Lazy   as HM (HashMap)

preceptKeys = fmap T.pack ["nickname", "username", "realname", "server"]
inceptKeys  = fmap T.pack ["hostname", "port", "ssl", "channels"]

type Precept = HM.HashMap Text String

data Incept = Incept
  { ircPrecept :: Precept
  , ircServer  :: String
  , ircPort    :: Int
  , ircSSL     :: Bool
  , ircChans   :: String
  } deriving (Eq, Show)

data Inception = Inception

-- | Contains an error integer and a message
type Err = (Int, String)

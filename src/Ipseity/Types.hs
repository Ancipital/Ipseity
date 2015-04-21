--------------------------------------------------------------------------------
module Ipseity.Types
  ( Err
  , PreceptBase
  , Precept(..)
  , preceptKeys
  ) where
--------------------------------------------------------------------------------

import qualified Data.Text           as T
import           Data.Text           (Text)
import qualified Data.HashMap.Lazy   as HM (HashMap)

preceptKeys = fmap T.pack ["nickname", "username", "realname", "server"]

type PreceptBase = HM.HashMap Text String

data Precept = Precept
  { ircNick    :: String
  , ircUser    :: String
  , ircName    :: String
  , ircSrvName :: String
  , ircSrvHost :: String
  , ircSrvPort :: Int
  , ircSrvSSL  :: Bool
  , ircSrvChn  :: String
  } deriving (Eq, Show)

-- | Contains an error integer and a message
type Err = (Int, String)

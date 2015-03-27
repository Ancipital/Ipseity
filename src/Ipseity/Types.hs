--------------------------------------------------------------------------------
module Ipseity.Types
  ( Err
  , Precept(..)
  ) where

--------------------------------------------------------------------------------

-- | Contains the nick, user, realname strings.
data Precept = Precept String String String
  deriving (Show)

data Inception = Inception

-- | Contains an error integer and a message
type Err = (Int, String)

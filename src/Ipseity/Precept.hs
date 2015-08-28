--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Ipseity.Precept
  ( precept
  , Err
  , Precept(..)
  , ServerConfig(..)
  ) where
--------------------------------------------------------------------------------

import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import           Control.Applicative  ((<*>), (<$>))
import           Control.Monad        (mzero)
import           Data.Aeson
import           Data.Maybe

--------------------------------------------------------------------------------

data Precept = Precept
  { server  :: ServerConfig
  } deriving (Eq, Show)

data ServerConfig = ServerConfig
  { ircNick    :: String
  , ircUser    :: String
  , ircName    :: String
  , ircSrvName :: String
  , ircSrvHost :: String
  , ircSrvPort :: Int
  , ircSrvSSL  :: Bool
  , ircSrvChn  :: [String]
  } deriving (Eq, Show)

-- | Contains an error integer and a message
type Err = (Int, String)

instance FromJSON ServerConfig where
  parseJSON (Object o) = 
    ServerConfig <$> o .: "nickname"
                 <*> o .: "username"
                 <*> o .: "realname"
                 <*> ((o .: "server") >>= (.: "name"))
                 <*> ((o .: "server") >>= (.: "hostname"))
                 <*> ((o .: "server") >>= (.: "port"))
                 <*> ((o .: "server") >>= (.: "ssl"))
                 <*> ((o .: "server") >>= (.: "channels"))

  parseJSON _          = mzero

instance ToJSON ServerConfig where
  toJSON c = 
    object [ "nickname"   .= ircNick c
           , "username"   .= ircUser c
           , "realname"   .= ircName c
           , "server"     .= server
           ]
    where 
      server = 
        object [ "name"       .= ircSrvName c
               , "hostname"   .= ircSrvHost c
               , "port"       .= ircSrvPort c 
               , "ssl"        .= ircSrvSSL c
               , "channels"   .= ircSrvChn c
               ]

getJSON :: FilePath -> IO B.ByteString
getJSON f = B.readFile f

precept :: FilePath -> IO (Either Err Precept)
precept f = do
  p <- (eitherDecode <$> (getJSON f)) :: IO (Either String ServerConfig)
  case p of
    Left err      -> return $ Left (1, "Failed to load servers configuration: " ++ err)
    Right config -> return $ Right (Precept config)

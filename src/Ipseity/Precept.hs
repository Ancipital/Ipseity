--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Ipseity.Precept
  ( precept
  , connectServer
  , Err
  , Precept(..)
  , ServerConfig(..)
  ) where
--------------------------------------------------------------------------------

import qualified Data.ByteString.Lazy as B
import           Control.Applicative ((<*>), (<$>))
import           Data.Aeson
import           Data.Maybe
import qualified Data.Text           as T

--------------------------------------------------------------------------------

data Precept = Precept
  { servers  :: [ServerConfig]
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
  parseJSON (Object v) = 
    ServerConfig <$> v .: "nickname"
                 <*> v .: "username"
                 <*> v .: "realname"
                 <*> v .: "serverName"
                 <*> v .: "serverHost"
                 <*> v .: "port"
                 <*> v .: "ssl"
                 <*> v .: "channels"

instance ToJSON ServerConfig where
  toJSON c = 
    object [ "nickname"   .= ircNick c
           , "username"   .= ircUser c
           , "realname"   .= ircName c
           , "serverName" .= ircSrvName c
           , "serverHost" .= ircSrvHost c
           , "port"       .= ircSrvPort c 
           , "ssl"        .= ircSrvSSL c
           , "channels"   .= ircSrvChn c
           ]


getJSON :: FilePath -> IO B.ByteString
getJSON f = B.readFile f

precept :: FilePath -> IO (Either Err Precept)
precept f = do
  p <- (eitherDecode <$> (getJSON f)) :: IO (Either String [ServerConfig])
  case p of
    Left err      -> return $ Left (1, "Failed to load servers configuration: " ++ err)
    Right configs -> return $ Right (Precept configs)


-- | Right now this just displays a bunch of results
-- from a correctly parsed config file.
connectServer :: ServerConfig -> IO ()
connectServer c = do
  putStrLn $ "Connecting to server: " ++ servername
  putStrLn $ "Hostname: " ++ server ++ ":" ++ (show port)
  putStrLn $ "SSL: " ++ show ssl
  putStrLn $ "Nickname: " ++ nickname
  putStrLn $ "Username: " ++ username
  putStrLn $ "Realname: " ++ realname
    where
  servername = ircSrvName c
  server     = ircSrvHost c
  port       = ircSrvPort c
  ssl        = ircSrvSSL c
  nickname   = ircNick c
  username   = ircUser c
  realname   = ircName c

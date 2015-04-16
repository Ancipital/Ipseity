--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Ipseity.Init
  ( incept
  , connectServer
  ) where
--------------------------------------------------------------------------------

import           Data.Maybe
import qualified Data.HashMap.Lazy   as HM (HashMap, lookup)
import qualified Data.Text           as T
import           Text.Toml.Types
import           Ipseity.Init.Precept
import           Ipseity.Init.Incept
import           Ipseity.Types

--------------------------------------------------------------------------------

incept :: FilePath -> IO (Either Err Incept)
incept c = do
  p <- createPrecept c
  case p of
    Left  err             -> return $ Left err
    Right (conf, precept) -> do
      let servername = fromJust $ HM.lookup (T.pack "server") precept
      let server = HM.lookup (T.pack servername) conf
      case server of
        Nothing -> return $ Left (1, "Server '" ++ servername ++ "' not found in " ++ c ++ ".")
        Just (NTable server') -> return $ chkServer precept server'

connectServer :: Incept -> IO ()
connectServer incept = do
  putStrLn $ "Connecting to server: " ++ servername
  putStrLn $ "Hostname: " ++ server ++ ":" ++ (show port)
  putStrLn $ "SSL: " ++ show ssl
  putStrLn $ "Nickname: " ++ nickname
  putStrLn $ "Username: " ++ username
  putStrLn $ "Realname: " ++ realname
    where
  precept = ircPrecept incept
  server = ircServer incept
  port = ircPort incept
  ssl = ircSSL incept
  chans = ircChans incept
  servername = fromJust $ HM.lookup "server" precept
  nickname = fromJust $ HM.lookup "nickname" precept
  realname = fromJust $ HM.lookup "realname" precept
  username = fromJust $ HM.lookup "username" precept

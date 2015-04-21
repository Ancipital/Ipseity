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
import           Ipseity.Types

--------------------------------------------------------------------------------

incept :: FilePath -> IO (Either Err Precept)
incept c = do
  p <- createPreceptBase c
  case p of
    Left  err             -> return $ Left err
    Right (conf, preceptBase) -> do
      let servername = fromJust $ HM.lookup (T.pack "server") preceptBase
      let server = HM.lookup (T.pack servername) conf
      case server of
        Nothing -> return $ Left (1, "Server '" ++ servername ++ "' not found in " ++ c ++ ".")
        Just (NTable server') -> return $ createPrecept preceptBase server'


-- | Right now this just displays a bunch of results
-- from a correctly parsed config file.
connectServer :: Precept -> IO ()
connectServer precept = do
  putStrLn $ "Connecting to server: " ++ servername
  putStrLn $ "Hostname: " ++ server ++ ":" ++ (show port)
  putStrLn $ "SSL: " ++ show ssl
  putStrLn $ "Nickname: " ++ nickname
  putStrLn $ "Username: " ++ username
  putStrLn $ "Realname: " ++ realname
    where
  servername = ircSrvName precept
  server     = ircSrvHost precept
  port       = ircSrvPort precept
  ssl        = ircSrvSSL precept
  nickname   = ircNick precept
  username   = ircUser precept
  realname   = ircName precept

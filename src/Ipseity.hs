--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Ipseity
  ( ipseity
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
import           Ipseity.Init

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- | Visual treats
--
putLn :: IO ()
putLn = putStrLn ""

nDash :: Char -> Int -> String
nDash c i = take i $ repeat c

putDshLn :: IO ()
putDshLn = putStrLn (nDash '-' 30)

putHdrLn :: IO ()
putHdrLn = putStrLn (nDash '=' 30)

--

-- ipseity :: FilePath -> IO ()
ipseity c = do
  putLn
  putHdrLn
  putStrLn "Loading Ipseity..."
  putDshLn


  i <- incept c

  case i of
    Left err -> do
      let errno  = ExitFailure (fst err)
      let errstr = snd err
      putStrLn errstr
      -- putHdrLn
      -- exitWith errno

    Right i' -> do
      putStrLn $ show i'
      -- exitWith ExitSuccess


    -- Right conf -> do
    --   -- | Config was parsed, now check if it
    --   -- actually contains what we need...
    --   case withIpseity conf of
    --     Left err -> do
    --       -- | The config file is incomplete.
    --       putStrLn $ snd err
    --       putDshLn
    --       exitWith $ fst err


    --     Right p -> do
    --       -- | p contains the minimum values
    --       -- minimum options, parsed as a tree.
    --       -- We'll just print it.
    --       putStrLn "Config was parsed."
    --       putStrLn $ show i


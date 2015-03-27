--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Ipseity
  ( ipseity
  ) where
--------------------------------------------------------------------------------

import           System.IO      (putStrLn)
import           System.Exit
--------------------------------------------------------------------------------

import           Ipseity.Init
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
      putStrLn "Error:"
      let errno  = ExitFailure (fst err)
      let errstr = snd err
      putStrLn errstr
      -- putHdrLn
      -- exitWith errno

    Right i' -> do
      putStrLn "Ok:"
      putStrLn $ show i'

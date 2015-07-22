--------------------------------------------------------------------------------
module Ipseity.Ipseity
  ( ipseity
  ) where
--------------------------------------------------------------------------------

import           Data.Text      (Text)
import qualified Data.Text      as T
import qualified Data.Text.IO   as D (putStrLn)
import           System.IO      hiding (putStrLn)
import           System.Exit
--------------------------------------------------------------------------------

import           Ipseity.Precept
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

ipseity :: FilePath -> IO ()
ipseity c = do
  putLn
  putHdrLn
  putStrLn "Loading Ipseity..."
  putDshLn


  i <- precept c

  case i of
    Left err -> do
      putStrLn "Error:"
      let errno  = ExitFailure (fst err)
      let errstr = snd err
      putStrLn errstr
      putHdrLn
      exitWith errno

    Right i' -> connectServer (head $ servers i')
      -- putStrLn "Ok:"
      -- putStrLn $ show i'

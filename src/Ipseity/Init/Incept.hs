--------------------------------------------------------------------------------
module Ipseity.Init.Incept
  ( chkServer
  ) where
--------------------------------------------------------------------------------

import           Control.Applicative ((<*>), (<$>))
import           Data.Either
import           Data.Either.Utils   (fromLeft, fromRight, fromEither)
import           Data.Maybe
import qualified Data.HashMap.Lazy   as HM (lookup, fromList)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as D (readFile, putStrLn)
import           Text.Toml.Types
import           System.Directory    (doesFileExist)
import           Ipseity.Types

--------------------------------------------------------------------------------

chkServer :: Precept -> Table -> Either Err Incept
chkServer precept table = do
  let lookup k = HM.lookup (T.pack k) table

  let hnm = case lookup "hostname" of
              Just (NTValue (VString a))  -> Right (T.unpack a)
              otherwise                   -> Left ("hostname: should be of type String.")

  let prt = case lookup "port" of
              Just (NTValue (VInteger a)) -> Right (fromIntegral a)
              otherwise                   -> Left ("port: should be of type Integer.")

  let ssl = case lookup "ssl" of
              Just (NTValue (VBoolean a)) -> Right a
              otherwise                   -> Left ("ssl: should be a boolean.")

  let chn = case lookup "channels" of
              Just (NTValue (VString a))  -> Right (T.unpack a)
              otherwise                   -> Left ("channels: should be of form '#chan1,#chan2' as a string.")

  if isLeft hnm
     then Left (1, fromLeft hnm)
     else if isLeft prt
     then Left (1, fromLeft prt)
     else if isLeft ssl
     then Left (1, fromLeft ssl)
     else if isLeft chn
     then Left (1, fromLeft chn)
     else Right $ Incept precept (fromRight hnm) (fromRight prt) (fromRight ssl) (fromRight chn)


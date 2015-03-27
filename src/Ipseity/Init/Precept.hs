--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Ipseity.Init.Precept
  ( createPrecept
  ) where
--------------------------------------------------------------------------------

import           Control.Applicative ((<*>), (<$>))
import           Data.Either         (rights)
import qualified Data.HashMap.Lazy   as M (lookup)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as D (readFile, putStrLn)
import           Text.Toml           (parseTomlDoc)
import           Text.Toml.Types
import           System.Directory    (doesFileExist)
import           Ipseity.Types

--------------------------------------------------------------------------------

createPrecept :: FilePath -> IO (Either Err Precept)
createPrecept c = do
  p <- parseConf c
  case p of
    Left err -> return $ Left err

    Right conf -> do
      -- | Config was parsed, now check if it
      -- actually contains what we need...
      case getvals conf of
        Left err -> return $ Left (1, show err)

        Right pre -> do
          -- | p contains the minimum values
          -- minimum options, parsed as a tree.
          -- let p' = createPrecept p
          return $ Right $ mkPrecept pre


mkPrecept :: [(String, String)] -> Precept
mkPrecept p = do
  let snds = snd <$> p
  Precept (snds !! 0) (snds !! 1) (snds !! 2)


-- | Toml-parse a config and fully return it
parseConf :: FilePath -> IO (Either Err Table)
parseConf file = do
  ce <- doesFileExist file
  if not ce
     then do
       let err = 2
       let errmsg = file ++ ": file not found.\n"
                         ++ "Did you forget to copy the config file?"
       return $ Left (err, errmsg)
     else do
       c <- parseConfWith file
       return c

parseConfWith :: FilePath -> IO (Either Err Table)
parseConfWith file = do
  conf <- D.readFile file
  let toml = parseTomlDoc "" conf
  case toml of
    Left a -> return $ Left (22, "Error reading " ++ file ++ " " ++ (show a))
    Right a -> return $ Right a


getvals c = do
  let keys = ["nickname", "username", "realname"]

  -- | This returns, for example
  -- [Just (NTValue (VString "nickname"))
  -- which has type Either a [Maybe Node]
  let kv = (\k -> (k, M.lookup k c)) <$> keys

  -- | Kind a cast for kv to get a proper type
  -- It returns [(key, Either (Maybe String) String)
  let kv' = getvals' <$> kv

  -- | Find how many mandatory settings are misconfigured or missing
  let wrongVals = filter (not . validVal) kv'

  if length wrongVals < 1
     then do
       let keys = fst . unzip $ kv'
       let goodVals = rights . snd . unzip $ kv'
       Right $ zip keys goodVals

     else Left $ Just $ unwrapValue <$> wrongVals


getvals' :: (Text, Maybe Node)
         -> (String , Either (Maybe String) String)
getvals' (a, b) = do
  let a' = T.unpack a
  let b' = case b of
             Just (NTValue (VString s)) -> Right $ T.unpack s
             Just s                     -> Left $ Just "value has invalid type"
             Nothing                    -> Left $ Just "absent in config file"
  (a', b')


validVal :: (String, Either (Maybe String) String) -> Bool
validVal a =
  case a of
    (_, Right _) -> True
    (_, Left _) -> False


-- | Function that returns a string containing a reason, or nothing.
unwrapValue :: (String, Either (Maybe String) b) -> String
unwrapValue (a, b) =
  case (a, b) of
    (a, Left Nothing) -> a
    (a, Left (Just b)) -> a ++ ": " ++ b

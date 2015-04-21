--------------------------------------------------------------------------------
module Ipseity.Init.Precept
  ( createPreceptBase
  , createPrecept
  , chkConf
  ) where
--------------------------------------------------------------------------------

import           Control.Applicative ((<*>), (<$>))
import           Data.Either         (rights, isLeft)
import           Data.Either.Utils   (fromLeft, fromRight, fromEither)
import qualified Data.HashMap.Lazy   as HM (lookup, fromList)
import           Data.Maybe
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as D (readFile, putStrLn)
import           Text.Toml           (parseTomlDoc)
import           Text.Toml.Types
import           System.Directory    (doesFileExist)
import           Ipseity.Types

--------------------------------------------------------------------------------

createPreceptBase :: FilePath -> IO (Either Err (Table, PreceptBase))
createPreceptBase c = do
  p <- parseConf c
  case p of
    Left err -> return $ Left err

    Right conf -> do
      -- | Config was parsed, now check if it
      -- actually contains what we need...
      case chkConf conf preceptKeys of
        Left err -> return $ Left (1, show err)

        Right preceptBase -> do
          -- | p contains the minimum values
          -- minimum options, parsed as a tree.
          -- let p' = createPrecept p
          return $ Right $ (conf, HM.fromList preceptBase)


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
    Left a -> return $ Left (22, "Error reading " ++ file ++ " " ++ show a)
    Right a -> return $ Right a


chkConf toml rks = do
  -- | Lookup rks in toml
  -- result type: [(Text, Maybe Node)]
  -- values:
  -- [ ("nickname", Just (NTValue (VString "mynick")))
  -- , ("username", Just (NTValue (VString "myuser")))
  -- , ("realname", Just (NTValue (VString "myname")))
  -- ]
  let kvs = (\k -> (k, HM.lookup k toml)) <$> rks

  -- | Check if kvs was found and contains the right types
  -- result type: [(String, Either (Maybe String) String)]
  -- values:
  -- [ ("nickname", Right "mynick")
  -- , ("username", Right "myuser")
  -- , ("realname", Right "myname")
  -- ]
  -- Or, if something else than a string was found:
  -- [ ("nickname", Right "mynick")
  -- , ("username", Left  "value has invalid type")
  -- , ("realname", Left  "absent from config file")
  -- ]
  let kvs' = validateConf <$> kvs

  -- | create a list containing all Lefts from kvs'
  let lvs' = filter valIsLeft kvs'

  if (not . null) lvs'
     then Left $ Just $ unwrapValue <$> lvs'

     else do
       let keys = fst . unzip $ kvs'
       let rvs  = T.unpack <$> (rights . snd . unzip $ kvs')
       Right $ zip keys rvs


-- validateConf :: (Text, Maybe Node)
--              -> (Text , Either (Maybe String) Text)
validateConf (a, b) = do
  let b' = case b of
             Just (NTValue (VString s))  -> Right $ s
             Just (NTValue (VInteger s)) -> Right $ T.pack $ show s
             Just (NTValue (VBoolean s)) -> Right $ T.pack "true"
             Just (NTValue (VArray s)) ->   Right $ T.pack $ show s
             Just s                      -> Left $ Just "value has invalid type"
             Nothing                     -> Left $ Just "absent from config file"
  (a, b')


valIsLeft :: (Text, Either (Maybe String) Text) -> Bool
valIsLeft a =
  case a of
    (_, Left  _) -> True
    (_, Right _) -> False


-- | Function that returns a string containing a reason, or nothing.
unwrapValue :: (Text, Either (Maybe String) b) -> String
unwrapValue (a, b) =
  case (a, b) of
    (a, Left Nothing)  -> T.unpack a
    (a, Left (Just b)) -> T.unpack a ++ ": " ++ b

createPrecept :: PreceptBase -> Table -> Either Err Precept
createPrecept preceptBase table = do
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
     else do
       let servname = fromJust $ HM.lookup "server"   preceptBase
       let nickname = fromJust $ HM.lookup "nickname" preceptBase
       let realname = fromJust $ HM.lookup "realname" preceptBase
       let username = fromJust $ HM.lookup "username" preceptBase

       Right $ Precept nickname username realname servname (fromRight hnm) (fromRight prt) (fromRight ssl) (fromRight chn)


import qualified Data.ByteString.Lazy as B
import           Ipseity.Ipseity
import           Data.Aeson
import           Control.Applicative ((<$>), (<*>))
import           Ipseity.Types

jsonFile :: FilePath
jsonFile = "test.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> getJSON) :: IO (Either String [ServerConfig])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right ps -> print ps

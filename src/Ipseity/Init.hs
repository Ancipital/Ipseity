--------------------------------------------------------------------------------
module Ipseity.Init
  ( incept
  ) where
--------------------------------------------------------------------------------

import           Ipseity.Init.Precept
import           Ipseity.Types

--------------------------------------------------------------------------------

incept :: FilePath -> IO (Either Err Precept)
incept c = do
  p <- createPrecept c
  case p of
    Left err -> return $ Left err
    Right precept -> return $ Right precept


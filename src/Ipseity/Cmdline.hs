-- vim: ts=2 sw=2 sts=2 et conceallevel=0
--------------------------------------------------------------------------------

import           System.Exit         (ExitCode(..), exitWith)
import           Options.Applicative

--------------------------------------------------------------------------------

-- | Options for the command line
-- cfType: -t on the cli, the type of config (toml, json, others one day)
-- cfFile: cli argument, the actual configuration file
data CmdOpts = CmdOpts
  { cfType :: String
  , cfFile :: FilePath
  }
  deriving (Show)


-- | Command line parser
cmdOpts :: Parser CmdOpts
cmdOpts = CmdOpts
      <$> strOption
          ( short 't'
         <> long "type"
         <> metavar "CONFIGTYPE"
         <> help "Configuration file type (toml | json)" )
      <*> argument str
          ( metavar "FILE"
         <> help "Configuration filename" )


-- | Currently just displays a greeting
greet (CmdOpts "toml" b) = putStrLn $ "Hello. a is toml, and b is " ++ b ++ "."
greet (CmdOpts "json" b) = putStrLn $ "Hello. a is json, and b is " ++ b ++ "."
greet (CmdOpts _      b) = do
  putStrLn $ "Error: Filetype needs to be either \"toml\" or \"json\"."
  exitWith $ ExitFailure 1


-- | This probably needs to go elsewhere
-- or maybe main needs to be renamed.
main = execParser opts >>= greet
  where
    opts = info (helper <*> cmdOpts)
         $ ( progDesc "Not quite an IRC bot... YET"
          <> header "Some fancy header"
          <> footer "Some fancy footer" )

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UmuChangelog where

import           Import
-- opt-parse
import           Options.Applicative
-- umu-changelog
import           UmuChangelog.Capability.Git
import           UmuChangelog.Command

newtype AppM a
  = AppM
  { unAppM :: IO a
  } deriving ( Functor, Applicative, Monad, MonadIO )

runAppM :: AppM a -> IO a
runAppM app = unAppM app

startApp :: IO ()
startApp = do
  comm <- showHelpOnErrorExecParser
    ( info ( helper <*> parseVersion <*> parseCommand )
      ( fullDesc <> progDesc umuProgDesc <> header umuHeader ))
  runAppM $ run comm
  where
    run :: Command -> AppM ()
    run comm = case comm of
      CommandUpdate  -> undefined
      CommandCurrent -> undefined

instance ManageGit ( AppM ) where
  getLatestTag = getLatestTagImpl

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

umuProgDesc :: String
umuProgDesc = "Use umu-changelog to manage the CHANGELOG.md"

umuHeader :: String
umuHeader = "umu-changelog: Manage CHANGELOG.md file"

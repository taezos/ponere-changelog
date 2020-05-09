{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PonereChangelog where

import           Import
-- lens
import           Lens.Micro
-- opt-parse
import           Options.Applicative
-- ponere-changelog
import           PonereChangelog.Capability.Changelog
import           PonereChangelog.Capability.Git
import           PonereChangelog.Capability.Log
import           PonereChangelog.Command
import           PonereChangelog.Log
-- exceptions
import           Control.Monad.Catch

newtype AppM a
  = AppM
  { unAppM :: IO a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow )

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
      CommandUpdate  -> do
        eRef <- getLatestRef
        case eRef of
          Left err  -> logError $ show err
          Right ref -> either
            ( logError . show )
            ( appendTagAndHint ref )
            =<< getCommitMsgsWithRef ref
      CommandRead -> readLog

instance ManageGit AppM where
  getLatestRef = getLatestTagImpl
  getCommitMsgWithRef = getCommitMsgWithRefImpl
  getCommitMsgsWithRef = getCommitMsgsWithRefImpl

instance ManageChangelog AppM where
  appendTag = appendTagImpl
  readLog = readLogImpl
  appendHint = appendHintImpl

instance LogMessage AppM where
  logMessage l = case l ^. logReason of
    Info  -> logMessageImpl l Info
    Debug -> logMessageImpl l Debug
    Error -> logMessageImpl l Error
    Warn  -> logMessageImpl l Warn

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

umuProgDesc :: String
umuProgDesc = "Use umu-changelog to manage the CHANGELOG.md"

umuHeader :: String
umuHeader = "umu-changelog: Manage CHANGELOG.md file"

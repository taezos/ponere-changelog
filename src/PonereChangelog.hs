{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PonereChangelog where

import           Import
-- mtl
import           Control.Monad.Except
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
import           PonereChangelog.Response

newtype AppM m a
  = AppM
  { unAppM :: ( ExceptT PonereError m ) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadError PonereError )

runAppM :: MonadIO m => Command -> ExceptT PonereError m PonereResponse
runAppM comm = unAppM $ runCommand comm

startApp :: IO ()
startApp = do
  comm <- showHelpOnErrorExecParser
    ( info ( helper <*> parseVersion <*> parseCommand )
      ( fullDesc <> progDesc ponereProgDesc <> header ponereHeader ))
  res <- runExceptT $ runAppM comm
  case res of
    Left err  -> logError $ ponereErrorToText err
    Right msg -> logInfo $ ponereResponseToText msg

runCommand :: MonadIO m => Command -> AppM m PonereResponse
runCommand comm = case comm of
  CommandUpdate  -> do
    eRefBeforeLatest <- getRefNameBeforeLatest
    eLatestRef <- getLatestRef
    let eRefs = combineRefs eRefBeforeLatest eLatestRef
    case eRefs of
      Left err -> throwError err
      Right ( refBeforeLatest, latestRef ) -> do
        commits <- getCommitMsgsWithRef ( fromRefNameBeforeLatest refBeforeLatest )
        either
          throwError
          ( appendTagAndHint ( fromLatestRefName latestRef ) )
          commits
  CommandRead -> readLog

instance MonadIO m => ManageGit ( AppM m ) where
  getLatestRef = getLatestRefNameImpl
  getRefNameBeforeLatest = getRefNameBeforeLatestImpl
  getCommitMsgWithRef = getCommitMsgWithRefImpl
  getCommitMsgsWithRef = getCommitMsgsWithRefImpl

instance MonadIO m => ManageChangelog ( AppM m ) where
  appendTag = appendTagImpl
  readLog = readLogImpl
  appendHint = appendHintImpl

instance MonadIO m => LogMessage ( AppM m ) where
  logMessage l = case l ^. logReason of
    Info  -> logMessageImpl l Info
    Debug -> logMessageImpl l Debug
    Error -> logMessageImpl l Error
    Warn  -> logMessageImpl l Warn

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

ponereProgDesc :: String
ponereProgDesc = "Use ponere-changelog to manage the CHANGELOG.md"

ponereHeader :: String
ponereHeader = "ponere-changelog: Manage CHANGELOG.md file"

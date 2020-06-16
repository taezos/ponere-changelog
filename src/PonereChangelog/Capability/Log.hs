module PonereChangelog.Capability.Log
  ( LogMessage (..)
  , log
  , logInfo
  , logWarn
  , logDebug
  , logError
  , logMessageImpl
  ) where

import           Import
import           Lens.Micro
import           PonereChangelog.Log

class Monad m => LogMessage m where
  logMessage :: Log -> m ()

instance LogMessage IO where
  logMessage l = case l ^. logReason of
    Info  -> logMessageImpl l Info
    Debug -> logMessageImpl l Debug
    Error -> logMessageImpl l Error
    Warn  -> logMessageImpl l Warn

log :: ( MonadIO m, LogMessage m ) => LogReason -> Text -> m ()
log reason = logMessage <=< mkLog reason

logInfo :: ( MonadIO m, LogMessage m  ) => Text -> m ()
logInfo = log Info

logWarn :: ( MonadIO m, LogMessage m ) => Text -> m ()
logWarn = log Warn

logDebug :: ( MonadIO m, LogMessage m ) => Text -> m ()
logDebug = log Debug

logError :: ( MonadIO m, LogMessage m ) => Text -> m ()
logError = log Error

logMessageImpl :: MonadIO m => Log -> LogReason -> m ()
logMessageImpl l logR = mkTerminalLog
  ( l ^. logMsg . logMessageText )
  logR
  ( l ^. logMsg . logMessageHeader )

module UmuChangelog.Log
  ( LogReason (..)
  , Log
  , mkLog
  , logReason
  , logMsg
  , logMessageText
  , logMessageHeader
  , mkTerminalLog
  ) where

import qualified Data.Text.IO        as T
import           Import
import           Lens.Micro
import           System.Console.ANSI as ANSI


data LogReason
  = Debug
  | Info
  | Warn
  | Error
  deriving (Eq, Show)

data Log = Log
  { _logReason :: LogReason
  , _logMsg    :: LogMessage
  }

data LogMessage = LogMessage
  { _logMessageText   :: Text
  , _logMessageHeader :: Text
  } deriving ( Eq, Show )

mkLog :: MonadIO m => LogReason -> Text -> m Log
mkLog reason msg = do
  pure $ Log
    { _logReason = reason
    , _logMsg = LogMessage
      { _logMessageText = msg
      , _logMessageHeader = mkHeader reason
      }
    }
  where
     mkHeader :: LogReason -> Text
     mkHeader res = case res of
       Debug -> "[DEBUG]: "
       Info  -> "[INFO]: "
       Warn  -> "[WARN]: "
       Error -> "[ERROR]: "

mkTerminalLog :: MonadIO m => Text -> LogReason -> Text -> m ()
mkTerminalLog msg reason logHeader = liftIO $ do
  ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull ( reasonToColor reason ) ]
  T.putStr logHeader
  ANSI.setSGR []
  T.putStrLn msg
  where
    reasonToColor :: LogReason -> Color
    reasonToColor lr = case lr of
      Debug -> ANSI.Blue
      Info  -> ANSI.Green
      Warn  -> ANSI.Yellow
      Error -> ANSI.Red

-------------------------
-- Lens
-------------------------

logReason :: Lens' Log LogReason
logReason fn log@Log{ _logReason = lg } =
  fn lg <&> \newLogReason -> log { _logReason = newLogReason }

logMsg :: Lens' Log LogMessage
logMsg fn log@Log{ _logMsg = lm } =
  fn lm <&> \newLogMsg -> log { _logMsg = newLogMsg }

logMessageText :: Lens' LogMessage Text
logMessageText fn logMessage@LogMessage{ _logMessageText = lm } =
  fn lm <&> \newLogMessageText -> logMessage { _logMessageText = newLogMessageText }

logMessageHeader :: Lens' LogMessage Text
logMessageHeader fn logMessage@LogMessage{ _logMessageHeader = lmh } =
  fn lmh <&> \newLogHeader -> logMessage { _logMessageHeader = newLogHeader }

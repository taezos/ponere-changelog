module PonereChangelog.Capability.Changelog
  ( ManageChangelog (..)
  , appendTagImpl
  , readLogImpl
  , appendHintImpl
  ) where

import           Import
-- text
import qualified Data.Text.IO    as T
-- time
import           Data.Time.Clock

class Monad m => ManageChangelog m where
  appendTag :: Text -> m ()
  appendHint :: Text -> m ()
  readLog :: m ()

appendHintImpl :: ( MonadIO m, ManageChangelog m ) => Text -> m ()
appendHintImpl hint = do
  liftIO $ T.appendFile "./CHANGELOG.md" ( formatHint hint )
  where
    formatHint :: Text -> Text
    formatHint h = unlines
      [ ""
      , "<--! Hint, DELETE THIS BEFORE COMMITING:"
      , "  " <> h
      , "-->"
      ]


appendTagImpl :: ( MonadIO m, ManageChangelog m ) => Text -> m ()
appendTagImpl tag = do
  tagTxt <- formatTag tag
  liftIO $ T.appendFile "./CHANGELOG.md" tagTxt
  where
    formatTag :: MonadIO m => Text -> m Text
    formatTag t = do
      currTime <- liftIO getCurrentTime
      pure $ "\n## " <> t <> " -- " <> ( show $ utctDay currTime )

readLogImpl :: ( MonadIO m, ManageChangelog m ) => m ()
readLogImpl =
  liftIO $ T.putStrLn =<< T.readFile "./CHANGELOG.md"

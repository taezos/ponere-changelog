module UmuChangelog.Capability.Changelog
  ( ManageChangelog (..)
  , appendTagImpl
  ) where

import           Import
-- text
import qualified Data.Text.IO    as T
-- time
import           Data.Time.Clock

class Monad m => ManageChangelog m where
  appendTag :: Text -> m ()

appendTagImpl :: ( MonadIO m, ManageChangelog m ) => Text -> m ()
appendTagImpl tag = do
  tagTxt <- formatTag tag
  liftIO $ T.appendFile "./CHANGELOG.md" tagTxt

formatTag :: MonadIO m => Text -> m Text
formatTag t = do
  currTime <- liftIO getCurrentTime
  pure $ "\n## " <> t <> " -- " <> ( show $ utctDay currTime )

module UmuChangelog.Capability.Changelog where

import qualified Data.Text.IO as T
import           Import

class Monad m => ManageChangelog m where
  appendTag :: Text -> m ()

appendTagImpl :: MonadIO m => FilePath -> Text -> m ()
appendTagImpl filepath tag = liftIO $ T.appendFile filepath tag

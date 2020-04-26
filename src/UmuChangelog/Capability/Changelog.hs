module UmuChangelog.Capability.Changelog where

import           Import

class Monad m => ManageChangelog m where
  appendTag :: Text -> m ()

module UmuChangelog.Capability.Command where

import           Import

class Monad m => ManageCommand m where
  update :: m ()

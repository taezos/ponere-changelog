module UmuChangelog.Command where

import           Import

data Command
  = CommandUpgrade
  | CommandCurrent
  deriving ( Show )

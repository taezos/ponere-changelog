module PonereChangelog.Response where

import           Import

data PonereError
  = FileNotExist
  | NoGitTagsCreated
  | NoGitRepoDetected
  | NoGitCommitsFound
  deriving ( Eq, Show )

instance Exception PonereError

ponereErrorToText :: PonereError -> Text
ponereErrorToText = show

newtype PonereResponse = ChangelogSuccess Text
  deriving ( Eq, Show )

ponereResponseToText :: PonereResponse -> Text
ponereResponseToText ( ChangelogSuccess msg ) = msg

module UmuChangelog.Capability.Git
  ( ManageGit(..)
  , getLatestTagImpl
  ) where

import           Import
-- git
import           Data.Git
import           Data.Git.Ref
import           Data.Git.Storage
-- text
import qualified Data.Text        as T

class Monad m => ManageGit m where
  getLatestTag :: m ( Either GitError Text )

localRepo :: MonadIO m => ( Git SHA1 -> IO ( Either GitError Text ) ) -> m ( Either GitError Text )
localRepo filePath = liftIO $ do
  mFilePath <- findRepoMaybe
  case mFilePath of
    Nothing -> pure $ Left NoRepoDetected
    Just _  -> withCurrentRepo filePath

data GitError
  = NoTagsCreated
  | NoRepoDetected
  deriving ( Eq, Show )

getLatestTagImpl :: ( MonadIO m, ManageGit m ) => m ( Either GitError Text )
getLatestTagImpl = localRepo $ \git -> do
  list <- liftIO $ tagList git
  if null list
    then pure $ Left NoTagsCreated
    else pure . Right . T.pack . refNameRaw . last . fromList $ toList list

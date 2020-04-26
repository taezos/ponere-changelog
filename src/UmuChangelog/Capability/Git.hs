module UmuChangelog.Capability.Git
  ( ManageGit(..)
  , getLatestTagImpl
  ) where

import           Data.Git
import           Data.Git.Ref
import           Data.Git.Storage
import qualified Data.Text        as T
import           Import

class Monad m => ManageGit m where
  getLatestTag :: m Text

localRepo :: MonadIO m => ( Git SHA1 -> IO Text ) -> m Text
localRepo filePath = liftIO $ do
  mFilePath <- findRepoMaybe
  case mFilePath of
    Nothing -> pure "No repo"
    Just _  -> withCurrentRepo filePath

getLatestTagImpl :: ( MonadIO m, ManageGit m ) => m Text
getLatestTagImpl = liftIO $ localRepo $ \git -> do
  list <- tagList git
  if null list
    then pure $ "No Tags created"
    else pure . T.pack . refNameRaw . last . fromList $ toList list

{-# LANGUAGE ScopedTypeVariables #-}
module PonereChangelog.Capability.Git
  ( ManageGit(..)
  , getLatestTagImpl
  , getLatestCommitMsgImpl
  ) where

import           Data.Either
import           Import
-- git
import           Data.Git
import qualified Data.Git.Monad   as GitMonad
import           Data.Git.Ref
import           Data.Git.Storage
-- text
import qualified Data.Text        as T

class Monad m => ManageGit m where
  getLatestTag :: m ( Either GitError Text )
  getLatestCommitMsg :: m ( Maybe ByteString )

localRepo :: MonadIO m => ( Git SHA1 -> IO ( Either GitError a ) ) -> m ( Either GitError a )
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

getLatestCommitMsgImpl :: ( MonadIO m, ManageGit m ) => m ( Maybe ByteString )
getLatestCommitMsgImpl = liftIO $ fmap ( join . hush ) <$> GitMonad.withCurrentRepo $
  GitMonad.withCommit ( "master" :: String ) $
    fmap commitMessage <$> GitMonad.getCommit ( "master" :: String )

hush :: Either e a -> Maybe a
hush = either ( const Nothing ) Just

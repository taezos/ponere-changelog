{-# LANGUAGE ScopedTypeVariables #-}
module PonereChangelog.Capability.Git
  ( ManageGit(..)
  , getLatestTagImpl
  , getCommitMsgWithRefImpl
  , getCommitMsgsWithRefImpl
  , getRefNameBeforeLatestImpl
  , hush
  , GitError(..)
  , combineRefs
  ) where

import           Data.Either
import           Data.Ord
import           Import
-- git
import           Data.Git
import qualified Data.Git.Monad         as GitMonad
import           Data.Git.Ref
import           Data.Git.Repository
import           Data.Git.Storage
import           Data.Git.Storage.Loose
import           Data.Git.Types
-- text
import qualified Data.Text              as T

class Monad m => ManageGit m where
  getLatestRef :: m ( Either GitError Text )
  getRefNameBeforeLatest :: m ( Either GitError Text )
  getCommitMsgWithRef :: Text -> m ( Maybe ByteString )
  getCommitMsgsWithRef :: Text -> m ( Either GitError [ ByteString ] )

localRepo
  :: MonadIO m
  => ( Git SHA1 -> IO ( Either GitError a ) )
  -> m ( Either GitError a )
localRepo filePath = liftIO $ do
  mFilePath <- findRepoMaybe
  case mFilePath of
    Nothing -> pure $ Left NoRepoDetected
    Just _  -> withCurrentRepo filePath

data GitError
  = NoTagsCreated
  | NoRepoDetected
  | NoCommitsFound
  deriving ( Eq, Show )

instance Exception GitError

getAllRefs
  :: MonadIO m
  => Git SHA1
  -> m [ Ref SHA1 ]
getAllRefs git = do
  prefixes <- liftIO $ looseEnumeratePrefixes (gitRepoPath git)
  liftIO
    $ fmap ( catMaybes . concat )
    $ traverse prefixesToRefs prefixes
  where
    prefixesToRefs :: MonadIO m => String -> m [ Maybe ( Ref SHA1 ) ]
    prefixesToRefs prefix = do
      refs <- liftIO $ looseEnumerateWithPrefix ( gitRepoPath git ) prefix
      traverse ( pure . Just ) refs

-- | retreives all commits with the latest commit first.
retreiveAllCommits :: MonadIO m => m ( Either GitError [ Commit SHA1 ] )
retreiveAllCommits = localRepo $ \git -> do
  refs <- getAllRefs git
  commits <- catMaybes <$> traverse ( getCommitMaybe git ) refs
  pure $ handler commits
  where
    handler :: [ Commit SHA1 ] -> ( Either GitError [ Commit SHA1 ] )
    handler com
      | null com = Left NoCommitsFound
      | otherwise = Right $ fromLatestCommits com

    -- sorts commits from latest commit
    fromLatestCommits :: [ Commit SHA1 ] -> [ Commit SHA1 ]
    fromLatestCommits commits = sortBy
      (\c c' -> timeOrdering ( getTimeFromCommit c ) ( getTimeFromCommit c' ) )
      commits

    -- time ordering with latest first.
    timeOrdering time1 time2
      | time1 > time2 = LT
      | time1 < time2 = GT
      | otherwise = EQ

    getTimeFromCommit = gitTimeUTC . personTime . commitAuthor

-- | get the latest ref name from the tag
getLatestTagImpl :: ( MonadIO m, ManageGit m ) => m ( Either GitError Text )
getLatestTagImpl = localRepo $ \git -> do
  list <- liftIO $ tagList git
  pure $ handler list
  where
    handler :: Set RefName -> Either GitError Text
    handler l
      | null l = Left NoTagsCreated
      | otherwise =  Right . T.pack . refNameRaw . last . fromList $ toList l

-- | gets the ref name before the latest tag.
getRefNameBeforeLatestImpl :: ( MonadIO m ) => m ( Either GitError Text )
getRefNameBeforeLatestImpl = localRepo $ \git -> do
  list <- liftIO $ tagList git
  pure $ handler list
  where
    handler :: Set RefName -> Either GitError Text
    handler l
      | null l = Left NoTagsCreated
      -- this drops the items except for the last 2 items in the list.
      -- Then it takes the head of the list by using listToMaybe
      | otherwise = Right
        . maybe mempty ( T.pack . refNameRaw )
        . listToMaybe
        . drop ( ( length $ toList l ) - 2 )
        $ toList l

getCommitsWithRefImpl :: ( MonadIO m, ManageGit m ) => Text -> m ( Either GitError [ Commit SHA1 ] )
getCommitsWithRefImpl ref = do
  commit <- fmap ( listToMaybe . commitParents ) <$> getLatestCommit ref
  commits <- retreiveAllCommits
  pure
    $ takeWhileInclusive
    (\c -> ( maybe mempty toHexString $ listToMaybe $ commitParents $ c ) /= ( maybe mempty toHexString $ join $ commit ))
    <$>
    commits

getCommitMsgsWithRefImpl
  :: ( MonadIO m, ManageGit m )
  => Text
  -> m ( Either GitError [ ByteString ] )
getCommitMsgsWithRefImpl ref =
  ( fmap . fmap ) commitMessage <$> getCommitsWithRefImpl ref

getLatestCommit :: MonadIO m => Text -> m ( Maybe ( Commit SHA1 ) )
getLatestCommit refName =
  liftIO $ fmap ( join . hush ) <$> GitMonad.withCurrentRepo
  $ GitMonad.withCommit ( T.unpack refName )
  $ GitMonad.getCommit ( T.unpack refName )

getCommitMsgWithRefImpl
  :: ( MonadIO m, ManageGit m )
  => Text
  ->  m ( Maybe ByteString )
getCommitMsgWithRefImpl refName =
  fmap commitMessage <$> getLatestCommit refName

-------------------------------
-- utils
-------------------------------
hush :: Either e a -> Maybe a
hush = either ( const Nothing ) Just

-- the predicate is for continuation purposes.
takeWhileInclusive :: ( a -> Bool ) -> [ a ] -> [ a ]
takeWhileInclusive _ [] = []
takeWhileInclusive predicate (x:xs) =
  x : if predicate x then takeWhileInclusive predicate xs else []

combineRefs :: Either GitError Text -> Either GitError Text -> Either GitError ( Text, Text )
combineRefs refNameBeforeLatest latestRef = do
  refBeforeLatest <- refNameBeforeLatest
  lRef <- latestRef
  pure ( refBeforeLatest, lRef )

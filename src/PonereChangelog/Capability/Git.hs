{-# LANGUAGE ScopedTypeVariables #-}
module PonereChangelog.Capability.Git
  ( ManageGit(..)
  , LatestRefName
  , RefNameBeforeLatest
  , getLatestRefNameImpl
  , getCommitMsgWithRefImpl
  , getCommitMsgsWithRefImpl
  , getRefNameBeforeLatestImpl
  , hush
  , combineRefs
  , fromLatestRefName
  , fromRefNameBeforeLatest
  ) where


import           Control.Exception        (evaluate)
import           Data.Either
import           Data.Ord
import           Import
-- text
import qualified Data.Text                as T
-- git
import           Data.Git
import qualified Data.Git.Monad           as GitMonad
import           Data.Git.Ref
import           Data.Git.Repository
import           Data.Git.Storage
import           Data.Git.Storage.Loose
import           Data.Git.Types
-- ponere-changelog
import           PonereChangelog.Response

class Monad m => ManageGit m where
  getLatestRef :: m ( Either PonereError LatestRefName )
  getRefNameBeforeLatest :: m ( Either PonereError RefNameBeforeLatest )
  getCommitMsgWithRef :: Text -> m ( Maybe ByteString )
  getCommitMsgsWithRef :: Text -> m ( Either PonereError [ ByteString ] )

localRepo
  :: MonadIO m
  => ( Git SHA1 -> IO ( Either PonereError a ) )
  -> m ( Either PonereError a )
localRepo filePath = liftIO $ do
  mFilePath <- findRepoMaybe
  case mFilePath of
    Nothing -> pure $ Left NoGitRepoDetected
    Just _  -> withCurrentRepo filePath

-- data PonereError
--   = NoTagsCreated
--   | NoRepoDetected
--   | NoCommitsFound
--   deriving ( Eq, Show )

-- instance Exception PonereError

getAllRefs
  :: MonadIO m
  => Git SHA1
  -> m [ Ref SHA1 ]
getAllRefs git = do
  prefixes <- liftIO $ looseEnumeratePrefixes ( gitRepoPath git )
  liftIO $ concat <$> traverse prefixesToRefs prefixes
  where
    prefixesToRefs :: MonadIO m => String -> m [ Ref SHA1 ]
    prefixesToRefs prefix =
      liftIO $ looseEnumerateWithPrefix ( gitRepoPath git ) prefix

-- | retreives all commits with the latest commit first.
retreiveAllCommits :: MonadIO m => m ( Either PonereError [ Commit SHA1 ] )
retreiveAllCommits = localRepo $ \git -> do
  refs <- getAllRefs git
  commits <- traverse ( ( evaluate =<< ) . getCommitMaybe git ) refs
  pure $ handler ( catMaybes commits )
  where
    handler :: [ Commit SHA1 ] -> ( Either PonereError [ Commit SHA1 ] )
    handler com
      | null com = Left NoGitCommitsFound
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

newtype LatestRefName = LatestRefName Text
  deriving ( Eq, Show )

instance Semigroup LatestRefName where
  ( LatestRefName ref ) <> ( LatestRefName ref' ) = LatestRefName ( ref <> ref' )

instance Monoid LatestRefName where
  mempty = LatestRefName mempty

-- | get the latest ref name from the tag
getLatestRefNameImpl
  :: ( MonadIO m, ManageGit m )
  => m ( Either PonereError LatestRefName )
getLatestRefNameImpl = localRepo $ \git -> do
  list <- liftIO $ tagList git
  pure $ handler list
  where
    handler :: Set RefName -> Either PonereError LatestRefName
    handler l
      | null l = Left NoGitTagsCreated
      | otherwise =  Right . LatestRefName . T.pack . refNameRaw . last . fromList $ toList l

newtype RefNameBeforeLatest = RefNameBeforeLatest Text
  deriving ( Eq, Show )

instance Semigroup RefNameBeforeLatest where
  ( RefNameBeforeLatest ref ) <> ( RefNameBeforeLatest ref' ) =
    RefNameBeforeLatest ( ref <> ref' )

instance Monoid RefNameBeforeLatest where
  mempty = RefNameBeforeLatest mempty

-- | gets the ref name before the latest tag.
getRefNameBeforeLatestImpl
  :: MonadIO m
  => m ( Either PonereError RefNameBeforeLatest )
getRefNameBeforeLatestImpl = localRepo $ \git -> do
  list <- liftIO $ tagList git
  pure $ handler list
  where
    handler :: Set RefName -> Either PonereError RefNameBeforeLatest
    handler l
      | null l = Left NoGitTagsCreated
      -- this drops the items except for the last 2 items in the list.
      -- Then it takes the head of the list by using listToMaybe
      | otherwise = Right
        . maybe mempty ( RefNameBeforeLatest . T.pack . refNameRaw )
        . listToMaybe
        . drop ( ( length $ toList l ) - 2 )
        $ toList l

getCommitsWithRefImpl
  :: ( MonadIO m, ManageGit m )
  => Text
  -> m ( Either PonereError [ Commit SHA1 ] )
getCommitsWithRefImpl ref = do
  commit <- join <$> fmap ( listToMaybe . commitParents ) <$> getLatestCommit ref
  commits <- retreiveAllCommits
  pure $ takeWhileInclusive ( whenCommitsNotMatch commit ) <$> commits
  where
    whenCommitsNotMatch :: Maybe ( Ref SHA1 ) -> Commit SHA1 -> Bool
    whenCommitsNotMatch mCommit c =
      ( maybe mempty toHexString $ listToMaybe $ commitParents $ c )
      /= ( maybe mempty toHexString mCommit )

getCommitMsgsWithRefImpl
  :: ( MonadIO m, ManageGit m )
  => Text
  -> m ( Either PonereError [ ByteString ] )
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
-- utils & helpers
-------------------------------
hush :: Either e a -> Maybe a
hush = either ( const Nothing ) Just

-- the predicate is for continuation purposes.
takeWhileInclusive :: ( a -> Bool ) -> [ a ] -> [ a ]
takeWhileInclusive _ [] = []
takeWhileInclusive predicate (x:xs) =
  x : if predicate x then takeWhileInclusive predicate xs else []

combineRefs
  :: Either PonereError RefNameBeforeLatest
  -> Either PonereError LatestRefName
  -> Either PonereError ( RefNameBeforeLatest, LatestRefName )
combineRefs refNameBeforeLatest latestRef = do
  refBeforeLatest <- refNameBeforeLatest
  lRef <- latestRef
  pure ( refBeforeLatest, lRef )

fromLatestRefName :: LatestRefName -> Text
fromLatestRefName ( LatestRefName txt ) = txt

fromRefNameBeforeLatest :: RefNameBeforeLatest -> Text
fromRefNameBeforeLatest ( RefNameBeforeLatest txt ) = txt

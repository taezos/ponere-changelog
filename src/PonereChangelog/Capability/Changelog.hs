{-# LANGUAGE FlexibleContexts #-}
module PonereChangelog.Capability.Changelog
  ( ManageChangelog (..)
  , appendTagImpl
  , readLogImpl
  , appendHintImpl
  , appendTagAndHint
  ) where

import           Import                   hiding (FilePath)
-- mtl
import           Control.Monad.Except
-- text
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.IO             as T
-- time
import           Data.Time.Clock
-- turtle
import           Turtle                   (FilePath)
import qualified Turtle.Prelude           as TP
-- ponere-changelog
import           PonereChangelog.Response

class Monad m => ManageChangelog m where
  appendTag :: Text -> m PonereResponse
  appendHint :: Text -> m PonereResponse
  readLog :: m PonereResponse

appendHintImpl
  :: ( MonadIO m, ManageChangelog m )
  => Text
  -> m PonereResponse
appendHintImpl hint = do
  liftIO $ T.appendFile "./CHANGELOG.md" ( formatHint hint )
  pure $ ChangelogSuccess "Appended hint"
  where
    formatHint :: Text -> Text
    formatHint h = unlines
      [ ""
      , "<--! Hint, DELETE THIS BEFORE COMMITING:"
      , "  " <> h
      , "-->"
      ]

appendTagImpl :: ( MonadIO m, ManageChangelog m ) => Text -> m PonereResponse
appendTagImpl tag = do
  tagTxt <- formatTag tag
  liftIO $ T.appendFile "./CHANGELOG.md" tagTxt
  pure $ ChangelogSuccess "Appended tag to CHANGELOG.md"
  where
    formatTag :: MonadIO m => Text -> m Text
    formatTag t = do
      currTime <- liftIO getCurrentTime
      pure $ "\n## " <> t <> " -- " <> ( show $ utctDay currTime )

readLogImpl
  :: ( MonadIO m, ManageChangelog m, MonadError PonereError m )
  => m PonereResponse
readLogImpl = do
  isExists <- TP.testfile changelogPath
  case isExists of
    False -> throwError FileNotExist
    True -> do
      liftIO $ T.putStrLn =<< TP.readTextFile changelogPath
      pure $ ChangelogSuccess "File read"
  where
    changelogPath :: FilePath
    changelogPath = "./CHANGELOG.md"

-------------------------
-- helper
-------------------------
appendTagAndHint :: ManageChangelog m => Text -> [ ByteString ] -> m PonereResponse
appendTagAndHint ref commitMsgs = do
  void $ appendTag ref
  void $ appendHint ( unlines $ TE.decodeUtf8 <$> commitMsgs )
  pure $ ChangelogSuccess "Appended tag and hint"

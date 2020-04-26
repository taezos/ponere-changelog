module UmuChangelog.Command where

import           Data.Version        (showVersion)
import           Import
import           Options.Applicative
import           Paths_umu_changelog (version)

data Command
  = CommandUpdate
  | CommandCurrent
  deriving ( Show )

parseCommand :: Parser Command
parseCommand = subparser $
  ( command "update" $ parseCommandUpdate `withInfo` "Update CHANGELOG file")

parseCommandUpdate :: Parser Command
parseCommandUpdate = pure CommandUpdate

parseVersion :: Parser ( a -> a )
parseVersion =
  infoOption ( concat [ showVersion version ] )
  ( short 'v' <> long "version" <> help "Show version" <> hidden )

withInfo :: Parser Command -> String -> ParserInfo Command
withInfo opts desc = info ( helper <*> opts ) $ progDesc desc

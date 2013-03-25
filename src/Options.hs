module Options
    ( SortType(..)
    , Settings(..)
    , getOptions
    ) where

import Options.Applicative


data SortType = Monospaced | Proportional
    deriving (Show)

data Settings = Settings
    { inputFile :: String
    , outputFile :: String
    , debugPrint :: Bool
    , typeOfSort :: SortType
    }
    deriving (Show)

options :: Parser Settings
options =
    Settings
    <$> argument str
        ( metavar "Input" )
    <*> argument str
        ( metavar "Output" )
    <*> switch
        ( long "debug"
        <> help "Print out debugging logs" )
    <*> flag Proportional Monospaced
        ( long "monospaced"
        <> help "Enable to have monospaced sorting (TODO: not functional yet)" )

getOptions :: IO Settings
getOptions =
    execParser $ info (helper <*> options)
        ( fullDesc
        <> progDesc "Resort the firefox bookmarks using the length of the bookmark Title or Url"
        <> header "firefoxSort - Sort firefox bookmarks" )

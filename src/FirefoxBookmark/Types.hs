{-# LANGUAGE OverloadedStrings #-}
module FirefoxBookmark.Types
    ( Root(..)
    , PType(..)
    , Primary(..)
    , Annos(..)
    ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec.Number
import Prelude hiding (id)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T


data Root = BookmarkMenuFolder | PlacesRoot | TagsFolder | ToolbarFolder | UnfiledBookmarksFolder
    deriving (Show, Eq)

data PType = Place | PlaceContainer | PlaceSeparator
    deriving (Show, Eq)

data Primary = Primary
    { dateAdded :: Integer
    , id :: Integer
    , index :: Maybe Integer
    , lastModified :: Integer
    , parent :: Maybe Integer
    , title :: Maybe T.Text
    , keyword :: Maybe T.Text
    , ptype :: PType
    , uri :: Maybe T.Text
    , children :: Maybe [Primary]
    , root :: Maybe Root
    , charset :: Maybe T.Text
    , annos :: Maybe [Annos]
    , pangoLength :: Double
    } deriving (Show)

instance FromJSON Primary where
    parseJSON (Object v) =
        Primary
        <$> (v .: "dateAdded")
        <*> (v .: "id")
        <*> (v .:? "index")
        <*> (v .: "lastModified")
        <*> (v .:? "parent")
        <*> (v .: "title")
        <*> (v .:? "keyword")
        <*> parsePType v
        <*> (v .:? "uri")
        <*> (parseJSON =<< (v .:? "children" .!= Null))
        <*> parseRoot v
        <*> (v .:? "charset")
        <*> (parseJSON =<< (v .:? "annos" .!= Null))
        <*> (setZero)
    parseJSON _ = mzero

instance ToJSON Primary where
    toJSON p = object
        [ "dateAdded"    .= dateAdded p
        , "id"           .= id p
        , "index"        .= index p
        , "lastModified" .= lastModified p
        , "parent"       .= parent p
        , "title"        .= title p
        , "keyword"      .= keyword p
        , "type"         .= (dumpPType $ ptype p)
        , "uri"          .= uri p
        , "children"     .= children p
        , "root"         .= (dumpRoot $ root p)
        , "charset"      .= charset p
        , "annos"        .= annos p
        ]


--data AType = LoadInSidebar | FolderLastUsed | Description
--    deriving (Show, Eq)

data Annos = Annos
    { expires :: Integer
    , flags :: Integer
    , mimeType :: Maybe T.Text
    , name :: T.Text
    , atype :: Integer
--    , atype :: AType
    , value :: Either Integer T.Text
    } deriving (Show)

instance FromJSON Annos where
    parseJSON (Object v) =
        Annos
        <$> (v .: "expires")
        <*> (v .: "flags")
        <*> (v .: "mimeType")
        <*> (v .: "name")
        <*> (v .: "type")
        <*> parseValue v
    parseJSON _ = mzero

instance ToJSON Annos where
    toJSON p = object
        [ "expires"  .= expires p
        , "flags"    .= flags p
        , "mimeType" .= mimeType p
        , "name"     .= name p
        , "type"     .= atype p
        , "value"    .= (dumpValue $ value p)
        ]


setZero :: Parser Double
setZero = pure 0

parsePType :: Object -> Parser PType
parsePType x = case H.lookup "type" x of
    Nothing -> fail "key type not present"
    Just v  -> convertToPType v
    where
        convertToPType :: Value -> Parser PType
        convertToPType (String "text/x-moz-place-separator") = pure PlaceSeparator
        convertToPType (String "text/x-moz-place-container") = pure PlaceContainer
        convertToPType (String "text/x-moz-place")           = pure Place
        convertToPType _                                     = fail "undefined text/x-moz-* type"

parseRoot :: Object -> Parser (Maybe Root)
parseRoot x = case H.lookup "root" x of
    Nothing -> pure Nothing
    Just v  -> convertToRoot v
    where
        convertToRoot :: Value -> Parser (Maybe Root)
        convertToRoot (String "unfiledBookmarksFolder") = pure $ Just UnfiledBookmarksFolder
        convertToRoot (String "bookmarksMenuFolder")    = pure $ Just BookmarkMenuFolder
        convertToRoot (String "toolbarFolder")          = pure $ Just ToolbarFolder
        convertToRoot (String "placesRoot")             = pure $ Just PlacesRoot
        convertToRoot (String "tagsFolder")             = pure $ Just TagsFolder
        convertToRoot  _                                = fail "undefined root tag value"

parseValue :: Object -> Parser (Either Integer T.Text)
parseValue x = case H.lookup "value" x of
    Nothing -> fail "key value not present"
    Just v  -> failParse v
    where
        failParse :: Value -> Parser (Either Integer T.Text)
        failParse (String v) = pure $ Right v
        failParse (Number v) = pure $ Left $ truncate $ toRational v -- Should not have to do this cos its fucking integers in the json
        failParse _          = fail "Useless"

dumpValue :: (Either Integer T.Text) -> Value
dumpValue (Left x)  = Number $ I x
dumpValue (Right x) = String x

dumpPType :: PType -> Value
dumpPType PlaceSeparator = String "text/x-moz-place-separator"
dumpPType PlaceContainer = String "text/x-moz-place-container"
dumpPType Place          = String "text/x-moz-place"

dumpRoot :: Maybe Root -> Value
dumpRoot (Just UnfiledBookmarksFolder) = String "unfiledBookmarksFolder"
dumpRoot (Just BookmarkMenuFolder) = String "bookmarksMenuFolder"
dumpRoot (Just ToolbarFolder) = String "toolbarFolder"
dumpRoot (Just PlacesRoot) = String "placesRoot"
dumpRoot (Just TagsFolder) = String "tagsFolder"
dumpRoot Nothing = Null

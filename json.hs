{-# LANGUAGE OverloadedStrings #-}
-- {u'dateAdded': 1355015138514222,
-- u'id': 5876,
-- u'index': 11,
-- u'lastModified': 1355015138514222,
-- u'parent': 5864,
-- u'title': None,
-- u'keyword': u'some keywords',
-- u'type': u'text/x-moz-place',
-- maybe - u'children': [],
-- maybe - u'root': u'unfiledBookmarksFolder',
-- maybe - u'charset': u'UTF-8',
-- maybe - u'annos': [
--     {u'expires': 4,
--      u'flags': 0,
--      u'mimeType': None,
--      u'name': u'bookmarkProperties/description',
--      u'type': 3,
--      u'value': u'foobar'}
--      ],
-- u'uri': u'http://en.wikipedia.org/wiki/RAID'}],

import Prelude hiding (id)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero, liftM)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.List as L

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
    } deriving (Show)

data AType = LoadInSidebar | FolderLastUsed | Description
    deriving (Show, Eq)

data Annos = Annos
    { expires :: Integer
    , flags :: Integer
    , mimeType :: Maybe T.Text
    , name :: T.Text
    , atype :: Integer
--    , atype :: AType
    , value :: Either Integer T.Text
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
    parseJSON _ = mzero

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

main :: IO ()
main = do
    test <- BL.readFile "./bookmarks-2013-02-22.json"
    let y = eitherDecode' test :: Either String Primary

    -- Primary
--    putStrLn $ T.unpack $ processJSON (T.pack . show . dateAdded) y
--    putStrLn $ T.unpack $ processJSON (T.pack . show . id) y
--    putStrLn $ T.unpack $ processJSON (T.pack . show . fromMaybe 0 . index) y
--    putStrLn $ T.unpack $ processJSON (T.pack . show . lastModified) y
--    putStrLn $ T.unpack $ processJSON (T.pack . show . fromMaybe 0 . parent) y
--    putStrLn $ T.unpack $ processJSON (fromMaybe "" . title) y
--    putStrLn $ T.unpack $ processJSON (T.pack . show . ptype) y
--    putStrLn $ T.unpack $ processJSON (fromMaybe "" . uri) y
--    putStrLn $ T.unpack $ processJSON (T.pack . show . root) y
--    putStrLn $ T.unpack $ processJSON (fromMaybe "" . charset) y

    -- Annos
--    putStrLn $ T.unpack $ processJSON (iterList (T.pack . show . expires) . annos) y
--    putStrLn $ T.unpack $ processJSON (iterList (T.pack . show . flags) . annos) y
--    putStrLn $ T.unpack $ processJSON (iterList (fromMaybe "" . mimeType) . annos) y
--    putStrLn $ T.unpack $ processJSON (iterList name . annos) y
--    putStrLn $ T.unpack $ processJSON (iterList (T.pack . show . atype) . annos) y
--    putStrLn $ T.unpack $ processJSON (iterList (procValue . value) . annos) y

    putStrLn $ T.unpack $ processJSON' y

    where
        procValue :: Either Integer T.Text -> T.Text
        procValue (Left x)  = T.pack $ show x
        procValue (Right x) = x

        processJSON' :: Either String Primary -> T.Text
        processJSON' (Left x)  = T.pack x
        processJSON' (Right x) = showJSON $ findBookmarksMenuChildren x

        showJSON :: Maybe Primary -> T.Text
        showJSON Nothing  = ""
        showJSON (Just x) = processJSON (fromMaybe "" . title) $ Right x


getChildren :: Primary -> [Primary]
getChildren = concat . maybeToList . children

breadthFirst :: Primary -> [Primary]
breadthFirst x = breadthFirst' [] [x]
    where
        breadthFirst' :: [Primary] -> [Primary] -> [Primary]
        breadthFirst' x [] = x
        breadthFirst' x y  = breadthFirst' (x ++ y) (concat $ map getChildren y)

depthFirst :: Primary -> [Primary]
depthFirst x = depthFirst' [x]
    where
        depthFirst' :: [Primary] -> [Primary]
        depthFirst' []     = []
        depthFirst' (x:xs) = [x] ++ depthFirst' (getChildren x) ++ depthFirst' xs

-- PlacesRoot
--  - UnfiledBookmarksFolder
--  - TagsFolder
--  - ToolbarFolder
--  - BookmarksMenuFolder
--      - Proceed
findBookmarksMenuChildren :: Primary -> Maybe Primary
findBookmarksMenuChildren = L.find isBookmarksMenuFolder . breadthFirst
    where
        isBookmarksMenuFolder :: Primary -> Bool
        isBookmarksMenuFolder x = maybe False (== BookmarkMenuFolder) (root x)

-- Get a list of [Primary]
--  - Break it up via span into pieces using PlaceSeparator
--  - Filter it by PlacesContainer and sort and descend into their childrens
--  - Filter by Places and sort
--  - Repeat the breaking action via span till ran out of place separators
-- Concat all of the above result
-- Return it
--
-- data PType = Place | PlaceContainer | PlaceSeparator


-- TODO:
-- Sort
--  - Sort PlacesContainer by title length
--  - Sort Places by title length

-- TODO:
-- Fix up the `index` ordering of each entries
--  - Get a list of [primary] or something
--  - Descend into each PlaceContainers, and sort the PlaceContainer + Places + PlacesSeparators by list order
--  - Update the index of each one of these entries


-- TODO:
-- URL Staleness check
-- * Keep a list of already checked urls (borrow code from hakyll)
-- * Check each url one by one, then output the stale urls
--
-- Tags/Anon cleanup?
-- * Tags are probably good/useful, Maybe nice to have something that helps with tagging
-- * May want to? strip the bookmark anon description, its kind of useless for me


processJSON :: (Primary -> T.Text) -> Either String Primary -> T.Text
processJSON _ (Left x)  = T.pack x
processJSON f (Right x) = iterChild f $ Just [x]

iterChild :: (Primary -> T.Text) -> Maybe [Primary] -> T.Text
iterChild _ Nothing   = ""
iterChild f (Just xs) = foldl buildString "" xs
    where
        buildString :: T.Text -> Primary -> T.Text
        buildString a b = foldl T.append "" [a, f b, "\n", iterChild f $ children b]

-- Text list
iterList :: (Annos -> T.Text) -> Maybe [Annos] -> T.Text
iterList _ Nothing   = ""
iterList f (Just xs) = foldl buildString "" xs
    where
        buildString :: T.Text -> Annos -> T.Text
        buildString a b = foldl T.append "" [a, f b, "\n"]

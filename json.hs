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
--

import Prelude hiding (concat, id)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe
import Data.Text hiding (map, foldl, foldr, index)
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H

data Primary = Primary
    { dateAdded :: Integer
    , id :: Integer
    , index :: Maybe Integer
    , lastModified :: Integer
    , parent :: Maybe Integer
    , title :: Maybe Text
    , keyword :: Maybe Text
    , ptype :: Text
    , uri :: Maybe Text
    , children :: Maybe [Primary]
    , root :: Maybe Text
    , charset :: Maybe Text
    , annos :: Maybe [Annos]
    } deriving (Show)

data Annos = Annos
    { expires :: Integer
    , flags :: Integer
    , mimeType :: Maybe Text
    , name :: Text
    , atype :: Integer
    , value :: Either Integer Text
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
        <*> (v .: "type")
        <*> (v .:? "uri")
        <*> (parseJSON =<< (v .:? "children" .!= Null))
        <*> (v .:? "root")
        <*> (v .:? "charset")
        <*> (parseJSON =<< (v .:? "annos" .!= Null))
    parseJSON _ = mzero

instance FromJSON Annos where
    parseJSON (Object v) = do
        Annos
        <$> (v .: "expires")
        <*> (v .: "flags")
        <*> (v .: "mimeType")
        <*> (v .: "name")
        <*> (v .: "type")
        <*> parseValue v
    parseJSON _ = mzero

parseValue :: Object -> Parser (Either Integer Text)
parseValue x = case H.lookup "value" x of
    Nothing -> fail "key value not present"
    Just v  -> failParse v

failParse :: Value -> Parser (Either Integer Text)
failParse (String v) = pure $ Right v
failParse (Number v) = pure $ Left $ truncate $ toRational v -- Should not have to do this cos its fucking integers in the json
failParse _          = fail "Useless"

main :: IO ()
main = do
    test <- L.readFile "./bookmarks-2013-02-22.json"
    let y = eitherDecode' test :: Either String Primary

    -- Primary
--    putStrLn $ unpack $ processJSON (pack . show . dateAdded) y
--    putStrLn $ unpack $ processJSON (pack . show . id) y
--    putStrLn $ unpack $ processJSON (pack . show . fromMaybe 0 . index) y
--    putStrLn $ unpack $ processJSON (pack . show . lastModified) y
--    putStrLn $ unpack $ processJSON (pack . show . fromMaybe 0 . parent) y
--    putStrLn $ unpack $ processJSON (fromMaybe "" . title) y
    putStrLn $ unpack $ processJSON ptype y
--    putStrLn $ unpack $ processJSON (fromMaybe "" . uri) y
    putStrLn $ unpack $ processJSON (fromMaybe "" . root) y
--    putStrLn $ unpack $ processJSON (fromMaybe "" . charset) y

    -- Annos
--    putStrLn $ unpack $ processJSON (iterList (pack . show . expires) . annos) y
--    putStrLn $ unpack $ processJSON (iterList (pack . show . flags) . annos) y
--    putStrLn $ unpack $ processJSON (iterList (fromMaybe "" . mimeType) . annos) y
    putStrLn $ unpack $ processJSON (iterList name . annos) y
--    putStrLn $ unpack $ processJSON (iterList (pack . show . atype) . annos) y
--    putStrLn $ unpack $ processJSON (iterList (procValue . value) . annos) y

    where
        procValue :: Either Integer Text -> Text
        procValue (Left x)  = pack $ show x
        procValue (Right x) = x


-- TODO:
--
-- Sorting the urls by title (longest to shortest)
-- * Look into loading up each list of children, and sorting by longest to shortest title
-- * Segment it by breaking up the list by "segments" (-----)
-- * Then break it up by folder vs urls
-- * Finally sort the folder, url, then merge the list
-- * Fix up the `index` ordering of each entries
--
-- URL Staleness check
-- * Keep a list of already checked urls (borrow code from hakyll)
-- * Check each url one by one, then output the stale urls
--
-- Tags/Anon cleanup?
-- * Tags are probably good/useful, Maybe nice to have something that helps with tagging
-- * May want to? strip the bookmark anon description, its kind of useless for me



processJSON :: (Primary -> Text) -> Either String Primary -> Text
processJSON _ (Left x)  = pack x
processJSON f (Right x) = iterChild f $ Just [x]

iterChild :: (Primary -> Text) -> Maybe [Primary] -> Text
iterChild _ Nothing   = ""
iterChild f (Just xs) = foldl buildString "" xs
    where
        buildString :: Text -> Primary -> Text
        buildString a b = foldl append "" [a, f b, "\n", (iterChild f $ children b)]

-- Text list
iterList :: (Annos -> Text) -> Maybe [Annos] -> Text
iterList _ Nothing   = ""
iterList f (Just xs) = foldl buildString "" xs
    where
        buildString :: Text -> Annos -> Text
        buildString a b = foldl append "" [a, f b, "\n"]

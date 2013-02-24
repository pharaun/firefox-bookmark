{-# LANGUAGE OverloadedStrings #-}
-- {u'dateAdded': 1355015138514222,
-- u'id': 5876,
-- u'index': 11,
-- u'lastModified': 1355015138514222,
-- u'parent': 5864,
-- u'title': None,
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

import Prelude hiding (concat)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe
import Data.Text hiding (map, foldl, foldr)
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
    , ptype :: Text -- WTF
    , uri :: Maybe Text
    , children :: Maybe [Primary]
    , root :: Maybe Text
    , charset :: Maybe Text -- WTF
    , annos :: Maybe [Annos]
    } deriving (Show)

data Annos = Annos
    { expires :: Integer
    , flags :: Integer -- WTF
    , mimeType :: Maybe Text -- WTF
    , name :: Text
    , atype :: Integer -- WTF
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
    putStrLn $ unpack $ processJSON (\i -> fromMaybe "" $ charset i) y
    putStrLn $ unpack $ processJSON (\i -> iterList (\j -> name j) (annos i)) y

processJSON :: (Primary -> Text) -> Either String Primary -> Text
processJSON _ (Left x)  = pack x
processJSON f (Right x) = iterChild f $ Just [x]

iterChild :: (Primary -> Text) -> Maybe [Primary] -> Text
iterChild _ Nothing   = ""
iterChild f (Just xs) = foldl buildString "" xs
    where
        buildString :: Text -> Primary -> Text
        buildString a b = foldl append "" [a, f b, "\n", (iterChild f $ children b)]

iterList :: (Annos -> Text) -> Maybe [Annos] -> Text
iterList _ Nothing   = ""
iterList f (Just xs) = foldl buildString "" xs
    where
        buildString :: Text -> Annos -> Text
        buildString a b = foldl append "" [a, f b, "\n"]




-- data Primary = Primary
--     { dateAdded :: Integer
--     , id :: Integer
--     , index :: Maybe Integer
--     , lastModified :: Integer
--     , parent :: Maybe Integer
--     , title :: Maybe Text
--     , ptype :: Text -- WTF
--     , uri :: Maybe Text
--     , children :: Maybe [Primary]
--     , root :: Maybe Text
--     , charset :: Maybe Text -- WTF
--     , annos :: Maybe [Annos]
--     } deriving (Show)
--
-- data Annos = Annos
--     { expires :: Integer
--     , flags :: Integer -- WTF
--     , mimeType :: Maybe Text -- WTF
--     , name :: Text
--     , atype :: Integer -- WTF
--     , value :: Either Integer Text
--     } deriving (Show)

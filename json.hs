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
import Control.Monad (mzero, ap)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Attoparsec.Number
import Data.Either
import Data.Maybe
import Data.Monoid (mconcat)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T

import Control.Monad.Reader

-- Strange dependency, its for calculating proportional font width of a string
import Graphics.Rendering.Pango

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

instance ToJSON Annos where
    toJSON p = object
        [ "expires"  .= expires p
        , "flags"    .= flags p
        , "mimeType" .= mimeType p
        , "name"     .= name p
        , "type"     .= atype p
        , "value"    .= (dumpValue $ value p)
        ]

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

main :: IO ()
main = do
    -- Load the file
    test <- BL.readFile "./bookmarks-2013-02-22.json"
    let w = eitherDecode test :: Either String Primary

    -- Set the string length
    pango <- initPango
    x <- processLength pango w

    -- Process the json file
    let y = processJSON' x

    -- Print
    putStrLn $ T.unpack $ showJSON y

    -- Dump to file
    let z = encode y
    BL.writeFile "./test-export.json" z

    where
        processLength :: PangoType -> Either String Primary -> IO (Either String Primary)
        processLength _ (Left x)  = return $ Left x
        processLength p (Right x) = do
            a <- runReaderT (generateLength x) p
            return $ Right a

        processJSON' :: Either String Primary -> Maybe Primary
        processJSON' (Left x)  = Nothing
        processJSON' (Right x) = listToMaybe $ updateIndexing [resortBookmarkMenu x]

        showJSON :: Maybe Primary -> T.Text
        showJSON Nothing  = "Error"
        showJSON (Just x) = processJSON customTitle $ Right x

        customTitle :: Primary -> T.Text
        customTitle x@(Primary {ptype=PlaceSeparator}) = prettyPrint " - s" x
        customTitle x@(Primary {ptype=PlaceContainer}) = prettyPrint (" - c - " `T.append` len x `T.append` " - " `T.append` extract x) x
        customTitle x@(Primary {ptype=Place})          = prettyPrint (" - p - " `T.append` len x `T.append` " - " `T.append` extract x) x

        len :: Primary -> T.Text
        len x = T.pack $ show $ pangoLength x

        pullIndex :: Primary -> T.Text
        pullIndex = T.pack . show . fromMaybe 0 . index

        prettyPrint :: T.Text -> Primary -> T.Text
        prettyPrint t p = (T.justifyRight 3 ' ' $ pullIndex p) `T.append` t


type PangoType = (PangoContext, [PangoAttribute])

--
-- Init the pango context for generating pango length
--
initPango :: IO PangoType
initPango = do
    -- Setup context and dpi
    a <- cairoFontMapGetDefault
    cairoFontMapSetResolution a 100

    -- Get pango context
    p <- cairoCreateContext $ Just a

    -- Settings
    let pa = [AttrFamily 0 (-1) "Sans", AttrSize 0 (-1) 10]

    return (p, pa)

goodCharacterLength :: String -> ReaderT PangoType IO [Double]
goodCharacterLength x
    | L.null x  = return [0]
    | otherwise = do
        (p, pa) <- ask
        -- Whole string length/size - this works but it screws up with CJK characters
        pi <- liftIO $ pangoItemize p x pa
        gi <- liftIO $ pangoShape $ head pi

        -- Discrete glyph length
        liftIO $ glyphItemGetLogicalWidths gi Nothing


badCharacterLength :: String -> ReaderT PangoType IO [Double]
badCharacterLength x
    | L.null x  = return [0]
    | otherwise = forM x (\a -> do
        (p, pa) <- ask
        -- PangoItem
        pi <- liftIO $ pangoItemize p [a] pa
        gi <- liftIO $ pangoShape $ head pi

        -- Discrete glyph length
        w <- liftIO $ glyphItemGetLogicalWidths gi Nothing
        return $ head w)

-- (Good, Bad)
splitString :: String -> (String, String)
splitString = L.partition isIn
    where
        isIn :: Char -> Bool
        isIn x = isJust $ L.elemIndex x goodChar

        goodChar :: String
        goodChar = "`~<=>|°_-,;:!?/.·'’\"“”«»()[]{}§©®™@$*&#%+→—0123³456789aAáàåæbBcCdDeEéfFgGhHiIjJkKlLmMnNoOóôöōpPqQrRsStTuUüvVwWxXyYzZþ"

generateLength :: Primary -> ReaderT PangoType IO Primary
generateLength x = rebuildPrimary x <$> generateLengthChildrens x
    where
        generateLengthChildrens :: Primary -> ReaderT PangoType IO [Primary]
        generateLengthChildrens x = mapM generateLength =<< (mapM updateLength $ getChildren x)

        updateLength :: Primary -> ReaderT PangoType IO Primary
        updateLength x = updatePangoLength x <$> calcLength x

        calcLength :: Primary -> ReaderT PangoType IO Double
        calcLength x = do
            -- We call upon reader here to dump the pango stuff
            let (good, bad) = splitString $ T.unpack $ extract x
            goodLen <- goodCharacterLength good
            badLen <- badCharacterLength bad

            return $ L.foldl' (+) 0 (goodLen ++ badLen)

        -- Update the pango length
        updatePangoLength :: Primary -> Double -> Primary
        updatePangoLength x y = x { pangoLength = y }


rebuildPrimary :: Primary -> [Primary] -> Primary
rebuildPrimary x y = x { children = if L.null y then Nothing else Just y }

-- If 0 store as None, otherwise Just x
updateIndex :: Primary -> Integer -> Primary
updateIndex x 0 = x { index = Nothing }
updateIndex x y = x { index = Just y }

getChildren :: Primary -> [Primary]
getChildren = concat . maybeToList . children

-- Find and update BookMarksMenuChildren
-- PlacesRoot
--  - UnfiledBookmarksFolder
--  - TagsFolder
--  - ToolbarFolder
--  - BookmarksMenuFolder
--      - Proceed
resortBookmarkMenu :: Primary -> Primary
resortBookmarkMenu = rebuildPrimary `ap` sortMenuChildrens
    where
        sortMenuChildrens :: Primary -> [Primary]
        sortMenuChildrens x
            | isBookmarksMenu x = concatMap sortPlacesContainer $ sliceBookmarks $ getChildren x
            | otherwise         = map resortBookmarkMenu $ getChildren x

        isBookmarksMenu :: Primary -> Bool
        isBookmarksMenu x = maybe False (== BookmarkMenuFolder) (root x)

isPlaceX :: PType -> Primary -> Bool
isPlaceX x y = ptype y == x

-- Break up a list of Primary using PlaceSeparator
sliceBookmarks :: [Primary] -> [[Primary]]
sliceBookmarks = breakBySeparator [[]]
    where
        breakBySeparator :: [[Primary]] -> [Primary] -> [[Primary]]
        breakBySeparator x [] = x
        breakBySeparator x y = do
            let (d, n) = break (isPlaceX PlaceSeparator) y
            breakBySeparator (x ++ [d] ++ [take 1 n]) (drop 1 n)

-- Filter it by PlacesContainer and sort and descend into their childrens
sortPlacesContainer :: [Primary] -> [Primary]
sortPlacesContainer x
    | all (isPlaceX PlaceSeparator) x = x
    | otherwise                       = do
        let (c, p) = L.partition (isPlaceX PlaceContainer) x
        sortPrimary (map sortChildrens c) ++ sortPrimary p
    where
        sortChildrens :: Primary -> Primary
        sortChildrens = rebuildPrimary `ap` custom
            where
                custom :: Primary -> [Primary]
                custom = concatMap sortPlacesContainer . sliceBookmarks . getChildren

-- Sort by title length then uri length
sortPrimary :: [Primary] -> [Primary]
sortPrimary x
    | all (isPlaceX PlaceSeparator) x = x
    | otherwise                       = L.sortBy sorter x
    where
        sorter :: Primary -> Primary -> Ordering
--        sorter x y = mconcat [compare (T.length $ extract y) (T.length $ extract x), compare (extract x) (extract y)]
        sorter x y = mconcat [compare (pangoLength y) (pangoLength x), compare (extract x) (extract y)]

extract :: Primary -> T.Text
extract x
    | isJust (title x) = fromJust $ title x
    | isJust (uri x)   = fromJust $ uri x
    | otherwise        = ""

-- Fix up the `index` ordering of each entries
--  - Get a list of [primary] or something
--  - Descend into each PlaceContainers, and sort the PlaceContainer + Places + PlacesSeparators by list order
--  - Update the index of each one of these entries
updateIndexing :: [Primary] -> [Primary]
updateIndexing xs = snd $ L.mapAccumL weaveIndexUpdate 0 xs
    where
        weaveIndexUpdate :: Integer -> Primary -> (Integer, Primary)
        weaveIndexUpdate acc x = (acc + 1, updateIndex (rebuildPrimary x (updateIndexing $ getChildren x)) acc)


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
processJSON f (Right x) = iterChild True f $ Just [x]

--iterChild :: Bool -> (Primary -> T.Text) -> Maybe [Primary] -> T.Text
--iterChild :: Bool -> (Annos -> T.Text) -> Maybe [Annos] -> T.Text -- Only if Bool = False
iterChild _ _ Nothing   = ""
iterChild b f (Just xs) = foldl (buildString b) "" xs
    where
--        buildString :: Bool -> T.Text -> Primary -> T.Text
--        buildString :: Bool -> T.Text -> Annos -> T.Text
        buildString True  a b = foldl T.append "" [a, f b, "\n", iterChild True f $ children b]
        buildString False a b = foldl T.append "" [a, f b, "\n"]

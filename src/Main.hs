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

import Data.Aeson (encode, eitherDecode)
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import Control.Monad.Reader


-- Local imports
import FirefoxBookmark.Types
import FirefoxBookmark.PangoLength
import FirefoxBookmark.Sorter


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
        processJSON' (Left _)  = Nothing
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
        buildString True  a c = foldl T.append "" [a, f c, "\n", iterChild True f $ children c]
        buildString False a c = foldl T.append "" [a, f c, "\n"]

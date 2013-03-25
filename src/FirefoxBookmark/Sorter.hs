{-# LANGUAGE OverloadedStrings #-}
module FirefoxBookmark.Sorter
    ( rebuildPrimary
    , getChildren
    , extract
    , updateIndexing
    , resortBookmarkMenu
    ) where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Text as T
import Control.Monad (ap)
import Data.Monoid (mconcat)

-- Local imports
import FirefoxBookmark.Types


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
--        sorter a b = mconcat [compare (T.length $ extract b) (T.length $ extract a), compare (extract a) (extract b)]
        sorter a b = mconcat [compare (pangoLength b) (pangoLength a), compare (extract a) (extract b)]

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

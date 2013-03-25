{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Reader
import Data.Aeson (encode, eitherDecode)
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- Local imports
import FirefoxBookmark.PangoLength
import FirefoxBookmark.Sorter
import FirefoxBookmark.Types
import Options

-- TODO:
-- URL Staleness check
-- * Keep a list of already checked urls (borrow code from hakyll)
-- * Check each url one by one, then output the stale urls
--
-- Tags/Anon cleanup?
-- * Tags are probably good/useful, Maybe nice to have something that helps with tagging
-- * May want to? strip the bookmark anon description, its kind of useless for me
--
-- Making the customizable debugging logger work
--
-- Fixing the code for supporting monospaced sorting options
--
-- Massivly improving error handling code, right now things are a bit adhoc in the main

main :: IO ()
main = do
    -- Quick and dirty options
    opt <- getOptions

    -- TODO: add error handling code
    input <- BL.readFile $ inputFile opt
    let bookmarks = eitherDecode input :: Either String Primary

    -- Set the string length
    labeledBookmarks <- case typeOfSort opt of
        Monospaced   -> error "Monospaced option/support is not implemented yet"
        Proportional -> do
            pango <- initPango
            processLength pango bookmarks

    -- Process the json file
    let sortedBookmarks = processJSON' labeledBookmarks

    -- Print debugging output
    when (debugPrint opt) (putStrLn $ T.unpack $ showJSON sortedBookmarks)

    -- Dump to file
    BL.writeFile (outputFile opt) $ encode sortedBookmarks

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
        prettyPrint t p = T.justifyRight 3 ' ' $ pullIndex p `T.append` t

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

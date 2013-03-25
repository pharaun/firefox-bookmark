module FirefoxBookmark.PangoLength
--module Graphics.Rendering.PangoLength
    ( PangoType()
    , initPango
    , generateLength
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Maybe
import Graphics.Rendering.Pango
import qualified Data.List as L
import qualified Data.Text as T

-- Local imports
-- TODO: Clean up FirefoxBookmark dependency so that this can be exported into Graphics.Rendering.PangoLength
import FirefoxBookmark.Types
import FirefoxBookmark.Sorter


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
        pai <- liftIO $ pangoItemize p x pa
        gli <- liftIO $ pangoShape $ head pai

        -- Discrete glyph length
        liftIO $ glyphItemGetLogicalWidths gli Nothing


badCharacterLength :: String -> ReaderT PangoType IO [Double]
badCharacterLength x
    | L.null x  = return [0]
    | otherwise = forM x (\a -> do
        (p, pa) <- ask
        -- PangoItem
        pai <- liftIO $ pangoItemize p [a] pa
        gli <- liftIO $ pangoShape $ head pai

        -- Discrete glyph length
        w <- liftIO $ glyphItemGetLogicalWidths gli Nothing
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
        generateLengthChildrens a = mapM generateLength =<< (mapM updateLength $ getChildren a)

        updateLength :: Primary -> ReaderT PangoType IO Primary
        updateLength a = updatePangoLength a <$> calcLength a

        calcLength :: Primary -> ReaderT PangoType IO Double
        calcLength a = do
            -- We call upon reader here to dump the pango stuff
            let (good, bad) = splitString $ T.unpack $ extract a
            goodLen <- goodCharacterLength good
            badLen <- badCharacterLength bad

            return $ L.foldl' (+) 0 (goodLen ++ badLen)

        -- Update the pango length
        updatePangoLength :: Primary -> Double -> Primary
        updatePangoLength c d = c { pangoLength = d }

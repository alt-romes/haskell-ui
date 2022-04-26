{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Icons where

-- Icons from https://heroicons.com

import UI.Extended
import Data.Text

-- | The Text is equivalent to heroicons defined path
--
-- For example, OutlinedI "M4.318 6.318a4.5 4.5 0 000 6.364L12 20.364l7.682-7.682a4.5 4.5 0 00-6.364-6.364L12 7.636l-1.318-1.318a4.5 4.5 0 00-6.364 0z" 
data Icon = OutlinedI Text
          | FilledI Text

homeO :: Icon
homeO = OutlinedI "M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"

archiveO :: Icon
archiveO = OutlinedI "M5 8h14M5 8a2 2 0 110-4h14a2 2 0 110 4M5 8v10a2 2 0 002 2h10a2 2 0 002-2V8m-9 4h4"

cartO :: Icon
cartO = OutlinedI "M16 11V7a4 4 0 00-8 0v4M5 9h14l1 12H4L5 9z"

heartO :: Icon
heartO = OutlinedI "M4.318 6.318a4.5 4.5 0 000 6.364L12 20.364l7.682-7.682a4.5 4.5 0 00-6.364-6.364L12 7.636l-1.318-1.318a4.5 4.5 0 00-6.364 0z" 

heart :: Icon
heart = FilledI "M3.172 5.172a4 4 0 015.656 0L10 6.343l1.172-1.171a4 4 0 115.656 5.656L10 17.657l-6.828-6.829a4 4 0 010-5.656z" 

play :: Icon
play = FilledI "M10 18a8 8 0 100-16 8 8 0 000 16zM9.555 7.168A1 1 0 008 8v4a1 1 0 001.555.832l3-2a1 1 0 000-1.664l-3-2z" 

search :: Icon
search = FilledI "M8 4a4 4 0 100 8 4 4 0 000-8zM2 8a6 6 0 1110.89 3.476l4.817 4.817a1 1 0 01-1.414 1.414l-4.816-4.816A6 6 0 012 8z"

-- | Render an icon with default values
renderIcon :: Icon -> UI t ()
renderIcon (OutlinedI p) = renderIcon' 6 "" (OutlinedI p)
renderIcon (FilledI p)   = renderIcon' 5 "" (FilledI p)

-- | Render an Icon given a size and additional classes
renderIcon' :: Int -> Text -> Icon -> UI t ()
renderIcon' size c (OutlinedI p) = UI $
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" (constDyn $ "stroke-width"=:"2" <> "stroke"=:"currentColor" <> "viewBox"=:"0 0 24 24" <> "fill"=:"none" <> "class" =: ("h-" <> pack (show size) <> " w-" <> pack (show size) <> " " <> c)) $
        elDynAttrNS (Just "http://www.w3.org/2000/svg") "path" (constDyn $ "d" =: p <> "stroke-linejoin"=:"round" <> "stroke-linecap"=:"round") (return ())
renderIcon' size c (FilledI p) = UI $
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" (constDyn $ "viewBox"=:"0 0 20 20" <> "fill"=:"currentColor" <> "class" =: ("h-" <> pack (show size) <> " w-" <> pack (show size) <> " " <> c)) $
        elDynAttrNS (Just "http://www.w3.org/2000/svg") "path" (constDyn $ "d" =: p <> "fill-rule"=:"evenodd" <> "clip-rule"=:"evenodd") (return ())

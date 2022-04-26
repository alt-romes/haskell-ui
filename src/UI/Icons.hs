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

-- | Render an Icon given a size
--
-- Sizes:
-- h-5 w-5 h-6 w-6
renderIcon :: Int -> Icon -> UI t ()
renderIcon size (OutlinedI p) = UI $
    elDynAttrNS (Just "http://www.w3.org/2000/svg") "svg" (constDyn $ "stroke-width"=:"2" <> "stroke"=:"currentColor" <> "viewBox"=:"0 0 24 24" <> "fill"=:"none" <> "class" =: ("h-" <> pack (show size) <> " w-" <> pack (show size))) $
        elDynAttrNS (Just "http://www.w3.org/2000/svg") "path" (constDyn $ "d" =: p <> "stroke-linejoin"=:"round" <> "stroke-linecap"=:"round") (return ())
renderIcon _ (FilledI _) = undefined

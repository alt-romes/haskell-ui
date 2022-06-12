{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
UI elements for displaying text
-}
module UI.Text where

import UI.Class
import UI.Theme

-- | Add a label above the given UI
label :: Theme UI => UI a -> UI b -> UI b
label t e = el "div" $ do
    _ <- elClass "label" (textDarker <> " block text-sm font-medium mb-1") t
    e

-- | Heading text
heading :: Theme UI => UI () -> UI ()
heading = elClass "h3" (textColor <> " pt-2 pb-2 text-2xl font-semibold w-2/3")

-- | Heading with a margin
headingM :: Theme UI => Margin -> UI () -> UI ()
headingM m = elAttr "h3" ("class"=:(textColor <> " pt-2 pb-2 text-2xl font-semibold w-2/3") <> "style"=:style m)


-- | Simple paragraph
para :: Theme UI => UI a -> UI a
para = elClass "p" (textColor <> " text-md")

-- | Medium intensity color paragraph
-- TODO: Receive Intensity argument?
paraM :: Theme UI => UI a -> UI a
paraM = elClass "p" (textMedium <> " text-md")

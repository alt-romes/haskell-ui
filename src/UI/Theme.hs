{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Theme where

import Data.Kind
import Data.Text (Text, pack)

import UI.Class

data Color = Slate
           | Gray
           | Zinc
           | Neutral
           | Stone
           | Red
           | Orange
           | Amber
           | Yellow
           | Lime
           | Green
           | Emerald
           | Teal
           | Cyan
           | Sky
           | Blue
           | Indigo
           | Violet
           | Purple
           | Fuchsia
           | Pink
           | Rose

instance Show Color where
    show Slate   = "slate"
    show Gray    = "gray"
    show Zinc    = "zinc"
    show Neutral = "neutral"
    show Stone   = "stone"
    show Red     = "red"
    show Orange  = "orange"
    show Amber   = "amber"
    show Yellow  = "yellow"
    show Lime    = "lime"
    show Green   = "green"
    show Emerald = "emerald"
    show Teal    = "teal"
    show Cyan    = "cyan"
    show Sky     = "sky"
    show Blue    = "blue"
    show Indigo  = "indigo"
    show Violet  = "violet"
    show Purple  = "purple"
    show Fuchsia = "fuchsia"
    show Pink    = "pink"
    show Rose    = "rose"

color :: Color -> Text
color = pack . show

type Theme :: (* -> * -> *) -> Constraint

class Theme a where
    primaryColor :: Color
    primaryColor = Red

    grayScale :: Color
    grayScale = Neutral

textColor :: Theme UI => Text
textColor = "text-" <> color (grayScale @UI) <> "-900"

textLight :: Theme UI => Text
textLight = "text-" <> color (grayScale @UI) <> "-200"

textMedium :: Theme UI => Text
textMedium = "text-" <> color (grayScale @UI) <> "-500"

textPrimary :: Theme UI => Text
textPrimary = "text-" <> color (primaryColor @UI) <> "-500"

borderColor :: Theme UI => Text
borderColor = "border-" <> color (grayScale @UI) <> "-200"

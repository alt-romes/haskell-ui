{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
The user should @instance Theme UI@ and override the defaults before using UI,
many UI functions depend on 'UI' instancing 'Theme'.
-}
module UI.Theme where

import Data.Kind
import Data.Text (Text, pack)

import UI.Class

type Theme :: (* -> *) -> Constraint

class Theme a where
    primaryColor :: Color
    primaryColor = Red

    grayScale :: Color
    grayScale = Neutral

    listsYPadding :: Padding
    listsYPadding = PY S3

data Color = Slate -- ^ Coldest gray
           | Gray
           | Zinc
           | Neutral
           | Stone -- ^ Warmest gray
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

-- | Convert a color to its textual value
color :: Color -> Text
color = pack . show

-- | A representational size
data Size = S0  -- ^ Size 0
          | SPX -- ^ Size 1 pixel
          | SH  -- ^ Size 0.5
          | S1  -- ^ Size 1
          | S1H -- ^ Size 1.5
          | S2  -- ^ Size 2
          | S2H -- ^ Size 2.5
          | S3  -- ^ as above...
          | S3H
          | S4
          | S5
          | S6
          | S7
          | S8
          | S9
          | S10
          | S11
          | S12
          | S14
          | S16
          | S20
          | S24
          | S28
          | S32
          | S36
          | S40
          | S44
          | S48
          | S52
          | S56
          | S60
          | S64
          | S72
          | S80
          | S96
          | SAuto -- ^ Auto keyword (margin: auto, padding: auto, etc)

-- | Convert a size into the corresponding CSS size value
size :: Size -> Text
size S0    = "0"
size SPX   = "1px"
size SH    = "0.125rem"
size S1    = "0.25rem"
size S1H   = "0.375rem"
size S2    = "0.5rem"
size S2H   = "0.625rem"
size S3    = "0.75rem"
size S3H   = "0.875rem"
size S4    = "1rem"
size S5    = "1.25rem"
size S6    = "1.5rem"
size S7    = "1.75rem"
size S8    = "2rem"
size S9    = "2.25rem"
size S10   = "2.5rem"
size S11   = "2.75rem"
size S12   = "3rem"
size S14   = "3.5rem"
size S16   = "4rem"
size S20   = "5rem"
size S24   = "6rem"
size S28   = "7rem"
size S32   = "8rem"
size S36   = "9rem"
size S40   = "10rem"
size S44   = "11rem"
size S48   = "12rem"
size S52   = "13rem"
size S56   = "14rem"
size S60   = "15rem"
size S64   = "16rem"
size S72   = "18rem"
size S80   = "20rem"
size S96   = "24rem"
size SAuto = "auto"

-- | CSS Margin
data Margin = MT Size -- ^ Margin Top
            | MR Size -- ^ Margin Right
            | MB Size -- ^ Margin Bottom
            | ML Size -- ^ Margin Left
            | MX Size -- ^ Margin X Axis
            | MY Size -- ^ Margin Y Axis
            | MA Size -- ^ Margin all sides
            | M Size Size Size Size  -- ^ Margin Top Right Bottom Left

-- | CSS Padding
data Padding = PT Size -- ^ Padding Top
             | PR Size -- ^ Padding Right
             | PB Size -- ^ Padding Bottom
             | PL Size -- ^ Padding Left
             | PX Size -- ^ Padding X Axis
             | PY Size -- ^ Padding Y Axis
             | PA Size -- ^ Padding all sides
             | P Size Size Size Size  -- ^ Padding Top Right Bottom Left

data Align = ALeft -- ^ Align start/left
           | ACenter -- ^ Align center
           | ARight -- ^ Align end/right

newtype ItemsAlign = ItemsAlign Align

-- | A type of which values can be converted by itself to a CSS style property
class Style a where
    style :: a -> Text

instance Style Margin where
    style (MT s) = "margin-top: " <> size s <> ";"
    style (MR s) = "margin-right: " <> size s <> ";"
    style (MB s) = "margin-bottom: " <> size s <> ";"
    style (ML s) = "margin-left: " <> size s <> ";"
    style (MX s) = "margin-left: " <> size s <> "; " <> "margin-right: " <> size s <> ";"
    style (MY s) = "margin-top: " <> size s <> "; " <> "margin-bottom: " <> size s <> ";"
    style (MA s) = "margin: " <> size s <> ";"
    style (M t r b l) = "margin: " <> size t <> " " <> size r <> " " <> size b <> " " <> size l <> ";"

instance Style Padding where
    style (PT s) = "padding-top: " <> size s <> ";"
    style (PR s) = "padding-right: " <> size s <> ";"
    style (PB s) = "padding-bottom: " <> size s <> ";"
    style (PL s) = "padding-left: " <> size s <> ";"
    style (PX s) = "padding-left: " <> size s <> "; " <> "padding-right: " <> size s <> ";"
    style (PY s) = "padding-top: " <> size s <> "; " <> "padding-bottom: " <> size s <> ";"
    style (PA s) = "padding: " <> size s <> ";"
    style (P t r b l) = "padding: " <> size t <> " " <> size r <> " " <> size b <> " " <> size l <> ";"

instance Style ItemsAlign where
    style (ItemsAlign ALeft) = "align-items: flex-start;"
    style (ItemsAlign ACenter) = "align-items: center;"
    style (ItemsAlign ARight) = "align-items: flex-end;"

type ClassName = Text

textColor :: Theme UI => ClassName
textColor = "text-" <> color (grayScale @UI) <> "-900"

textLight :: Theme UI => ClassName
textLight = "text-" <> color (grayScale @UI) <> "-200"

textMedium :: Theme UI => ClassName
textMedium = "text-" <> color (grayScale @UI) <> "-500"

textDarker :: Theme UI => ClassName
textDarker = "text-" <> color (grayScale @UI) <> "-700"

textPrimary :: Theme UI => ClassName
textPrimary = "text-" <> color (primaryColor @UI) <> "-500"

borderColor :: Theme UI => ClassName
borderColor = "border-" <> color (grayScale @UI) <> "-200"

borderPrimary :: Theme UI => ClassName
borderPrimary = "border-" <> color (primaryColor @UI) <> "-500"

divideColor :: Theme UI => ClassName
divideColor = "divide-" <> color (grayScale @UI) <> "-200"

ringPrimary :: Theme UI => ClassName
ringPrimary = "ring-" <> color (primaryColor @UI) <> "-500"


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
UI elements for inputting data
-}
module UI.Input
    ( input
    , inputP
    , inputC
    ) where

import Data.Text

import UI.Theme
import UI.Class


-- | Simple text input box
input :: Theme UI => Text -> UI (Dynamic Text)
input = fmap value . input' IText id
{-# INLINE input #-}

-- | Simple password input box
inputP :: Theme UI => Text -> UI (Dynamic Text)
inputP = fmap value . input' IPassword id
{-# INLINE inputP #-}

-- | Simple text input box that is cleared when the given event fires.
inputC :: Theme UI => Text -> Event a -> UI (Dynamic Text)
inputC t evt = value <$> input' IText (inputElementConfig_setValue .~ ("" <$ evt)) t

data InputType = IText | IPassword
instance Show InputType where
    show IText = "text"
    show IPassword = "password"

-- | Complex Input that takes a Lens/Function to modify the InputElConfig
-- and returns the full InputElement
input' :: Theme UI
       => InputType
       -> (InputElementConfig -> InputElementConfig) -- ^ Lens/Function to modify the InputElConfig
       -> Text -- ^ Placeholder
       -> UI InputElement
input' inputType confLens placeholder =
    inputElement (def & (initialAttributes .~
                         (  "type" =: (pack . show) inputType
                         <> "class" =: inputClass
                         <> "placeholder" =: placeholder)) . confLens)

inputClass :: Theme UI => Text
inputClass = borderColor <> (" focus:" <> borderPrimary) <> (" focus:" <> ringPrimary) <> " border px-3 py-2 rounded-lg shadow-sm focus:outline-none focus:ring-1"

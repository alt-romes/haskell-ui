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
input :: Theme UI => Text -> UI t (Dynamic t Text)
input = fmap value . input' IText id
{-# INLINE input #-}

-- | Simple password input box
inputP :: Theme UI => Text -> UI t (Dynamic t Text)
inputP = fmap value . input' IPassword id
{-# INLINE inputP #-}

-- | Simple text input box that is cleared when the given event fires.
inputC :: Theme UI => Reflex t => Text -> Event t a -> UI t (Dynamic t Text)
inputC t evt = value <$> input' IText (inputElementConfig_setValue .~ ("" <$ evt)) t

data InputType = IText | IPassword
instance Show InputType where
    show IText = "text"
    show IPassword = "password"

-- | Complex Input that takes a Lens/Function to modify the InputElConfig
-- and returns the full InputElement
input' :: Theme UI
       => InputType
       -> (InputElementConfig EventResult t GhcjsDomSpace -> InputElementConfig EventResult t GhcjsDomSpace) -- ^ Lens/Function to modify the InputElConfig
       -> Text -- ^ Placeholder
       -> UI t (InputElement EventResult GhcjsDomSpace t)
input' inputType confLens placeholder = UI do
    inputElement (def & (initialAttributes .~
                         (  "type" =: (pack . show) inputType
                         <> "class" =: inputClass
                         <> "placeholder" =: placeholder)) . confLens)

-- -- | Simple Input with a Label
-- inputL :: Text -> UI t (Dynamic t Text)
-- inputL t = UI $ el "div" $ unUI $ do
--     label t
--     input

inputClass :: Theme UI => Text
inputClass = borderColor <> (" focus:" <> borderPrimary) <> (" focus:" <> ringPrimary) <> " border px-3 py-2 rounded-lg shadow-sm focus:outline-none focus:ring-1"

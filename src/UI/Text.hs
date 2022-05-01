{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
UI elements for displaying text
-}
module UI.Text where

import Data.Text

import qualified Reflex.Dom as D

import UI.Class
import UI.Theme

-- | Add a label above the given UI
label :: Theme UI => Text -> UI t a -> UI t a
label t e = UI $ el "div" $ do
    elClass "label" (textDarker <> " block text-sm font-medium mb-1") $ D.text t
    unUI e

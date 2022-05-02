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
label :: Theme UI => Text -> UI a -> UI a
label t e = el "div" $ do
    elClass "label" (textDarker <> " block text-sm font-medium mb-1") $ D.text t
    e

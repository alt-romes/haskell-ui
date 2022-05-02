{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Extended where

import Data.Text as T (Text, unwords)

import UI.Class
import UI.Theme

import qualified Reflex.Dom as D

data Side = T | R | B | L

-- data Attribute = Raw Text
--                | P [Side] Int

type Attribute = Text
type ClassName = Text

renderAttrs :: [Attribute] -> ClassName
renderAttrs = T.unwords
    -- where
        -- renderAttr :: Attribute -> ClassName
        -- renderAttr (Raw x) = x
        -- renderAttr (P [] x) = "p-" <> pack (show x)
        -- renderAttr (P ls x) = let x' = pack (show x) in
        --                         foldr ((<>) . (<> " ") . (\y -> "p" <> y <> "-" <> x') . renderSide) "" ls

        -- renderSide :: Side -> Text
        -- renderSide T = "t"
        -- renderSide R = "r"
        -- renderSide B = "b"
        -- renderSide L = "l"

---- Button ------------

button_ :: Theme UI => [Attribute] -> Text -> UI (Event ())
button_ attrs t = UI $ do
    (btn, _) <- elClass' "button" (renderAttrs ("button":attrs) <> " active:" <> borderPrimary) $ D.text t
    return $ domEvent Click btn

------------------------


--- Unused... ---

data UIM = UIM :-: UIM
          | Attribute :> UIM
          | UIM :< Attribute 
          | ContentView UIM
          | VStack UIM
          | HStack UIM
          | Form [Text] Text
          | Text Text
          | Empty

infixr 0 :>
infixl 0 :<

instance Semigroup UIM where
    f <> x = f :-: x
    {-# INLINE (<>) #-}

instance Monoid UIM where
    mempty = Empty
    {-# INLINE mempty #-}

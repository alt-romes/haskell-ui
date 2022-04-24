{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
module UI.Extended ( module UI.Extended ) where

import Data.Text as T (Text, unwords)
import Reflex.Dom

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


---- Input -------------

input_ :: MonadWidget t ui
       => [Attribute]
       -> ui (Dynamic t Text)
input_ = fmap value . flip input_' id
{-# INLINE input_ #-}

input_' :: MonadWidget t ui
       => [Attribute]
       -> (InputElementConfig EventResult t (DomBuilderSpace ui) -> InputElementConfig EventResult t (DomBuilderSpace ui)) -- ^ Lens/Function to modify the InputElConfig
       -> ui (InputElement EventResult (DomBuilderSpace ui) t)
input_' attrs confLens =
    inputElement (def & (initialAttributes .~ ("type" =: "text" <> "class" =: renderAttrs ("input":attrs))) . confLens)

---- Button ------------

button_ :: MonadWidget t ui => [Attribute] -> Text -> ui (Event t ())
button_ attrs t = do
    (btn, _) <- elClass' "button" (renderAttrs $ "button":attrs) $ text t
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

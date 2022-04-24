{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module UI ( module Reflex.Dom, module UI, UI, Text ) where

import Data.FileEmbed
import Data.Text (Text)
import Control.Monad
import Reflex.Dom hiding (button)

import UI.Extended

-- | The mainUI function takes a root and renders the app
mainUI :: (forall t. UI t ()) -> IO ()
mainUI (UI root) = mainWidgetWithHead headWidget root
        where
            headWidget :: MonadWidget t ui => ui ()
            headWidget = do
                elAttr "meta" ("charset" =: "utf-8") blank
                elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
                el "style" (text $(embedStringFile "./dist/output.css"))

---- Layout ------------

-- | A centered container
contentView :: UI t a -> UI t a
contentView (UI x) = UI $ divClass "container mx-auto py-6 px-4" x

-- | Horizontally stack items
hstack :: UI t a -> UI t a
hstack (UI x) = UI $ divClass "flex flex-row flex-wrap justify-evenly gap-8" x

-- | Vertically stack items
vstack :: UI t a -> UI t a
vstack (UI x) = UI $ divClass "flex flex-col flex-wrap justify-evenly gap-8" x

---- Input -------------

-- | Simplest Input box
input :: UI t (Dynamic t Text)
input = value <$> input' id
{-# INLINE input #-}

-- | Complex Input that takes a Lens/Function to modify the InputElConfig
-- and returns the full InputElement
input' :: (InputElementConfig EventResult t GhcjsDomSpace -> InputElementConfig EventResult t GhcjsDomSpace) -- ^ Lens/Function to modify the InputElConfig
       -> UI t (InputElement EventResult GhcjsDomSpace t)
input' = input_' []
{-# INLINE input' #-}

-- | Simple Input with a Label
inputL :: Text -> UI t (Dynamic t Text)
inputL label = UI $ divClass "" $ do
    elClass "label" "label mb-1" $ text label
    unUI input

-- | Simple Input with a label.
-- The input value is cleared when the @Event@ fires.
inputLC :: Text -> Event t a -> UI t (Dynamic t Text)
inputLC label evt = UI $ divClass "" $ do
    elClass "label" "label mb-1" $ text label
    unUI $ value <$> input' (inputElementConfig_setValue .~ ("" <$ evt))

---- Button ------------

button :: Text -> UI t (Event t ())
button = button_ []
{-# INLINE button #-}

------------------------

form :: [Text] -- ^ List of label texts and inputs
     -> Text   -- ^ Submit button text
     -> UI t (Event t [Text])
form labels submitText = UI $ elClass "form" "form" do
    rec
        inputs <- forM labels $ \labelText -> value <$> do
                divClass "" do
                    elClass "label" "label mb-1" $ text labelText
                    unUI $ input' (inputElementConfig_setValue .~ ("" <$ btnEvt))
        btnEvt <- unUI $ button submitText
    return (btnEvt <~~ distributeListOverDyn inputs)


(<~~) :: Reflex t => Event t b -> Dynamic t a -> Event t a
(<~~) = flip tagPromptlyDyn

infixl 1 <~~

(~~>) :: Reflex t => Dynamic t a -> Event t b -> Event t a
(~~>) = tagPromptlyDyn

infixr 1 ~~>

-- todoListWidget :: MonadWidget t m => Dynamic t [Todo] -> m ()
-- todoListWidget = void . flip simpleList (elClass "p" "block" . dynText . fmap todoText)

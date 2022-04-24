{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module UI ( module Reflex.Dom, module UI, Text ) where

import Data.FileEmbed
import Data.Text (Text)
import Control.Monad
import Reflex.Dom hiding (button)

import UI.Extended

-- | The mainUI function takes a root and renders the app
mainUI :: (forall t ui. MonadWidget t ui => ui ()) -> IO ()
mainUI = mainWidgetWithHead headWidget
        where
            headWidget :: MonadWidget t ui => ui ()
            headWidget = do
                elAttr "meta" ("charset" =: "utf-8") blank
                elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
                el "style" (text $(embedStringFile "./dist/output.css"))

---- Layout ------------

-- | A centered container
contentView :: MonadWidget t ui => ui a -> ui a
contentView = divClass "container mx-auto py-6 px-4"

-- | Horizontally stack items
hstack :: MonadWidget t ui => ui a -> ui a
hstack = divClass "flex flex-row flex-wrap justify-evenly gap-8"

-- | Vertically stack items
vstack :: MonadWidget t ui => ui a -> ui a
vstack = divClass "flex flex-col flex-wrap justify-evenly gap-8"

---- Input -------------

input :: MonadWidget t ui => ui (InputElement EventResult (DomBuilderSpace ui) t)
input = input' id
{-# INLINE input #-}

input' :: MonadWidget t ui
       => (InputElementConfig EventResult t (DomBuilderSpace ui) -> InputElementConfig EventResult t (DomBuilderSpace ui)) -- ^ Lens/Function to modify the InputElConfig
       -> ui (InputElement EventResult (DomBuilderSpace ui) t)
input' = input_' []
{-# INLINE input' #-}

---- Button ------------

button :: MonadWidget t ui => Text -> ui (Event t ())
button = button_ []
{-# INLINE button #-}

------------------------

form :: MonadWidget t ui
     => [Text] -- ^ List of label texts and inputs
     -> Text   -- ^ Submit button text
     -> ui (Event t [Text])
form labels submitText = elClass "form" "form" do
    rec
        inputs <- forM labels $ \labelText -> value <$> do
                divClass "" do
                    elClass "label" "label mb-1" $ text labelText
                    input' (inputElementConfig_setValue .~ ("" <$ btnEvt))
        btnEvt <- button submitText
    return (btnEvt `taggedWith` distributeListOverDyn inputs)


taggedWith :: Reflex t => Event t b -> Dynamic t a -> Event t a
taggedWith = flip tagPromptlyDyn

-- todoListWidget :: MonadWidget t m => Dynamic t [Todo] -> m ()
-- todoListWidget = void . flip simpleList (elClass "p" "block" . dynText . fmap todoText)

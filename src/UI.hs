{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module UI ( module Reflex.Dom, module UI, Text ) where

import Data.FileEmbed
import Data.Text (Text)
import Control.Monad
import Reflex.Dom hiding (button)

-- import UI.Utils

newtype Label = Label Text -- TODO: System image?

data Side = T -- ^ Top
          | R -- ^ Right
          | B -- ^ Bottom
          | L -- ^ Left

{-
-- data UI = TabView [Attribute] [Label] [UI]
        -- | ScrollView [Attribute] [UI]
        -- | VStack [Attribute] [UI] -- ^ Organize contents vertically
        -- | HStack [Attribute] [UI] -- ^ Organize contents horizontally
        -- | UIText [Attribute] Text -- ^ Content text
        -- | UIButton [Attribute] Text (forall a b. a -> b) -- ^ A button takes a function

-- data UI = VStack [Attribute] [UI]
--         | UIText [Attribute] Text
-}

data Padding = P [Side] Int

data Attribute = Raw Text
               | Padding Padding

-- | The mainUI function takes a root and renders the app
mainUI :: (forall t m. MonadWidget t m => m ()) -> IO ()
mainUI rootWidget = do
    mainWidgetWithHead headWidget rootWidget

-- | A default <head></head> with metas and a stylesheet
-- Which inits TailwindCSS
headWidget :: MonadWidget t m => m ()
headWidget = do
    elAttr "meta" ("charset" =: "utf-8") blank
    elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
    -- elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdn.jsdelivr.net/npm/bulma@0.9.3/css/bulma.min.css") blank
    el "style" (text $(embedStringFile "dist/output.css"))

-- | A centered container
contentView :: MonadWidget t m => m a -> m a
contentView = divClass "container mx-auto py-6 px-4"

hstack :: MonadWidget t m => m a -> m a
hstack = divClass "grid sm:grid-flow-col sm:auto-cols-max justify-evenly md:gap-2 gap-8"

form :: MonadWidget t m
     => [Text] -- ^ List of label texts and inputs
     -> Text   -- ^ Submit button text
     -> m (Event t [Text])
form labels submitText = elClass "form" "form" do
    rec
        inputs <- forM labels $ \labelText -> value <$> do
                divClass "" do
                    elClass "label" "label mb-1" $ text labelText
                    inputElement (def
                        & (initialAttributes .~ ("type" =: "text" <> "class" =: "input"))
                        . (inputElementConfig_setValue .~ ("" <$ btnEvt)))
        btnEvt <- button submitText
    return (btnEvt `taggedWith` distributeListOverDyn inputs)

input :: MonadWidget t m => m (InputElement EventResult (DomBuilderSpace m) t)
input = inputElement (def & initialAttributes .~ ("type" =: "text" <> "class" =: "input"))

-- TODO: Take a lens for input config instead of change event
input' :: MonadWidget t m => Event t Text -> m (InputElement EventResult (DomBuilderSpace m) t)
input' changeValueEvt =
    inputElement (def
        & (initialAttributes .~ ( "type" =: "text" <> "class" =: "input"))
        . (inputElementConfig_setValue .~ changeValueEvt))

button :: MonadWidget t m => Text -> m (Event t ())
button t = do
    (btn, _) <- elClass' "button" "button" $ text t
    return $ domEvent Click btn

taggedWith :: Reflex t => Event t b -> Dynamic t a -> Event t a
taggedWith = flip tagPromptlyDyn

-- todoListWidget :: MonadWidget t m => Dynamic t [Todo] -> m ()
-- todoListWidget = void . flip simpleList (elClass "p" "block" . dynText . fmap todoText)

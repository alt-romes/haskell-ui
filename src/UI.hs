{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module UI ( module Reflex.Dom, module UI, Text ) where

import Data.Functor.Foldable

import Data.FileEmbed
import Data.Text (Text)
import Control.Monad
import Reflex.Dom hiding (button)

data Side = T | R | B | L

data Attribute = Raw Text
               | Padding [Side] Int

data UI a = forall b. UI b :-: UI a
          | UI a :!: Attribute
          | ContentView (UI a)
          | VStack (UI a)
          | HStack (UI a)
          | Text Text a
          | Empty a

deriving instance Functor UI

-- makeBaseFunctor ''UI

data UIF a b = forall c. UI c :-:$ b 
             | b :!:$ Attribute
             | ContentViewF b
             | VStackF b
             | HStackF b
             | TextF Text a
             | EmptyF a

deriving instance Functor (UIF a)
deriving instance Foldable (UIF a)
deriving instance Traversable (UIF a)

type instance Base (UI a) = UIF a

instance Recursive (UI a) where
      project (x :-: y) = x :-:$ y
      project (x :!: y) = x :!:$ y
      project (ContentView x) = ContentViewF x
      project (VStack x) = VStackF x
      project (HStack x) = HStackF x
      project (Text t x) = TextF t x
      project (Empty x) = EmptyF x

instance Corecursive (UI a) where
      embed (x :-:$ y) = x :-: y
      embed (x :!:$ y) = x :!: y
      embed (ContentViewF x) = ContentView x
      embed (VStackF x) = VStack x
      embed (HStackF x) = HStack x
      embed (TextF t x) = Text t x
      embed (EmptyF x) = Empty x

extract :: UI a -> a
extract = cata go where
    go :: UIF a a -> a
    go (x :!:$ _) = x
    go (TextF _ x) = x
    go (EmptyF x) = x
    go fs = case foldr (:) [] fs of
        x:xs -> last (x:xs)
        _ -> error "All other constructors should have at least one (UI a)   \
                   \ argument, and extract `a` from the last (UI a) argument."

instance Applicative UI where
    pure x = Empty x
    {-# INLINE pure #-}
    f <*> x = f :-: x :-: Empty (extract f $ extract x)
    {-# INLINE (<*>) #-}

instance Monad UI where
    x >>= f = x :-: f (extract x)
    {-# INLINE (>>=) #-}

main2 = do
    ContentView $ do

            HStack $ do

                Text "Hi" ()

                VStack $ do

                    Text "Ho" True
                    Text "He" ()

-- import UI.Utils

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
    el "style" (text $(embedStringFile "./dist/output.css"))

-- | A centered container
contentView :: MonadWidget t m => m a -> m a
contentView = divClass "container mx-auto py-6 px-4"

-- | Horizontally stack items
hstack :: MonadWidget t m => m a -> m a
hstack = divClass "flex flex-row flex-wrap justify-evenly gap-8"

-- | Vertically stack items
vstack :: MonadWidget t ui => ui a -> ui a
vstack = divClass "flex flex-col flex-wrap justify-evenly gap-8"

form :: MonadWidget t ui
     => [Text] -- ^ List of label texts and inputs
     -> Text   -- ^ Submit button text
     -> ui (Event t [Text])
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

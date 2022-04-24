{-# LANGUAGE FlexibleContexts #-}
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

data UI t a = forall b. UI t b :-: UI t a
          | Attribute :> UI t a
          | UI t a :< Attribute 
          | ContentView (UI t a)
          | VStack (UI t a)
          | HStack (UI t a)
          | Form [Text] Text a
          | Text Text a
          | Empty a

infixr 0 :>
infixl 0 :<

deriving instance Functor (UI t)

data UIF t a b = forall c. UI t c :-:$ b 
             | Attribute :>$ b
             | b :<$ Attribute 
             | ContentViewF b
             | VStackF b
             | HStackF b
             | FormF [Text] Text a
             | TextF Text a
             | EmptyF a

deriving instance Functor (UIF t a)
deriving instance Foldable (UIF t a)
deriving instance Traversable (UIF t a)

type instance Base (UI t a) = UIF t a

instance Recursive (UI t a) where
      project (x :-: y) = x :-:$ y
      project (x :> y) = x :>$ y
      project (x :< y) = x :<$ y
      project (ContentView x) = ContentViewF x
      project (VStack x) = VStackF x
      project (HStack x) = HStackF x
      project (Form x y z) = FormF x y z
      project (Text t x) = TextF t x
      project (Empty x) = EmptyF x

instance Corecursive (UI t a) where
      embed (x :-:$ y) = x :-: y
      embed (x :>$ y) = x :> y
      embed (x :<$ y) = x :< y
      embed (ContentViewF x) = ContentView x
      embed (VStackF x) = VStack x
      embed (HStackF x) = HStack x
      embed (FormF x y z) = Form x y z
      embed (TextF t x) = Text t x
      embed (EmptyF x) = Empty x

extract :: UI t a -> a
extract = cata go where
    go :: UIF t a a -> a
    go fs = case foldr (:) [] fs of
        x:xs -> last (x:xs)
        _ -> error "All other constructors should have at least one (UI a) \
                   \ argument, and should extract `a` from the last (UI a) argument."

instance Applicative (UI t) where
    pure x = Empty x
    {-# INLINE pure #-}
    f <*> x = f :-: x :-: Empty (extract f $ extract x)
    {-# INLINE (<*>) #-}

instance Monad (UI t) where
    x >>= f = x :-: f (extract x)
    {-# INLINE (>>=) #-}

form' ::
      [Text] -- ^ List of label texts and inputs
      -> Text   -- ^ Submit button text
      -> UI t (Event t [Text])
form' = undefined

main2 :: (Reflex t, MonadHold t (UI t)) => UI t ()
main2 = do
    Raw "hi" :> Padding [] 15 :> ContentView $ do

            HStack $ do

                Text "Hi" ()

                VStack $ do

                    addEvt <- form' ["Movie Name", "Movie Year", "Director", "Rating"] "Add Movie" 
                    dynEvt <- holdDyn [] addEvt

                    Text "Ho" ()
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

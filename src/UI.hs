{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module UI ( module UI, UI, Text ) where

import Data.FileEmbed
import Data.Text (Text, pack)

import qualified Reflex.Dom as R

import UI.Extended

-- | The mainUI function takes a root and renders the app
mainUI :: (forall t. Reflex t => UI t ()) -> IO ()
mainUI (UI root) = mainWidgetWithHead headWidget root
        where
            headWidget :: MonadWidget t ui => ui ()
            headWidget = do
                elAttr "meta" ("charset" =: "utf-8") R.blank
                elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") R.blank
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
vstack (UI x) = UI $ divClass "flex flex-col flex-wrap gap-8" x

-- | Vertically stack items inside the semantic <form></form> tags
form :: UI t a -> UI t a
form (UI x) = UI $ elClass "form" "flex flex-col flex-wrap gap-8" x

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

---- Dyn ---------------

display :: Show a => Dynamic t a -> UI t ()
display x = UI (R.dynText (pack . show <$> x))

dynText :: Dynamic t Text -> UI t ()
dynText x = UI (R.dynText x)

dynIf :: Dynamic t Bool -> UI t a -> UI t a -> UI t (Event t a)
dynIf b (UI x) (UI y) = UI (dyn ((\case True -> x; False -> y) <$> b))

---- Routing! ----------

-- Router! Receives an initial value and a function that transforms
-- values of the same type into UI holding value (route-changing) generating events
--
-- Example usage:
-- @
--   mainUI $
--     router "/login" $ \case
--
--       "/login" -> do
--           loginBtnEv <- loginForm
--           return ("/main" <$ loginEv)

--       "/main" -> do
--           transition <- mainContent
--           return transition
-- 
--       _ -> do
--           ev <- button "Return to login"
--           return ("/login" <$ ev)
-- @
router :: Reflex t => a -> (a -> UI t (Event t a)) -> UI t ()
router initialRoute routerF = UI $ mdo
    wow <- fmap (fmap routerF) <$> widgetHold (unUI $ routerF initialRoute) (unUI <$> switchDyn wow)
    return ()

---- Other -------------

blank :: UI t ()
blank = return ()

(<~~) :: Reflex t => Event t b -> Dynamic t a -> Event t a
(<~~) = flip tagPromptlyDyn

infixl 1 <~~

(~~>) :: Reflex t => Dynamic t a -> Event t b -> Event t a
(~~>) = tagPromptlyDyn

infixr 1 ~~>

------------------------

-- todoListWidget :: MonadWidget t m => Dynamic t [Todo] -> m ()
-- todoListWidget = void . flip simpleList (elClass "p" "block" . dynText . fmap todoText)

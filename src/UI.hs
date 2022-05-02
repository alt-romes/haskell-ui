{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module UI ( module UI, UI, Text, NominalDiffTime) where

import Data.FileEmbed
import Data.Text (Text, pack)
import Data.Time (NominalDiffTime)

import Control.Monad (forM, (>=>))

import qualified Reflex.Dom as D

import UI.Extended
import UI.Icons
import UI.Theme
import UI.Class

-- | The mainUI function takes a root and renders the app
mainUI :: UI () -> IO ()
mainUI (UI root) = mainWidgetWithHead headWidget $ do
    divClass "flex flex-col h-screen overflow-hidden root" root
    where
        headWidget :: MonadWidget t ui => ui ()
        headWidget = do
            elAttr "meta" ("charset" =: "utf-8") D.blank
            elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") D.blank
            el "style" (D.text $(embedStringFile "./dist/output.css"))

---- Layout ------------

paddingContainer :: UI a -> UI a
paddingContainer = divClass "p-3"

paddingYContainer :: UI a -> UI a
paddingYContainer = divClass "py-3"

-- | Horizontally stack items
hstack :: UI a -> UI a
hstack = divClass "flex flex-row flex-nowrap gap-4 items-center w-full justify-center"

-- | Vertically stack items
vstack :: UI a -> UI a
vstack = divClass "flex flex-col gap-4 items-center w-full justify-center"

-- | Vertically stack items with the given gap in between elements.
-- Available gap sizes are 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6, 7, 8... 24
-- The intermediate initial values aren't currently available...
vstack' :: Int -> UI a -> UI a
vstack' size = divClass ("flex flex-col h-auto gap-" <> (pack . show) size)

spacer :: UI ()
spacer = divClass "flex-1" blank

listClass :: Theme UI => Text
listClass = borderColor <> " " <> divideColor <> " flex flex-col border-b border-t divide-y list w-full" 

-- | Display a dynamic list of values
-- Return a dynamic list of the values returned by the UI created for each list item
list :: Theme UI => Dynamic [a] -> (Dynamic a -> UI b) -> UI (Dynamic [b])
list l = divClass listClass . D.simpleList l

-- | Like 'list', but returns an event that fires when any item of the list is
-- clicked, with the value returned by the 'UI' generating function for that item.
listE :: Theme UI => Dynamic [a] -> (Dynamic a -> UI b) -> UI (Event b)
listE l f =
    divClass listClass do
        mergeDynEvts <$> D.simpleList l \i -> do
            (e, x) <- elClass' "div" "cursor-pointer" (f i)
            return (x <$ domEvent Click e)

menu :: Theme UI => [a] -> (a -> UI b) -> UI [b]
menu l = divClass listClass . forM l

menuE :: Theme UI => [a] -> (a -> UI b) -> UI (Event b)
menuE l f =
    divClass listClass do
        leftmost <$> forM l \i -> do
            (e, x) <- elClass' "div" "cursor-pointer" (f i)
            return (x <$ domEvent Click e)

-- | Turn a dynamic list of (XOR) events (meaning only one of the events can
-- occur at a time) into an event that occurs every time one of the events
-- occurs.
--
-- This is particularly useful for turning the output of 'list' into a single
-- event representing a click on one of the items.
--
-- Example:
--
-- Because of 'mergeDynEvts', @clickListItem@ is an event that fires whenever one of the list items is clicked
-- @
-- clickListItem <- mergeDynEvts <$> list ["a","b","c"] (\t -> do
--                       click <- button "Join"
--                       return (t <$ click) 
-- @
mergeDynEvts :: Dynamic [Event a] -> Event a
mergeDynEvts = switchDyn . fmap leftmost

-- | Vertically stack items inside the semantic <form></form> tags
-- form :: UI t a -> UI t a
-- form (UI x) = UI $ elClass "form" "flex flex-col flex-wrap gap-8" x

---- UI ----------------

-- -- | A view for your content.
-- -- At the moment, for vstack with spacers to work correctly, 'contentView'
-- -- should be the parent element
contentView :: UI a -> UI a
-- contentView (UI x) = UI $ divClass "container mx-auto py-8 px-5 h-full flex flex-col" x
contentView = divClass "py-8 px-5"


---- Text --------------

-- | Rounded small image given an URL
imageRS :: Text -> UI ()
imageRS url = elAttr "img" ("src"=:url <> "class"=:"h-10 w-10 rounded-full object-cover") blank

-- | Dynamic Image (the image will change when the dynamic url is updated)
imageD :: Dynamic Text -> UI ()
imageD url = elDynAttr "img" (("class"=:"object-cover max-w-56 max-h-56 rounded-md" <>) . ("src"=:) <$> url) blank

heading :: Theme UI => Text -> UI ()
heading = elClass "h3" (textColor <> " pt-6 pb-2 text-2xl font-semibold w-2/3") . text

navigationTitle :: Theme UI => Text -> UI ()
navigationTitle = elClass "h1" (textColor <> " pt-6 pb-2 text-4xl font-bold w-2/3") . text

-- TODO: Unify navigationBar and navigationTitle

-- | Create a navigation bar without a back button, so basically just a top bar
-- with a title
-- navigationBar' :: Maybe Text -> UI t ()
-- navigationBar' titleText = UI do

-- | Label with an Icon
labelI :: Icon -> Text -> UI ()
labelI i t = elClass "label" "label mb-1 flex items-center gap-2" $ do
    el "span" $ renderIcon' 5 "" i
    text t

-- | Label with dynamic text and an Icon
labelI' :: Icon -> Dynamic Text -> UI ()
labelI' i t = elClass "label" "label mb-1 flex flex-nowrap items-center gap-2" $ do
    el "span" $ renderIcon' 5 "" i
    dynText t

display :: Show a => Dynamic a -> UI ()
display = dynText . fmap (pack . show)

dynText :: Dynamic Text -> UI ()
dynText = D.dynText

---- Button ------------

button :: Theme UI => Text -> UI (Event ())
button = button_ []
{-# INLINE button #-}

---- Dyn ---------------

dynIf :: Dynamic Bool -> UI a -> UI a -> UI (Event a)
dynIf b x y = dyn ((\case True -> x; False -> y) <$> b)

---- Other -------------

p :: UI a -> UI a
p = el "p"

text :: Text -> UI ()
text = D.text

blank :: UI ()
blank = return ()

-- | Fire an event every X seconds
timer :: NominalDiffTime -> UI (Event ())
timer x = UI ((() <$) <$> tickLossyFromPostBuildTime x)

-- | Fire an event /now/
now :: UI (Event ())
now = D.now

-- | Fires an event very soon
verySoon :: UI (Event ())
verySoon = timer 0.00000001 >>= headE

-- | Fires an event after X seconds
after :: NominalDiffTime -> UI (Event ())
after = timer >=> headE

clickEvt :: Element -> Event ()
clickEvt = domEvent Click

(<~~) :: Event b -> Dynamic a -> Event a
(<~~) = flip tagPromptlyDyn

infixl 1 <~~


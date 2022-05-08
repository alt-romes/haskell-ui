{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
UI elements to layout other elements
-}
module UI.Layout where

import Data.Text (pack)

import UI.Class
import UI.Theme
import UI

import qualified Reflex.Dom as D

-- | Horizontally stack items
hstack :: UI a -> UI a
hstack = divClass "flex flex-row flex-nowrap gap-4 items-center w-full justify-center"

-- | Vertically stack items
vstack :: UI a -> UI a
vstack = divClass "flex flex-col gap-4 items-center w-full justify-center"

-- | Vertically stack items with the given gap in between elements and a given skew.
-- Available gap sizes are 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6, 7, 8... 24
-- The intermediate initial values aren't currently available...
vstack' :: Align -> Size -> UI a -> UI a
vstack' a s = elAttr "div" ("class"=:"flex flex-col h-auto" <> "style"=:("gap: " <> size s <> "; " <> style (ItemsAlign a)))

-- | Occupy the maximum possible space between two items in a horizontal or
-- vertical stack
spacer :: UI ()
spacer = divClass "flex-1" blank

flexboxE :: Dynamic [a]
        -> (Dynamic a -> UI b)
        -> UI (Event b)
flexboxE l f = do
    divClass "flex flex-row flex-wrap gap-8" do
        mergeDynEvts <$> D.simpleList l \i -> do
            (e, x) <- elClass' "div" "cursor-pointer" (f i)
            return (x <$ domEvent Click e)

-- | Grid with responsive number of columns, fixed for mobile
gridYE :: Int -- ^ Minimum col number
       -> Dynamic [a] -- ^ Items to display
       -> (Dynamic a -> UI b) -- ^ Function that generates a UI from the dynamic item
       -> UI (Event b) -- ^ The built UI and an event that fires when any grid item is clicked
gridYE s items f = do
    -- elDynClass "div" ((\l -> "gap-8 grid grid-cols-" <> (pack . show) size <> " grid-rows-" <> (pack . show) (length l `div` size + 1)) <$> items) do
    divClass ("gap-8 grid grid-cols-" <> (pack . show) s <> " sm:grid-cols-" <> (pack . show) (s+1) <> " md:grid-cols-" <> (pack . show) (s + 2) <> " lg:grid-cols-" <> (pack . show) (s + 3)) do
        mergeDynEvts <$> D.simpleList items \i -> do
            (e, x) <- elClass' "div" "cursor-pointer" (f i)
            return (x <$ domEvent Click e)

-- | Grid with fixed number of rows, that scrolls horizontally to the
-- overflowing items
gridXE :: Int
       -> Dynamic [a]
       -> (Dynamic a -> UI b)
       -> UI (Event b)
gridXE s items f = do
    divClass ("gap-8 snap-x no-scrollbar grid overflow-x-scroll grid-flow-col grid-rows-" <> (pack . show) s) do
        mergeDynEvts <$> D.simpleList items \i -> do
            (e, x) <- elClass' "div" "snap-start cursor-pointer" (f i)
            return (x <$ domEvent Click e)

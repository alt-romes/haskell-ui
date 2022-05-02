{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
UI elements to layout other elements
-}
module UI.Layout where

import Data.Text (pack)

import UI.Class
import UI

import qualified Reflex.Dom as D

flexboxE :: Dynamic [a]
        -> (Dynamic a -> UI b)
        -> UI (Event b)
flexboxE l f = UI do
    divClass "flex flex-row flex-wrap gap-8" do
        mergeDynEvts <$> D.simpleList l \i -> do
            (e, x) <- elClass' "div" "cursor-pointer" (unUI (f i))
            return (x <$ domEvent Click e)

gridYE :: Int
       -> Dynamic [a]
       -> (Dynamic a -> UI b)
       -> UI (Event b)
gridYE size items f = UI do
    -- elDynClass "div" ((\l -> "gap-8 grid grid-cols-" <> (pack . show) size <> " grid-rows-" <> (pack . show) (length l `div` size + 1)) <$> items) do
    divClass ("gap-8 grid grid-cols-" <> (pack . show) size <> " sm:grid-cols-" <> (pack . show) (size+1) <> " md:grid-cols-" <> (pack . show) (size + 2) <> " lg:grid-cols-" <> (pack . show) (size + 3)) do
        mergeDynEvts <$> D.simpleList items \i -> do
            (e, x) <- elClass' "div" "cursor-pointer" (unUI (f i))
            return (x <$ domEvent Click e)

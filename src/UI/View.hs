{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
UI components to define views and navigation between them
-}
module UI.View
    ( scrollView
    , navigationView
    , tabView
    ) where

import Data.Text (Text, pack)

import Control.Monad (forM, forM_, when)

import UI.Class
import UI.Theme
import UI.Icons
import UI.Router

import qualified Reflex.Dom as D

-- | Scroll view!
--
-- Use a scroll view to scroll your content.
--
-- It ensures that overflowing content can be
-- reached. If the content exceeds the page limit, it'll be hidden, however,
-- with scroll view, scrolling is enabled so you can scroll down to the end of
-- the content
scrollView :: UI t a -> UI t a
scrollView (UI x) = UI $ divClass "scroll-smooth overflow-y-scroll scroll-view" x

data NavigationView = Top | Nested

-- | A 'NavigationTitle' is used to specify how the navigation title should be
-- displayed in a 'navigationView'
-- data NavigationTitle = NoNT
--                      | TopNT Text
--                      | FullNT Text

-- | A view for presenting a stack of views that represents a visiblee path in
-- a navigatioin hierarchy.
--
-- Inspired by SwiftUI
-- 
-- Receives the name of the navigation view and a widget that on an event
-- creates another widget
--
-- If the created widget wants to have a title it should set so itself
navigationView :: Theme UI => Reflex t => Text -> UI t (Event t (UI t a)) -> UI t ()
navigationView title topView = do
    router (error "The top view should be rendered statically", Top) $ \case
        (_, Top) -> fmap (,Nested) <$> topView
        (view, Nested) -> do
            back <- navigationBar (Just title) Nothing -- TODO: Make this bar receive the text elsewhere...
            -- todo: backtrack rather than always go to top?
            ((error "The top view should be rendered statically", Top) <$ back) <$ view
    return ()

-- | A content-changing tab view at the bottom.
--
-- Takes an initial tab, a list of tab names, and a routing function (tab name -> ui)
-- The bool indicates whether to display or not the route name under the icon
tabView :: Theme UI => Reflex t => Text -> [(Text, Icon)] -> Bool -> (Text -> UI t a) -> UI t ()
tabView initial ls displayName routing = mdo
    router initial ((clicks <$) <$> routing)
    clicks <- leftmost <$> UI do
        elClass "footer" footerClass $ do
            elClass "ul" ("grid grid-cols-" <> (pack . show . length) ls) $ do
                forM ls $ \(name, i) -> mdo
                    (li, _) <- elClass' "li" ("cursor-pointer flex flex-col items-center" <> if displayName then "" else " pt-1.5 pb-2") $ do
                        itemClass <- holdDyn (mkTabItemClass name initial) (mkTabItemClass name <$> clicks)
                        unUI $ renderIcon' 6 itemClass i
                        when displayName $
                           elDynClass "p" itemClass (D.text name)
                    return (name <$ domEvent Click li)
    return ()
        where footerClass = borderColor <> " inset-x-0 border-t bg-white/40 backdrop-blur-md pt-0.5 px-2"
              mkTabItemClass n x = (if n == x then textPrimary else textColor <> " text-opacity-50") <> " text-center truncate overflow-hidden text-2xs tracking-tight"


-- | Create a navigation bar with a back button (with the first argument), a
-- top title (the second argument) that fires the resulting event when the back button is clicked
navigationBar :: Theme UI => Maybe Text -> Maybe Text -> UI t (Event t ())
navigationBar backText titleText = UI do
    elClass "header" (borderColor <> " inset-x-0 border-b bg-white/40 backdrop-blur-md px-2 py-2") $ do
        elClass "div" "grid grid-cols-6" $ do
            svg <- unUI $ renderIcon'' 6 ((textPrimary <>) <$> " cursor-pointer col-span-1") chevronLeftO
            forM_ titleText (elClass "h1" (textColor <> " font-semibold text-center col-span-4 text-ellipsis overflow-hidden") . D.text)
            return (domEvent Click svg)


{-# LANGUAGE TemplateHaskell #-}
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
import Data.Bifunctor (second)

import Control.Monad (forM, when, forM_)

import qualified Reflex.Dom as D
import Reflex.Network (networkHold)

import UI.Extended
import UI.Icons

-- | The mainUI function takes a root and renders the app
mainUI :: (forall t. Reflex t => UI t ()) -> IO ()
mainUI (UI root) = mainWidgetWithHead headWidget $ do
    divClass "flex flex-col h-screen overflow-hidden root" root
    where
        headWidget :: MonadWidget t ui => ui ()
        headWidget = do
            elAttr "meta" ("charset" =: "utf-8") D.blank
            elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") D.blank
            el "style" (D.text $(embedStringFile "./dist/output.css"))

---- Layout ------------

paddingContainer :: UI t a -> UI t a
paddingContainer (UI x) = UI $ divClass "p-3" x

paddingYContainer :: UI t a -> UI t a
paddingYContainer (UI x) = UI $ divClass "py-3" x

-- | Horizontally stack items
hstack :: UI t a -> UI t a
hstack (UI x) = UI $ divClass "flex flex-row flex-nowrap gap-4 items-center w-full justify-center" x

-- | Vertically stack items
vstack :: UI t a -> UI t a
vstack (UI x) = UI $ divClass "flex flex-col gap-4 items-center w-full justify-center" x

-- | Vertically stack items with the given gap in between elements.
-- Available gap sizes are 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6, 7, 8... 24
-- The intermediate initial values aren't currently available...
vstack' :: Int -> UI t a -> UI t a
vstack' size (UI x) = UI $ divClass ("flex flex-col h-auto gap-" <> (pack . show) size) x

spacer :: UI t ()
spacer = UI $ divClass "flex-1" D.blank

-- | Display a dynamic list of values
-- Return a dynamic list of the values returned by the UI created for each list item
list :: Dynamic t [a] -> (Dynamic t a -> UI t b) -> UI t (Dynamic t [b])
list l f = UI do
    divClass "flex flex-col ml-4 border-neutral-200 border-b border-t divide-y list w-full" do
        D.simpleList l (unUI . f)

-- | Like 'list', but returns an event that fires when any item of the list is
-- clicked, with the value returned by the 'UI' generating function for that item.
listE :: Reflex t => Dynamic t [a] -> (Dynamic t a -> UI t b) -> UI t (Event t b)
listE l f = UI do
    divClass "flex flex-col ml-4 border-neutral-200 border-b border-t divide-y list w-full" do
        mergeDynEvts <$> D.simpleList l \i -> do
            (e, x) <- elClass' "div" "cursor-pointer" (unUI (f i))
            return (x <$ domEvent Click e)

menu :: [a] -> (a -> UI t b) -> UI t [b]
menu l f = UI do
    divClass "flex flex-col ml-4 border-neutral-200 border-b border-t divide-y list w-full" do
        forM l (unUI . f)

menuE :: [a] -> (a -> UI t b) -> UI t (Event t b)
menuE l f = UI do
    divClass "flex flex-col ml-4 border-neutral-200 border-b border-t divide-y list w-full" do
        leftmost <$> forM l \i -> do
            (e, x) <- elClass' "div" "cursor-pointer" (unUI (f i))
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
mergeDynEvts :: Reflex t => Dynamic t [Event t a] -> Event t a
mergeDynEvts = switchDyn . fmap leftmost

-- | Vertically stack items inside the semantic <form></form> tags
-- form :: UI t a -> UI t a
-- form (UI x) = UI $ elClass "form" "flex flex-col flex-wrap gap-8" x

---- UI ----------------

-- DEPRECATED! TODO del
-- -- | A view for your content.
-- -- At the moment, for vstack with spacers to work correctly, 'contentView'
-- -- should be the parent element
-- contentView :: UI t a -> UI t a
-- contentView (UI x) = UI $ divClass "container mx-auto py-8 px-5 h-full flex flex-col" x

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

data NavigationView = Top | Nested (Maybe Text)

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
-- creates another widget and the text naming that widget, if said to create widget has a name
navigationView :: Reflex t => Text -> UI t (Event t (UI t a, Maybe Text)) -> UI t ()
navigationView title topView = do
    router (error "The top view should be rendered statically", Top) $ \case
        (_, Top) -> fmap (second Nested) <$> topView
        (view, Nested t) -> do
            back <- navigationBar (Just title) t
            -- todo: backtrack rather than always go to top
            ((error "The top view should be rendered statically", Top) <$ back) <$ view
    return ()

-- | A content-changing tab view at the bottom.
--
-- Takes an initial tab, a list of tab names, and a routing function (tab name -> ui)
-- The bool indicates whether to display or not the route name under the icon
tabView :: Reflex t => Text -> [(Text, Icon)] -> Bool -> (Text -> UI t a) -> UI t ()
tabView initial ls displayName routing = mdo
    router initial ((clicks <$) <$> routing)
    clicks <- leftmost <$> UI do
        elClass "footer" "inset-x-0 border-t border-neutral-200 bg-white/40 backdrop-blur-md pt-0.5 px-2" $ do
            elClass "ul" ("grid grid-cols-" <> (pack . show . length) ls) $ do
                forM ls $ \(name, i) -> mdo
                    (li, _) <- elClass' "li" ("cursor-pointer flex flex-col items-center" <> if displayName then "" else " pt-1.5 pb-2") $ do
                        itemClass <- holdDyn (if initial == name then "text-red-500" else "text-neutral-900/50")
                                        ((\x -> if x == name then "text-red-500" else "text-neutral-900/50") <$> clicks)
                        unUI $ renderIcon' 6 itemClass i
                        when displayName $
                           elDynClass "p" (("text-center truncate overflow-hidden text-2xs tracking-tight " <>) <$> itemClass) (D.text name)
                    return (name <$ domEvent Click li)
    return ()

---- Text --------------

-- | Rounded small image given an URL
imageRS :: Text -> UI t ()
imageRS url = UI $ elAttr "img" ("src"=:url <> "class"=:"h-10 w-10 rounded-full object-cover") D.blank

heading :: Text -> UI t ()
heading t = UI do
    elClass "h3" "px-4 pt-6 pb-2 text-2xl font-semibold text-neutral-900 dark:text-neutral-100 w-2/3" $ D.text t

navigationTitle :: Text -> UI t ()
navigationTitle t = UI do
    elClass "h1" "px-4 pt-6 pb-2 text-4xl font-bold text-neutral-900 dark:text-neutral-100 w-2/3" $ D.text t

-- TODO: Unify navigationBar and navigationTitle

-- | Create a navigation bar with a back button (with the first argument), a
-- top title (the second argument) that fires the resulting event when the back button is clicked
navigationBar :: Maybe Text -> Maybe Text -> UI t (Event t ())
navigationBar backText titleText = UI do
    elClass "header" "inset-x-0 border-b border-neutral-200 bg-white/40 backdrop-blur-md px-2 py-2" $ do
        elClass "div" "grid grid-cols-6" $ do
            svg <- unUI $ renderIcon'' 6 "cursor-pointer col-span-1 text-red-500" chevronLeftO
            forM_ titleText (elClass "h1" "font-semibold text-center text-neutral-900 col-span-4 text-ellipsis overflow-hidden". D.text)
            return (domEvent Click svg)

-- | Create a navigation bar without a back button, so basically just a top bar
-- with a title
-- navigationBar' :: Maybe Text -> UI t ()
-- navigationBar' titleText = UI do

label :: Text -> UI t ()
label t = UI $ elClass "label" "label mb-1" $ D.text t

-- | Label with an Icon
labelI :: Icon -> Text -> UI t ()
labelI i t = UI $ elClass "label" "label mb-1 flex items-center gap-2" $ do
    el "span" $ unUI $ renderIcon' 5 "" i
    D.text t

-- | Label with dynamic text and an Icon
labelI' :: Icon -> Dynamic t Text -> UI t ()
labelI' i t = UI $ elClass "label" "label mb-1 flex flex-nowrap items-center gap-2" $ do
    el "span" $ unUI $ renderIcon' 5 "" i
    D.dynText t

display :: Show a => Dynamic t a -> UI t ()
display x = UI (D.dynText (pack . show <$> x))

dynText :: Dynamic t Text -> UI t ()
dynText x = UI (D.dynText x)

---- Input -------------

data InputType = IText | IPassword

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
inputL t = UI $ el "div" $ unUI $ do
    label t
    input

-- | Simple Input with a label.
-- The input value is cleared when the @Event@ fires.
inputLC :: Text -> Event t a -> UI t (Dynamic t Text)
inputLC t evt = UI $ el "div" $ unUI $ do
    label t
    value <$> input' (inputElementConfig_setValue .~ ("" <$ evt))

-- | Simple password input with a Label
inputPL :: Text -> UI t (Dynamic t Text)
inputPL t = UI $ el "div" $ unUI $ do
    label t
    value <$> inputP_' [] id

---- Button ------------

button :: Text -> UI t (Event t ())
button = button_ []
{-# INLINE button #-}

---- Dyn ---------------

dynIf :: Dynamic t Bool -> UI t a -> UI t a -> UI t (Event t a)
dynIf b (UI x) (UI y) = UI (dyn ((\case True -> x; False -> y) <$> b))

---- Routing! ----------

-- | Router! Receives an initial value and a function that transforms
-- values of the same type into UI holding value (route-changing) generating events
--
-- The holding value can be used to pass state around the router
--
-- Note: Using the event 'now' here seems to cause problems. Use a small 'timer' or 'next' instead.
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
--
-- Example usage of the router! with state:
--
-- @
-- main :: IO ()
-- main = mainUI $ do
--
--     router' ("/login", Nothing) $ \case
--
--         ("/login", Nothing) -> do
--             loginEv <- userLoginPage "xyi"
--             return ((\case Nothing -> ("/login", Nothing); Just s -> ("/main", Just s)) <$> loginEv)
--
--         ("/login", Just session) -> return (("/main", Just session) <$ now)
--
--         ("/main", Nothing) -> return (("/login", Nothing) <$ now)
--
--         ("/main", Just session) -> do
--             mainContent session
--             x <- button "Go To Unknown"
--             y <- button "Go To Login"
--             return ((, Just session) <$> (("//" <$ x) <> ("/login" <$ y)))
--
--         (_, session) -> do
--             x <- button "Unknown"
--             return (("/login", session) <$ x)
-- @
router :: a -> (a -> UI t (Event t a)) -> UI t ()
router initialRoute routerF = UI $ mdo
    wow <- fmap (fmap routerF) <$> networkHold (unUI $ routerF initialRoute) (unUI <$> switchDyn wow)
    return ()

-- | Start at a UI returning an Event that when triggered will pass the value to
-- a UI creating function.
--
-- Contrary to @router@, @path@ has a linear flow (from page A to page B and
-- viceversa) and information from page A can be passed to page B.
--
-- @
--     path (userLoginPage :: UI t (Event t Token)) $ \tok -> do
--         dbTable "XYZ" tok    -- dbTable :: String -> Token -> UI t ()
--         return never         -- never return to the previous page
-- @
path :: UI t (Event t a) -> (a -> UI t b) -> UI t ()
path (UI g) f = UI $ mdo
    pathEv <- networkHold g ((never <$) . unUI . f <$> switchDyn pathEv)
    return ()


---- Other -------------

p :: UI t a -> UI t a
p x = UI (el "p" $ unUI x)

text :: Text -> UI t ()
text x = UI (D.text x)

blank :: UI t ()
blank = return ()

-- | Fire an event every X seconds
timer :: NominalDiffTime -> UI t (Event t ())
timer x = UI ((() <$) <$> tickLossyFromPostBuildTime x)

-- | Fire an event /now/
now :: UI t (Event t ())
now = UI D.now

-- | Fires an event very soon
verySoon :: UI t (Event t ())
verySoon = UI (unUI (timer 0.00000001) >>= headE)

-- | Fires an event after X seconds
after :: NominalDiffTime -> UI t (Event t ())
after t = UI (unUI (timer t) >>= headE)

clickEvt :: Reflex t => Element EventResult GhcjsDomSpace t -> Event t ()
clickEvt = domEvent Click

(<~~) :: Reflex t => Event t b -> Dynamic t a -> Event t a
(<~~) = flip tagPromptlyDyn

infixl 1 <~~

-- | The same as @path@, but more visually appealing
(~~>) :: UI t (Event t a) -> (a -> UI t (Event t a)) -> UI t ()
(~~>) = path

infixr 1 ~~>

------------------------

-- todoListWidget :: MonadWidget t m => Dynamic t [Todo] -> m ()
-- todoListWidget = void . flip simpleList (elClass "p" "block" . dynText . fmap todoText)

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
module UI ( module UI, UI, Text, NominalDiffTime) where

import Data.FileEmbed
import Data.Text (Text, pack)
import Data.Time (NominalDiffTime)

import Control.Monad (forM, when)

import qualified Reflex.Dom as D
import Reflex.Network (networkHold)

import UI.Extended
import UI.Icons

-- | The mainUI function takes a root and renders the app
mainUI :: (forall t. Reflex t => UI t ()) -> IO ()
mainUI (UI root) = mainWidgetWithHead headWidget root
        where
            headWidget :: MonadWidget t ui => ui ()
            headWidget = do
                elAttr "meta" ("charset" =: "utf-8") D.blank
                elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") D.blank
                el "style" (D.text $(embedStringFile "./dist/output.css"))

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

---- UI ----------------

-- | A content-changing tab view at the bottom.
--
-- Takes an initial tab, a list of tab names, and a routing function (tab name -> ui)
-- The bool indicates whether to display or not the route name under the icon
tabView :: Reflex t => Text -> [(Text, Icon)] -> Bool -> (Text -> UI t a) -> UI t ()
tabView initial ls displayName routing = mdo
    router initial ((leftmost clicks <$) <$> routing)
    clicks <- UI $ do
        elClass "section" "fixed bottom-0 inset-x-0 border-t border-gray/20 bg-white/40 backdrop-blur-md pt-0.5" $ do
            elClass "ul" "flex justify-evenly" $ do
                forM ls $ \(name, i) -> do
                    (li, _) <- elClass' "li" ("flex flex-col items-center" <> if displayName then "" else " pt-1.5 pb-2") $ do
                        unUI $ renderIcon (if displayName then 5 else 6) i
                        when displayName $
                           elClass "p" "text-sm" (D.text name) -- only if name is available
                    return (name <$ domEvent Click li) 
    return ()


---- Input -------------

label :: Text -> UI t ()
label t = UI $ elClass "label" "label mb-1" $ D.text t

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
inputL t = UI $ divClass "" $ unUI $ do
    label t
    input

-- | Simple Input with a label.
-- The input value is cleared when the @Event@ fires.
inputLC :: Text -> Event t a -> UI t (Dynamic t Text)
inputLC t evt = UI $ divClass "" $ unUI $ do
    label t
    value <$> input' (inputElementConfig_setValue .~ ("" <$ evt))

-- inputLT :: Text -> InputType -> UI t (Dynamic t Text)
-- inputLT labelText inputType = UI $ divClass "" $ unUI $ do
--     label labelText
--     value <$> input' ()

---- Button ------------

button :: Text -> UI t (Event t ())
button = button_ []
{-# INLINE button #-}

---- Dyn ---------------

display :: Show a => Dynamic t a -> UI t ()
display x = UI (D.dynText (pack . show <$> x))

dynText :: Dynamic t Text -> UI t ()
dynText x = UI (D.dynText x)

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

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Router where

import Reflex.Network (networkHold)

import UI.Class

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
router :: a -> (a -> UI (Event a)) -> UI ()
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
path :: UI (Event a) -> (a -> UI b) -> UI ()
path (UI g) f = UI $ mdo
    pathEv <- networkHold g ((never <$) . unUI . f <$> switchDyn pathEv)
    return ()

-- | The same as @path@, but more visually appealing
(~~>) :: UI (Event a) -> (a -> UI (Event a)) -> UI ()
(~~>) = path
infixr 1 ~~>

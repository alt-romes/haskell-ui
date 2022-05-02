{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
module UI.Class ( module Reflex.Dom, module UI.Class ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix, mfix)

import Reflex.Class as R (MonadHold(now), Behavior, Dynamic, Event)
import Reflex.Dom.Builder.Class as B (Element, InputElement, InputElementConfig)
import Reflex.Dom hiding (button, display, dynText, blank, now, text, list, simpleList, Behavior, Dynamic, Event, Element, InputElement, InputElementConfig)

type Behavior = R.Behavior Spider
type Event = R.Event Spider
type Dynamic = R.Dynamic Spider
type Element = B.Element EventResult GhcjsDomSpace Spider
type InputElement = B.InputElement EventResult GhcjsDomSpace Spider
type InputElementConfig = B.InputElementConfig EventResult Spider GhcjsDomSpace

newtype UI a = UI { unUI :: forall m. (MonadWidget Spider m) => m a }
    deriving (Functor)

instance Applicative UI where
    pure x = UI $ pure x
    (UI f) <*> (UI a) = UI (f <*> a)

instance Monad UI where
    (UI x) >>= f = UI (x >>= unUI . f)

instance MonadFix UI where
    mfix f = UI $ mfix (unUI . f)

instance MonadIO UI where
    liftIO x = UI $ liftIO x

instance MonadSample Spider UI where
    sample x = UI (sample x)

instance NotReady Spider UI where
    notReadyUntil e = UI (notReadyUntil e)
    notReady = UI notReady

instance Adjustable Spider UI where
    runWithReplace u e = UI (runWithReplace (unUI u) (fmap unUI e))
    traverseIntMapWithKeyWithAdjust f i e = UI (traverseIntMapWithKeyWithAdjust (fmap unUI <$> f) i e)
    traverseDMapWithKeyWithAdjustWithMove f d e = UI (traverseDMapWithKeyWithAdjustWithMove ((fmap . fmap) unUI f) d e)

instance DomBuilder Spider UI where
    type DomBuilderSpace UI = GhcjsDomSpace
    textNode t = UI (textNode t)
    commentNode c = UI (commentNode c)
    element t e u = UI (element t e (unUI u))
    inputElement e = UI (inputElement e)
    textAreaElement e = UI (textAreaElement e)
    selectElement e u = UI (selectElement e (unUI u))
    placeRawElement e = UI (placeRawElement e)
    wrapRawElement e cfg = UI (wrapRawElement e cfg)

instance PostBuild Spider UI where
    getPostBuild = UI getPostBuild

instance MonadHold Spider UI where
    hold x e = UI (hold x e)
    holdDyn x e = UI (holdDyn x e)
    holdIncremental p e = UI (holdIncremental p e)
    buildDynamic a e = UI (buildDynamic a e)
    headE e = UI (headE e)
    now = UI now


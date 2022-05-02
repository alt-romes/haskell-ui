{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
module UI.Class ( module Reflex.Dom, module UI.Class ) where

import Data.Text (Text)

import Reflex.Class (MonadHold(now))

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix, mfix)

import Reflex.Dom hiding (button, display, dynText, blank, now, text, list, simpleList)

newtype UI t a = UI { unUI :: forall m t'. (Reflex t' => t' ~ t, MonadWidget t m)  => m a }
    deriving (Functor)

instance Applicative (UI t) where
    pure x = UI $ pure x
    (UI f) <*> (UI a) = UI (f <*> a)

instance Monad (UI t) where
    (UI x) >>= f = UI (x >>= unUI . f)

instance MonadFix (UI t) where
    mfix f = UI $ mfix (unUI . f)

instance MonadIO (UI t) where
    liftIO x = UI $ liftIO x

instance MonadSample t (UI t) where
    sample x = UI (sample x)

instance NotReady t (UI t) where
    notReadyUntil :: Event t a -> UI t ()
    notReadyUntil e = UI (notReadyUntil e)

    notReady :: UI t ()
    notReady = UI notReady

instance Reflex t => Adjustable t (UI t) where
    runWithReplace u e = UI (runWithReplace (unUI u) (fmap unUI e))
    traverseIntMapWithKeyWithAdjust f i e = UI (traverseIntMapWithKeyWithAdjust (fmap unUI <$> f) i e)
    traverseDMapWithKeyWithAdjustWithMove f d e = UI (traverseDMapWithKeyWithAdjustWithMove ((fmap . fmap) unUI f) d e)

instance Reflex t => DomBuilder t (UI t) where
    type DomBuilderSpace (UI t) = GhcjsDomSpace

    textNode :: TextNodeConfig t -> UI t (TextNode GhcjsDomSpace t)
    textNode t = UI (textNode t)

    commentNode :: CommentNodeConfig t -> UI t (CommentNode GhcjsDomSpace t)
    commentNode c = UI (commentNode c)

    element :: Text -> ElementConfig er t GhcjsDomSpace -> UI t a -> UI t (Element er GhcjsDomSpace t, a)
    element t e u = UI (element t e (unUI u))

    inputElement :: InputElementConfig er t GhcjsDomSpace -> UI t (InputElement er GhcjsDomSpace t)
    inputElement e = UI (inputElement e)

    textAreaElement :: TextAreaElementConfig er t GhcjsDomSpace -> UI t (TextAreaElement er GhcjsDomSpace t)
    textAreaElement e = UI (textAreaElement e)

    selectElement :: SelectElementConfig er t GhcjsDomSpace -> UI t a -> UI t (SelectElement er GhcjsDomSpace t, a)
    selectElement e u = UI (selectElement e (unUI u))

    placeRawElement :: RawElement GhcjsDomSpace -> UI t ()
    placeRawElement e = UI (placeRawElement e)

    wrapRawElement :: RawElement GhcjsDomSpace -> RawElementConfig er t GhcjsDomSpace -> UI t (Element er GhcjsDomSpace t)
    wrapRawElement e cfg = UI (wrapRawElement e cfg)

instance Reflex t => PostBuild t (UI t) where
    getPostBuild = UI getPostBuild

instance MonadHold t (UI t) where
    hold :: a -> Event t a -> UI t (Behavior t a)
    hold x e = UI (hold x e)

    holdDyn :: a -> Event t a -> UI t (Dynamic t a)
    holdDyn x e = UI (holdDyn x e)

    holdIncremental :: Patch p => PatchTarget p -> Event t p -> UI t (Incremental t p)
    holdIncremental p e = UI (holdIncremental p e)

    buildDynamic :: PushM t a -> Event t a -> UI t (Dynamic t a)
    buildDynamic a e = UI (buildDynamic a e)

    headE :: Event t a -> UI t (Event t a)
    headE e = UI (headE e)

    now :: UI t (Event t ())
    now = UI now


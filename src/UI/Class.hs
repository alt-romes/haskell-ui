{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
module UI.Class ( module Reflex.Dom, module UI.Class ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix, mfix)

import Reflex.Dom hiding (button, display, dynText, blank, now, text, list, simpleList)

-- import Reflex.Dom.Builder.Class

-- type UI :: x -> * -> * -- TODO:
newtype UI t a = UI { unUI :: forall m. MonadWidget t m  => m a }
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

-- instance DomBuilder t (UI t) where -- TODO:
--     type DomBuilderSpace (UI t) = GhcjsDomSpace

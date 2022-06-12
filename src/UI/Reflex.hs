module UI.Reflex
    ( updated
    ) where

import qualified Reflex
import UI.Class

updated :: Dynamic a -> Event a
updated = Reflex.updated

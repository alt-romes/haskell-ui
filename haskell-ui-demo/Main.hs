{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Control.Monad.IO.Class
import Control.Monad (forM_)

import Cob
import Cob.RecordM.TH
import Cob.RecordM.UI
import Cob.UserM.UI

import UI.Theme
import UI.Class
import UI.Icons
import UI

newtype Todo = Todo Text
mkRecord ''Todo "ROMES Todos" ["Todo"]

data Artist = Artist { url :: Text, name :: Text } deriving Show
mkRecord ''Artist "ROMES Artists" ["Picture", "Name"]

main :: IO ()
main = mainUI (cobLogin "mimes8.cultofbits.com" ui)

instance Theme UI where
    primaryColor = Teal

ui :: Reflex t => CobSession -> UI t (Event t CobRoute)
ui session = do
    tabView "Browse" [("Listen Now", play), ("Browse", viewGrid), ("Radio", statusOnlineO), ("Library", collection), ("Search", search)] True

        \case

            "Listen Now" ->
                navigationTitle "Listen Now"

            "Browse" -> navigationTitle "Browse"

            "Radio" -> 
                navigationTitle "Radio"

            "Library" -> navigationView "Library" $ scrollView do

                navigationTitle  "Library"

                evs <- menuE [("Playlists", menuAlt2O),
                              ("Artists", microphoneO),
                              ("Albums", collectionO),
                              ("Made For You", inboxO),
                              ("Songs", noteO),
                              ("Downloaded", arrowCircleDownO)]
                             libraryMenuItem
                             
                heading "Recently Added"
                -- gridY 2 ()
                return evs


            "Search" -> 
                navigationTitle  "Search"

            _ -> error "unknown route"
    return never
    where

        libraryMenuItem :: Reflex t => (Text, Icon) -> UI t (UI t (), Maybe Text)
        libraryMenuItem (t, i) = paddingYContainer $ hstack do
            renderIcon' 6 (constDyn textPrimary) i
            p $ text t
            spacer
            renderIcon' 5 (constDyn textColor) chevronRightO
            return (librarySubviews t, Just t)

        librarySubviews :: Reflex t => Text -> UI t ()
        librarySubviews = \case
            "Artists" -> navigationView "Artists" $ scrollView do
                navigationTitle "Artists"
                artists <- rmDefinitionInstances ("*" :: String) session
                listE artists \a -> paddingYContainer $ hstack do
                    (sample . current) a >>= imageRS . url
                    p $ dynText (name <$> a)
                    (sample . current) a >>= return . (display a,) . Just . name
                    spacer
                return never

            _ -> error "undefined"


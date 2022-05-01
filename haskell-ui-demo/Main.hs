{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (pack)
import qualified Data.Text as T

import Control.Monad.IO.Class
import Control.Monad (forM_)

import Cob
import Cob.RecordM.TH
import Cob.RecordM.UI
import Cob.UserM.UI

import UI.Layout
import UI.Text
import UI.Theme
import UI.Class
import UI.Icons
import UI

data Album = Album { cover :: Text, album :: Text, artist :: Text } deriving Show
mkRecord ''Album "ROMES Albums" ["Cover", "Album", "Artist"]

data Artist = Artist { url :: Text, name :: Text } deriving Show
mkRecord ''Artist "ROMES Artists" ["Picture", "Name"]

main :: IO ()
main = mainUI (cobLogin "mimes8.cultofbits.com" ui)

instance Theme UI where
    primaryColor = Red
    grayScale = Neutral

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

                albums <- rmDefinitionInstances ("*" :: String) session
                evs2 <- gridYE 2 albums recentlyAddedItem

                return (leftmost [evs, evs2])


            "Search" -> 
                navigationTitle  "Search"

            _ -> error "unknown route"
    return never
    where
        recentlyAddedItem :: Reflex t => Dynamic t Album -> UI t (UI t (), Maybe Text)
        recentlyAddedItem dalbum = vstack do
            imageD (cover <$> dalbum)
            dynText (album <$> dalbum)
            dynText (artist <$> dalbum)
            return (albumView dalbum, Nothing)

        albumView :: Reflex t => Dynamic t Album -> UI t ()
        albumView dalbum = label "Album" $ dynText (pack . show <$> dalbum)

        libraryMenuItem :: Reflex t => (Text, Icon) -> UI t (UI t (), Maybe Text)
        libraryMenuItem (t, i) = paddingYContainer $ hstack do
            renderIcon' 6 (constDyn textPrimary) i
            p $ text t
            spacer
            renderIcon' 5 (constDyn textLight) chevronRightO
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


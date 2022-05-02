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

import UI.View
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

instance Theme UI where
    primaryColor = Fuchsia
    grayScale = Neutral


main :: IO ()
main = do
    -- session <- emptySession "mimes8.cultofbits.com"
    -- mainUI (ui session >> return ())
    mainUI (cobLogin "mimes8.cultofbits.com" ui)

ui :: CobSession -> UI (Event CobRoute)
ui session = do

    tabView "Browse" [("Listen Now", play), ("Browse", viewGrid), ("Radio", statusOnlineO), ("Library", collection), ("Search", search)] True

        \case

            "Listen Now" -> navigationTitle "Listen Now"

            "Browse"     -> navigationTitle "Browse"

            "Radio"      -> navigationTitle "Radio"

            "Library"    -> navigationView "Library" $ scrollView $ contentView do

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


            "Search" -> navigationTitle  "Search"

            _ -> error "unknown route"
    return never

    where
        recentlyAddedItem :: Dynamic Album -> UI (UI ())
        recentlyAddedItem dalbum =
            vstack do
                imageD (cover <$> dalbum)
                dynText (album <$> dalbum)
                dynText (artist <$> dalbum)
                return (albumView dalbum)

        albumView :: Dynamic Album -> UI ()
        albumView dalbum = label "Album" $ dynText (pack . show <$> dalbum)

        libraryMenuItem :: (Text, Icon) -> UI (UI ())
        libraryMenuItem (t, i) = paddingYContainer $
            hstack do

                renderIcon' 6 (constDyn textPrimary) i
                p (text t)

                spacer

                renderIcon' 5 (constDyn textLight) chevronRightO

                return (librarySubviews t)

        librarySubviews :: Text -> UI ()
        librarySubviews = \case
            "Artists" -> navigationView "Artists" $ scrollView $ contentView do
                navigationTitle "Artists"
                artists <- rmDefinitionInstances ("*" :: String) session
                listE artists \a -> paddingYContainer $ hstack do
                    (sample . current) a >>= imageRS . url
                    p $ dynText (name <$> a)
                    (sample . current) a >>= return . (display a,) . Just . name
                    spacer
                return never

            _ -> error "undefined"


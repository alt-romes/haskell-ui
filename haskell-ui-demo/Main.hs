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

import UI.Lazy
import UI.View
import UI.Layout
import UI.Text
import UI.Theme
import UI.Class
import UI.Icons as Icons
import UI

data Album = Album { cover :: Text, album :: Text, artist :: Text } deriving Show
mkRecord ''Album "ROMES Albums" ["Cover", "Album", "Artist"]

data Artist = Artist { url :: Text, name :: Text } deriving Show
mkRecord ''Artist "ROMES Artists" ["Picture", "Name"]

instance Theme UI where
    primaryColor = Red
    grayScale = Neutral

main :: IO ()
main = do
    -- session <- emptySession "mimes8.cultofbits.com"
    -- mainUI (ui session >> return ())
    mainUI (cobLogin "mimes8.cultofbits.com" ui)

ui :: CobSession -> UI (Event CobRoute)
ui session = do

    tabView "Browse" [("Listen Now", play), ("Browse", viewGrid), ("Radio", statusOnlineO), ("Library", collection), ("Search", Icons.search)] True

        \case

            "Listen Now" -> listenNow

            "Browse"     -> browse

            "Radio"      -> radio

            "Library"    -> library

            "Search"     -> searchV

            _ -> navigationTitle "Unknown Route"

    return never

    where

    listenNow :: UI ()
    listenNow = navigationView "Listen Now" $
        scrollView $ contentView do
            navigationTitle "Listen Now"
            menu [1..4] $ \case
                1 -> do
                    heading "Top Picks"
                    albums <- definitionInstances "*" session
                    gridXE 1 albums albumItem
                    headingM (MT S4) "Recently Played"
                2 -> heading "2000s"
                3 -> heading "New Releases"
                4 -> heading "1980s"

            return never


    browse :: UI ()
    browse = navigationView "Browse" $ scrollView $ contentView do
        navigationTitle "Browse"
        return never

    radio :: UI ()
    radio = navigationView "Radio" $ scrollView $ contentView do
        navigationTitle "Radio"
        return never

    library :: UI ()
    library = navigationView "Library" $

      scrollView $ contentView do

        navigationTitle  "Library"

        evs <- menuE
                 [("Playlists", menuAlt2O),
                  ("Artists", microphoneO),
                  ("Albums", collectionO),
                  ("Made For You", inboxO),
                  ("Songs", noteO),
                  ("Downloaded", arrowCircleDownO)] $
                      libraryMenuItem \case
                          "Artists" -> artistsPage
                          _ -> navigationTitle "Undefined"

        headingM (MT S4) "Recently Added"

        albums <- definitionInstances "*" session
        evs2 <- gridYE 2 albums albumItem

        return (leftmost [evs, evs2])

    searchV :: UI ()
    searchV = navigationView "Search" $ scrollView $ contentView do
        navigationTitle "Search"
        return never

    albumItem :: Dynamic Album -> UI (UI ())
    albumItem dalbum =
        vstack' ALeft S0 do
            imageD (cover <$> dalbum)
            para  $ dynText (album <$> dalbum)
            paraM $ dynText (artist <$> dalbum)
            return (albumView dalbum)

    albumView :: Dynamic Album -> UI ()
    albumView dalbum = label (text "Album") $ dynText (pack . show <$> dalbum)

    libraryMenuItem :: (Text -> UI ()) -> (Text, Icon) -> UI (UI ())
    libraryMenuItem mkPage (t, i) =
        hstack do
            renderIcon' 6 (constDyn textPrimary) i
            p (text t)
            spacer
            renderIcon' 5 (constDyn textLight) chevronRightO
            return (mkPage t)

    artistsPage :: UI ()
    artistsPage = navigationView "Artists" $ scrollView $ contentView do
        navigationTitle "Artists"
        artists <- definitionInstances "*" session
        listE artists \a -> hstack do
            dynImgRoundedSm (url <$> a)
            p $ dynText (name <$> a)
            spacer
        return never


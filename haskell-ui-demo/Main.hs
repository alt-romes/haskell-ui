{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Control.Monad.IO.Class

import Cob
import Cob.RecordM.TH
import Cob.RecordM.UI
import Cob.UserM.UI

import UI.Extended
import UI.Icons
import UI

newtype Todo = Todo Text
mkRecord ''Todo "ROMES Todos" ["Todo"]


data Movie = Movie Text Text Text Text
mkRecord ''Movie "ROMES Watched Movies" ["Movie", "Year", "Director", "Rating"]

mainContent :: Reflex t => CobSession -> UI t ()
mainContent session = do
    contentView $ do

        hstack $ do

            -- Watched Movies
            vstack $ mdo

                v1 <- inputLC "Movie" click
                v2 <- inputLC "Director" click
                v3 <- inputLC "Year" click
                v4 <- inputLC "Rating" click

                click <- button "Add Movie"

                dynIf ((==) <$> v1 <*> v2)
                    (display (v1 <> "---" <> v2))
                    blank

                rmAddInstances (click <~~ Movie <$> v1 <*> v2 <*> v3 <*> v4) session

            -- Todos
            vstack $ mdo

                val <- inputLC "Todo" click
                click <- button "Add Todo"
                rmAddInstances (click <~~ Todo <$> val) session

    return ()

searchPage = vstack $ do
    input
    input
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"
    button "Search"

main :: IO ()
main = mainUI $ do

    tabView "Browse" [("Listen Now", play), ("Browse", viewGrid), ("Radio", statusOnlineO), ("Library", collection), ("Search", search)] True

        $ \case

            "Listen Now" ->
                navigationView "Listen Now" $ scrollView $ contentView $ do
                    navigationTitle "Listen Now"
                    vstack $ do
                        val <- inputL "Name the song you want to hear"
                        click <- button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        button "Search"
                        return (click <~~ (scrollView $ contentView searchPage,) . Just <$> val)

            "Browse" -> contentView $
                navigationTitle  "Browse"

            "Radio" -> contentView $
                navigationTitle  "Radio"

            "Library" -> contentView $
                navigationTitle  "Library"

            "Search" -> contentView $
                navigationTitle  "Search"

            _ -> error "unknown route"

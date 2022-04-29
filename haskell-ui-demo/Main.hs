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

import UI.Extended
import UI.Icons
import UI

newtype Todo = Todo Text
mkRecord ''Todo "ROMES Todos" ["Todo"]


data Movie = Movie Text Text Text Text
mkRecord ''Movie "ROMES Watched Movies" ["Movie", "Year", "Director", "Rating"]

mainContent :: Reflex t => CobSession -> UI t ()
mainContent session = do
    -- contentView $ do

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

cardView :: Reflex t => Dynamic t Int -> Dynamic t Text -> UI t ()
cardView x y = paddingContainer $ vstack' 1 $ do
    hstack $ do
        text "Design"
        spacer
    spacer
    hstack $ do
        labelI' userGroupO (T.pack . show <$> x)
        spacer
        labelI' clockO y


main :: IO ()
main = mainUI $ do

    tabView "Browse" [("Listen Now", play), ("Browse", viewGrid), ("Radio", statusOnlineO), ("Library", collection), ("Search", search)] True

        $ \case

            "Listen Now" ->
                navigationView "Listen Now" $ scrollView $ do
                    navigationTitle "Listen Now"
                    hstack $ do
                        button "hii"
                        spacer
                        button "ho"
                    ev <- vstack $ do
                        val <- inputL "Name the song you want to hear"
                        spacer
                        click <- button "Search"
                        return (click <~~ (scrollView $ searchPage,) . Just <$> val)
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    vstack $ do
                        val <- inputL "Name the song you want to hear"
                        spacer
                        click <- button "Search"
                        return (click <~~ (scrollView $ searchPage,) . Just <$> val)
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    navigationTitle "Listen Now"
                    return ev

            "Browse" -> scrollView do
                navigationTitle  "Browse"
                let l = constDyn $ [(14, "Ok"),(15, "Ok"),(16, "Not Ok")]
                list l $ \i -> do
                    cardView (fst <$> i) (snd <$> i)
                vstack do
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                    button "rch"
                return ()

            "Radio" -> 
                navigationTitle  "Radio"

            "Library" -> 
                navigationTitle  "Library"

            "Search" -> 
                navigationTitle  "Search"

            _ -> error "unknown route"

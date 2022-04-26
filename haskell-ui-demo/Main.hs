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

loginForm :: UI t (Event t ())
loginForm = button "Hi"

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

main :: IO ()
main = mainUI $ do
    tabView "Home" [("Home", homeO), ("Archive", archiveO), ("Cart", cartO)] True $ \case

        "Home" -> text "Welcome home"

        "Archive" -> text "Accounting place"

        "Cart" -> text "Buying place"


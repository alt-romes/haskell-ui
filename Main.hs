{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Cob
import Cob.RecordM.TH
import Cob.RecordM.Reflex

import UI

newtype Todo = Todo { todoText :: Text }
mkRecord ''Todo "ROMES Todos" ["Todo"]

data Movie = Movie Text Text Text Text
mkRecord ''Movie "ROMES Watched Movies" ["Movie", "Year", "Director", "Rating"]

main = do
    cobToken <- init <$> readFile "cob-token.secret"
    session  <- makeSession "mimes8.cultofbits.com" cobToken
    mainUI $ do

        contentView $ do

            hstack $ do

                addMovieEvt <- form ["Movie Name", "Movie Year", "Director", "Rating"] "Add Movie"
                rmAddInstances ((\[name, year, director, rating] -> Movie name year director rating) <$> addMovieEvt) session

                divClass "" $ do
                    addTodoEvt <- form ["Todo"] "Add Todo"
                    rmAddInstances ((\[todo] -> Todo todo) <$> addTodoEvt) session

                    addTodoEvt <- form ["Kardashian Name", "Kardhaian Son", "KArdahsing Rapper Marry"] "Olá"
                    return ()

        return ()


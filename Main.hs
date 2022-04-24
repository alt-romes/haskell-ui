{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Cob
import Cob.RecordM.TH
import Cob.RecordM.Reflex

import UI.Extended
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
            vstack $ mdo
                v1 <- inputLC "Title" btn
                v2 <- inputLC "Yaer" btn
                v3 <- inputLC "Director" btn
                v4 <- inputLC "Rating" btn
                btn <- button "Btn"
                UI $ rmAddInstances (btn <~~ Movie <$> v1 <*> v2 <*> v3 <*> v4) session


        contentView $ do

            hstack $ do

                addMovieEvt <- form ["Movie Name", "Movie Year", "Director", "Rating"] "Add Movie"
                UI $ rmAddInstances ((\[name, year, director, rating] -> Movie name year director rating) <$> addMovieEvt) session

                vstack $ do

                    addTodoEvt <- form ["Todo"] "Add Todo"
                    UI $ rmAddInstances ((\[todo] -> Todo todo) <$> addTodoEvt) session

                    _ <- button_ ["bg-red-500"] "Big top"

                    return ()

        return ()


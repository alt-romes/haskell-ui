{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import Cob
import Cob.RecordM.TH
import Cob.RecordM.UI

import UI.Extended
import UI

newtype Todo = Todo Text
mkRecord ''Todo "ROMES Todos" ["Todo"]

data Movie = Movie Text Text Text Text
mkRecord ''Movie "ROMES Watched Movies" ["Movie", "Year", "Director", "Rating"]

main :: IO ()
main = do
    cobToken <- init <$> readFile "cob-token.secret"
    session  <- makeSession "mimes8.cultofbits.com" cobToken

    mainUI $ do

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


                    -- dynUI ((\x ->
                    --     if x == "Pai"
                    --        then display v4
                    --        else return ()) <$> v2)

                    rmAddInstances (click <~~ Movie <$> v1 <*> v2 <*> v3 <*> v4) session

                -- Todos
                vstack $ mdo
                    val <- inputLC "Todo" click
                    click <- button "Add Todo"
                    rmAddInstances (click <~~ Todo <$> val) session

        return ()


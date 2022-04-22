{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
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

data Movie = Movie Text Text
mkRecord ''Movie "ROMES Watched Movies" ["Name", "Year"]

todoform :: MonadWidget t m => m (Event t Todo)
todoform = do
    rec
        todoIn <- input' ("" <$ btnEvt)
        btnEvt <- button "Add Todo"
    return (btnEvt `taggedWith` (Todo <$> value todoIn))

movieform :: MonadWidget t m => m (Event t Movie)
movieform = do
    formres <- form ["Movie Name", "Movie Year"] "Add Movie"
    return (list2Movie <$> formres)
    where
        -- TODO: Use "dependently typed" sized lists to guarantee correctness?
        list2Movie (Fields (name:.(year:.E))) = Movie name year

root :: MonadWidget t m => CobSession -> m ()
root session = container $ do

    addMovieEvt <- movieform
    rmAddInstances addMovieEvt session

    return ()





main = do
    cobToken <- init <$> readFile "cob-token.secret"
    session  <- makeSession "mimes8.cultofbits.com" cobToken
    mainUI (root session)

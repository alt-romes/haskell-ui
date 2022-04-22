{-# LANGUAGE DataKinds #-}
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

test :: ExplicitSizedList ('S ('S 'Z)) Text
test = "name":.("age":.E)

decompose =
    case test of
      (n:.(age:.E)) -> Movie n age

movieform :: MonadWidget t m => m (Event t Movie)
movieform = do
    formres <- form (fromList ["Movie Name", "Movie Year"]) "Add Movie"
    return (list2Movie <$> formres)
    where
        list2Movie (name:.year:.E) = Movie name year

root :: MonadWidget t m => CobSession -> m ()
root session = contentView $ do

    addMovieEvt <- movieform
    rmAddInstances addMovieEvt session

    return ()

main = do
    cobToken <- init <$> readFile "cob-token.secret"
    session  <- makeSession "mimes8.cultofbits.com" cobToken
    mainUI (root session)

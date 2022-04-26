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

    cobRouter "mimes8.cultofbits.com" (\session -> do

        router "myroute" $ \case

            "myroute" -> contentView $ do
                text "My route..."
                click <- button "To main"
                return ("main" <$ click)

            "main" -> do
                mainContent session
                click <- button "To myroute"
                return ("myroute" <$ click)

        contentView $ hstack $ do
            toLogout <- button "To logout"
            toLogin  <- button "To login"
            return $ leftmost $
                [ CRLogout <$ toLogout
                , CRLogin  <$ toLogin
                ]
                                      )

    -- router ("/login", Nothing) $ \case

    --     ("/login", Nothing) -> do
    --         loginEv <- userLoginPage "mimes8.cultofbits.com"
    --         return ((\case
    --                     Nothing -> ("/login", Nothing)
    --                     Just s -> ("/main", Just s)) <$> loginEv
    --                )

    --     ("/login", Just session) -> do
    --         text "Already logged in... redirecting to main"
    --         (("/main", Just session) <$) <$> (after 0.5)

    --     ("/main", Nothing) -> (("/login", Nothing) <$) <$> verySoon

    --     ("/main", Just session) -> do
    --         mainContent session
    --         x <- button "Go To Unknown"
    --         y <- button "Go To Login"
    --         return $ leftmost [
    --             (("//", Just session) <$ x),
    --             (("/login", Just session) <$ y)
    --                           ]

    --     (_, session) -> do
    --         x <- button "Unknown"
    --         return (("/login", session) <$ x)

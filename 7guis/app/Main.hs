{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
module Main where

import Reflex
import UI.Theme
import UI.Text
import UI.Layout
import UI.Input
import UI

import Text.Read (readMaybe)
import Data.Witherable (mapMaybe)

instance Theme UI

main :: IO ()
main = mainUI $ contentView do

    menu [1..2] $ \case
        1 -> counter
        2 -> temperature

    return ()

-- ui :: UI ()
-- ui = vstack $
--     p (text "


counter :: UI ()
counter= mdo
    click <- label (text "Clicks: " >> display clicksN) (button "Click me")
    clicksN <- count click
    return ()


temperature :: UI ()
temperature = vstack $ mdo
    celcius <- mapMaybe readMaybe' <$> inputE "Celcius" (toCelcius <$> fahrenheit)
    p (text "Celcius")
    spacer
    fahrenheit <- mapMaybe readMaybe' <$> inputE "Fahrenheit" (toFahrenheit <$> celcius)
    spacer
    p (text "Farenheit")


toCelcius x = pack . show $ (x - 32) * 5/9
toFahrenheit x = pack . show $ x * 9/5 + 32
readMaybe' = readMaybe @Float . unpack

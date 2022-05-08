{-# LANGUAGE OverloadedStrings #-}
module UI.Lazy where

import Control.Monad.IO.Class
import UI.Class
import UI

import Data.Map

litest :: UI ()
litest = do
    liftIO $ putStrLn "Hi"
    (a, _) <- virtualList (constDyn 200) 50 (constDyn 1) 0 never id (fromList [(0,"hii")]) never (\_ _ _ -> el "li" $ text "hii")
    sample (current a) >>= liftIO . print
    return ()

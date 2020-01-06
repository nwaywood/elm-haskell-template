{-# LANGUAGE OverloadedStrings #-}
module Core 
    ( run 
    ) where
    
import Web.Scotty

import Data.Monoid (mconcat)

run = scotty 3000 $
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

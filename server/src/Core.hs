{-# LANGUAGE OverloadedStrings #-}
module Core 
    ( run 
    ) where
    
import Web.Scotty
import Network.Wai.Middleware.Static (staticPolicy, addBase)

import Data.Monoid (mconcat)

run = scotty 3000 $ do 
    middleware $ staticPolicy $ addBase "../client/index.html"
    get "/api/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

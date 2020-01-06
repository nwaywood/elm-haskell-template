{-# LANGUAGE OverloadedStrings #-}
module Core 
    ( run 
    ) where
    
import Web.Scotty as S
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.HTTP.Req
import Data.Aeson.Parser
import Data.Monoid (mconcat)
import Control.Monad.IO.Class

run = scotty 3000 $ do 
    middleware $ staticPolicy $ addBase "../client/index.html"
    get "/api/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    get "/api/articles" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
        -- S.json getArticles
       

getArticles :: IO ()
getArticles = runReq defaultHttpConfig $ do
    bs <-   (req GET (https "hn.algolia.com" /: "api" /: "v1") NoReqBody jsonResponse mempty)
    liftIO $ putStrLn (responseBody bs)

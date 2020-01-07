{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Core 
    ( run 
    ) where
    
import Web.Scotty as S
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.HTTP.Req
import Data.Aeson.Parser
import Data.Aeson
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import GHC.Generics 
import Debug.Trace (trace)
import Data.Text


run = scotty 3000 $ do 
    middleware $ staticPolicy $ addBase "../client/index.html"
    --get "/api/:word" $ do
    --    beam <- param "word"
    --    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    get "/api/articles" $ do
        res <- getArticles
        S.json res

       

--getArticles :: HttpResponseBody (JsonResponse Articles)
getArticles = runReq defaultHttpConfig $ do
    bs <- (req GET (https "hn.algolia.com" /: "api" /: "v1" /: "search_by_date") NoReqBody jsonResponse ("tags" =: ("story" :: Text)))
    let testObj =  (  responseBody bs :: Articles)
    trace ("Test Object" ++ show testObj) (liftIO $ pure testObj)
    

thing  = "test"

data Articles = Articles {
        hits :: [Article]
} deriving (Generic, Show)


data Article = Article {
        title :: Text
        , author :: Text
} deriving (Generic, Show)




instance FromJSON Articles

instance FromJSON Article

instance ToJSON Article 

instance ToJSON Articles 

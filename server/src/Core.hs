{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Core 
    ( run 
    ) where
    
import Web.Scotty as S
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.HTTP.Req as Req
import Data.Aeson.Parser 
import Data.Aeson 
import Data.Aeson.Types
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import GHC.Generics 
import Debug.Trace (trace)
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Control.Applicative (empty)
import Network.HTTP.Types (status404)
import Data.String (fromString)


 
data Article = Article {
        title :: Text
        , author :: Text
} deriving (Generic, Show)




run = scotty 3000 $ do 

    middleware $ staticPolicy $ addBase "../client/static"
    
    get "/api/articles" $ do
        res <- getArticles 
        handleResponse res
    -- Matches any APi route that does not exist
    matchAny (regex "/api/.*") $ do
        path <- param "0"
        status status404
        html $ fromString $ "<h1>Can't find API path:  " ++ path ++ "</h1>" 

    -- Matches every unmatched get route to return frontend.
    get (regex ".*") $ do 
        file "../client/static/index.html"


getArticles :: ActionM (Result [Article])
getArticles = runReq defaultHttpConfig $ do
    bs <- (req GET (https "hn.algolia.com" /: "api" /: "v1" /: "search_by_date") NoReqBody jsonResponse ("tags" =: ("story" :: Text)))
    let result = parse articles (responseBody bs)
    liftIO $ pure result


handleResponse :: Result [Article] -> ActionM ()
handleResponse (Error str) = raise (pack str)
handleResponse (Success a) = S.json a



-- JSON Parsing 
articles :: Value -> Parser [Article]
articles = withObject "articles" $ \v -> v .: "hits" 

instance FromJSON Article where
     parseJSON (Object v) = Article <$>
                            v .: "title" <*>
                            v .: "author"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = empty 



instance ToJSON Article 


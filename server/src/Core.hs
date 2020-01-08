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

 


run = scotty 3000 $ do 
    middleware $ staticPolicy $ addBase "../client/index.html"
    get "/api/articles" $ do
        res <- getArticles
        handleResponse res


-- getArticles :: [Article]
getArticles = runReq defaultHttpConfig $ do
    bs <- (req GET (https "hn.algolia.com" /: "api" /: "v1" /: "search_by_date") NoReqBody jsonResponse ("tags" =: ("story" :: Text)))
    let result = parse articles (responseBody bs)
    liftIO $ pure result
    

handleResponse :: Result [Article] -> ActionM ()
handleResponse (Error str) = raise (pack str)
handleResponse (Success a) = S.json a


data Article = Article {
        title :: Text
        , author :: Text
} deriving (Generic, Show)


articles :: Value -> Parser [Article]
articles = withObject "articles" $ \v -> v .: "hits" 


instance FromJSON Article where
     parseJSON (Object v) = Article <$>
                            v .: "title" <*>
                            v .: "author"
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = empty 



instance ToJSON Article 


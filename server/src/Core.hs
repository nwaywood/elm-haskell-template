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
import Data.Aeson.Types
import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import GHC.Generics 
import Debug.Trace (trace)
import Data.Text (Text)
import Control.Applicative (empty)

 


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
    -- let testObj = (  responseBody bs :: Articles)
    let maybeObj = parseMaybe articles (responseBody bs) 
    let obj =  getMaybeObj maybeObj
    trace ("Test Object" ++ show obj) (liftIO $ pure obj)
    

getMaybeObj :: Maybe [Article] -> [Article] 
getMaybeObj (Just a ) = a 
getMaybeObj _ = [Article "test" "test" ] 


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


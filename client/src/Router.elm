module Router exposing (Route(..), fromUrl, routeParser)

import Url
import Url.Parser as Parser exposing (Parser, map, oneOf, s, top)



-- Define your routes and your parser


type Route
    = Home
    | About
    | NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home (s "home")
        , map Home top
        , map About (s "about")
        ]



-- Create a helper function that converts a url to a route using the parser


fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (Parser.parse routeParser url)

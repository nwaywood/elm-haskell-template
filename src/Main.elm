module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode exposing (Decoder, field, list, map2, string)
import Router exposing (Route(..), fromUrl, routeParser)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    , articles : ArticleStatus (List Article)
    }


type ArticleStatus a
    = Loading
    | Failure Http.Error
    | Success a


type alias Article =
    { title : String
    , author : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url Router.NotFound Loading, getArticles )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | RefreshList
    | GotArticles (Result Http.Error (List Article))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        RefreshList ->
            ( { model | articles = Loading }, getArticles )

        UrlChanged url ->
            ( { model | route = fromUrl url }, Cmd.none )

        GotArticles res ->
            case res of
                Ok articles ->
                    ( { model | articles = Success articles }, Cmd.none )

                Err e ->
                    ( { model | articles = Failure e }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Router.Home ->
            viewHomePage model

        Router.About ->
            viewAboutPage model

        Router.NotFound ->
            viewNotFound model


type alias Page msg =
    { title : String
    , body : List (Html msg)
    }


buildPage : String -> Element msg -> Page msg
buildPage title body =
    { title = title
    , body = List.singleton <| layout [] body
    }


header : Element msg
header =
    column []
        [ text "Header"
        , viewLink "/home"
        , viewLink "/about"
        ]


viewHomePage : Model -> Page Msg
viewHomePage model =
    buildPage "Home Page"
        (column []
            [ header
            , Input.button [] { onPress = Just RefreshList, label = el [] (text "Refresh") }
            , case model.articles of
                Success a ->
                    showArticles a

                Failure e ->
                    text <| "Failed to load articles\n" ++ parseHttpError e

                Loading ->
                    text "Loading"
            ]
        )


parseHttpError : Http.Error -> String
parseHttpError e =
    case e of
        Http.BadUrl s ->
            "BadUrl: " ++ s

        Http.Timeout ->
            "Timeout error"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus i ->
            "Bad Status: " ++ String.fromInt i

        Http.BadBody b ->
            "Bad body: " ++ b


showArticles : List Article -> Element msg
showArticles articles =
    column [] (List.map showArticle articles)


showArticle : Article -> Element msg
showArticle article =
    column [] [ text article.title, text article.author ]


viewAboutPage : Model -> Page msg
viewAboutPage model =
    buildPage "About"
        (column []
            [ header
            , text "About page"
            ]
        )


viewNotFound : Model -> Page msg
viewNotFound model =
    buildPage "Not Found"
        (column []
            [ header
            , text "Not found"
            ]
        )


viewLink : String -> Element msg
viewLink path =
    link []
        { url = path
        , label = text path
        }



-- HTTP


getArticles : Cmd Msg
getArticles =
    Http.get
        { url = "https://hn.algolia.com/api/v1/search_by_date?tags=story&hitsPerPage=50"
        , expect = Http.expectJson GotArticles articlesDecoder
        }


articlesDecoder : Decoder (List Article)
articlesDecoder =
    field "hits" (Json.Decode.list articleDecoder)


articleDecoder : Decoder Article
articleDecoder =
    map2 Article
        (field "title" string)
        (field "author" string)

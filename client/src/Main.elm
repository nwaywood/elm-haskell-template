module Main exposing (Article, ArticleStatus(..), Model, Msg(..), Page, articleDecoder, articlesDecoder, buildPage, edges, getArticles, header, init, main, parseHttpError, showArticle, showArticles, subscriptions, update, view, viewAboutPage, viewHomePage, viewLink, viewNotFound)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
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
    ( Model key url (Router.fromUrl url) Loading, getArticles )



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
        [ el [ paddingEach { edges | bottom = 12 }, Font.size 28 ] <| text "Header"
        , viewLink "/home"
        , viewLink "/about"
        ]


viewHomePage : Model -> Page Msg
viewHomePage model =
    buildPage "Home Page"
        (column [ spacing 12 ]
            [ header
            , Input.button [ Border.rounded 3, Border.width 1, Border.color <| rgb255 200 200 200, padding 12 ] { onPress = Just RefreshList, label = el [] (text "Refresh") }
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
    column [ spacing 6 ] (List.map showArticle articles)


showArticle : Article -> Element msg
showArticle article =
    column [ Border.widthEach { edges | bottom = 1 }, paddingEach { edges | bottom = 6 } ] [ text article.title, text article.author ]


viewAboutPage : Model -> Page msg
viewAboutPage model =
    buildPage "About"
        (column [ spacing 12 ]
            [ header
            , text "About page"
            , image [] { src = "/static/elm-logo.png", description = "Elm-Logo-Test" }
            ]
        )


viewNotFound : Model -> Page msg
viewNotFound model =
    buildPage "Not Found"
        (column [ spacing 12 ]
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


edges =
    { top = 0
    , right = 0
    , left = 0
    , bottom = 0
    }



-- HTTP


getArticles : Cmd Msg
getArticles =
    Http.get
        { url = "/api/articles"
        , expect = Http.expectJson GotArticles articlesDecoder
        }


articlesDecoder : Decoder (List Article)
articlesDecoder =
    Json.Decode.list articleDecoder


articleDecoder : Decoder Article
articleDecoder =
    map2 Article
        (field "title" string)
        (field "author" string)

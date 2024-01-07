module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Builder
import Http
import Html exposing (..)
import Html.Attributes exposing (..)

import Route exposing (Route)

main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }


-- MODEL

type alias Model =
    { key : Nav.Key
    , page : Page
    }
    
type Page
    = NotFound
    | ErrorPage Http.Error
    | TopPage
    | UserPage (List Repo)
    | RepoPage (List Issue)

type alias Repo =
    { name : String
    }

type alias Issue =
    { title : String
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key TopPage
        |> goTo (Route.fromUrl url)

-- UPDATE

type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Loaded (Result Http.Error Page)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    (model, Nav.pushUrl model.key (Url.toString url))
                Browser.External href ->
                    (model, Nav.load href)
        UrlChanged url ->
            goTo (Route.fromUrl url) model
        Loaded result ->
            ({ model
                | page =
                    case result of
                        Ok page ->
                            page
                        Err e ->
                            ErrorPage e
            }
            , Cmd.none
            )

goTo : Maybe Route -> Model -> (Model, Cmd Msg)
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ({ model | page = NotFound }, Cmd.none)
        Just Route.Top ->
            ({ model | page = TopPage }, Cmd.none)
        Just (Route.User userName) ->
            ({ model | page = UserPage [ (Repo "repo1") ]}, Cmd.none)
        Just (Route.Repo userName repoName) ->
            ({ model | page = RepoPage [ (Issue "issue1") ]}, Cmd.none)
        

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "GitHub viewer"
    , body =
        [ a [ href "/" ] [ h1 [] [ text "GitHub viewer" ] ]
        , case model.page of
            NotFound ->
                text "not found"
            ErrorPage error ->
                text "something is wrong"
            TopPage ->
                viewTopPage
            UserPage repos ->
                text "user page"
            RepoPage issues ->
                text "repo page"
        ]
    }

viewTopPage : Html Msg
viewTopPage =
    ul []
        [ viewLink (Url.Builder.absolute [ "nsbt" ] [])
        , viewLink (Url.Builder.absolute [ "evancz" ] [])
        , viewLink (Url.Builder.absolute [ "elm" ] [])
        ]

viewLink : String -> Html Msg
viewLink path =
    li [] [ a [ href path ] [ text path ]]

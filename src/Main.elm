module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Builder
import Http
import Html exposing (..)
import Html.Attributes exposing (..)

import Route exposing (Route)
import Page.User
import Page.Repo
import GitHub

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
    | UserPage Page.User.Model
    | RepoPage Page.Repo.Model

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Model key TopPage
        |> goTo (Route.fromUrl url)

-- UPDATE

type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | Loaded (Result Http.Error Page)
    | UserMsg Page.User.Msg
    | RepoMsg Page.Repo.Msg

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
        UserMsg userMsg ->
            case model.page of
                UserPage userModel ->
                    let
                        (newUserModel, topCmd) =
                            Page.User.update userMsg userModel
                    in
                        ( { model | page = UserPage newUserModel }
                        , Cmd.map UserMsg topCmd
                        )
                _ ->
                    ( model, Cmd.none )
        RepoMsg repoMsg ->
            case model.page of
                RepoPage repoModel ->
                    let
                        (newRepoModel, topCmd) =
                            Page.Repo.update repoMsg repoModel
                    in
                        ( { model | page = RepoPage newRepoModel }
                        , Cmd.map RepoMsg topCmd
                        )
                _ ->
                    ( model, Cmd.none )

goTo : Maybe Route -> Model -> (Model, Cmd Msg)
goTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ({ model | page = NotFound }, Cmd.none)
        Just Route.Top ->
            ({ model | page = TopPage }, Cmd.none)
        Just (Route.User userName) ->
            let
                ( userModel, userCmd ) =
                    Page.User.init userName
            in
                ( { model | page = UserPage userModel }
                , Cmd.map UserMsg userCmd
                )
        Just (Route.Repo userName repoName) ->
            let
                ( repoModel, repoCmd ) =
                    Page.Repo.init userName repoName
            in
                ( { model | page = RepoPage repoModel }
                , Cmd.map RepoMsg repoCmd
                )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "GitHub Viewer"
    , body =
        [ a [ href "/" ] [ h1 [] [ text "GitHub Viewer" ] ]
        , case model.page of
            NotFound ->
                text "not found"
            ErrorPage error ->
                case error of
                    Http.BadBody message ->
                        pre [] [ text message ]
                    _ ->
                        text (Debug.toString error)
            TopPage ->
                ulUsers [ "nsbt", "qiskit", "evancz", "elm" ]
            UserPage userPageModel ->
                Page.User.view userPageModel
                    |> Html.map UserMsg
            RepoPage repoPageModel ->
                Page.Repo.view repoPageModel
                    |> Html.map RepoMsg
        ]
    }

ulUsers : List GitHub.User -> Html Msg
ulUsers users =
    ul [] (List.map liUser users)

liUser : GitHub.User -> Html Msg
liUser user =
    let
        path : String
        path = Url.Builder.absolute [ user ] []
    in
        li [] [ a [ href path ] [ text path ]]

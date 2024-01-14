module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Url
import Url.Builder
import Http
import Html exposing (..)
import Html.Attributes exposing (..)

import Route exposing (Route)
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
    | UserPage (List GitHub.Repo)
    | RepoPage (List GitHub.Issue)

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
            ( model
            , GitHub.getRepos
                (Result.map UserPage >> Loaded)
                userName
            )
        Just (Route.Repo userName repoName) ->
            ( model
            , GitHub.getIssues
                (Result.map RepoPage >> Loaded)
                userName
                repoName
            )


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
                case error of
                    Http.BadBody message ->
                        pre [] [ text message ]
                    _ ->
                        text (Debug.toString error)
            TopPage ->
                ulUsers [ "nsbt", "qiskit", "evancz", "elm" ]
            UserPage repos ->
                ulRepos repos
            RepoPage issues ->
                ulIssues issues
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

ulRepos : List GitHub.Repo -> Html Msg
ulRepos repos =
    ul [] (List.map liRepo repos)

liRepo : GitHub.Repo -> Html msg
liRepo repo =
    let
        path : String
        path = Url.Builder.absolute [ repo.owner, repo.name ] []
    in
        li [] [ a [ href path ] [ text path ] ]

ulIssues : List GitHub.Issue -> Html Msg
ulIssues issues =
    ul [] (List.map liIssue issues)

liIssue : GitHub.Issue -> Html msg
liIssue issue =
    li []
        [ span [] [ text ("[" ++ issue.state ++ "]") ]
        , span [] [ text ("#" ++ String.fromInt issue.number) ]
        , span [] [ text issue.title ]
        ]

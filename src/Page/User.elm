module Page.User exposing (Model, Msg, init, update, view)

import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Url.Builder

import GitHub


-- MODEL

type alias Model =
    { userName : String
    , state : State
    }

type State
    = Init
    | Loaded (List GitHub.Repo)
    | Error Http.Error

init : String -> (Model, Cmd Msg)
init userName =
    ( Model userName Init
    , GitHub.getRepos GotRepos userName
    )


-- UPDATE

type Msg
    = GotRepos (Result Http.Error (List GitHub.Repo))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotRepos (Ok repos) ->
            ({ model | state = Loaded repos }, Cmd.none)
        GotRepos (Err err) ->
            ({ model | state = Error err}, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    case model.state of
        Init ->
            text "Loading..."
        Loaded repos ->
            ulRepos repos
        Error error ->
            Debug.toString error |> text

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

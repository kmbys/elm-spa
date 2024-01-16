module Page.Repo exposing (Model, Msg, init, update, view)

import Http
import Html exposing (..)

import GitHub


-- MODEL

type alias Model =
    { userName : String
    , repoName : String
    , state : State
    }

type State
    = Init
    | Loaded (List GitHub.Issue)
    | Error Http.Error

init : String -> String -> (Model, Cmd Msg)
init userName repoName =
    ( Model userName repoName Init
    , GitHub.getIssues GotIssues userName repoName
    )


-- UPDATE

type Msg
    = GotIssues (Result Http.Error (List GitHub.Issue))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotIssues (Ok issues) ->
            ({ model | state = Loaded issues }, Cmd.none)
        GotIssues (Err err) ->
            ({ model | state = Error err }, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    case model.state of
        Init ->
            text "Loading..."
        Loaded issues ->
            ulIssues issues
        Error error ->
            Debug.toString error |> text

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

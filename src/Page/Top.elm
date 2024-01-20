module Page.Top exposing (Model, init, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Url.Builder

import GitHub


-- MODEL

type alias Model =
    { users : List GitHub.User
    }

init : Model
init =
    Model [ "nsbt", "qiskit", "evancz", "elm" ]


-- UPDATE

-- VIEW

view : Model -> Html msg
view model =
    ulUsers model.users

ulUsers : List GitHub.User -> Html msg
ulUsers users =
    ul [] (List.map liUser users)

liUser : GitHub.User -> Html msg
liUser user =
    let
        path : String
        path = Url.Builder.absolute [ user ] []
    in
        li [] [ a [ href path ] [ text path ]]

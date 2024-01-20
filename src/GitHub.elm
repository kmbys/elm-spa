module GitHub exposing (User, Repo, Issue, getRepos, getIssues, issueUrl)

import Url.Builder
import Http
import Json.Decode as D


-- TYPE

type alias User = String

type alias Repo =
    { name : String
    , description : Maybe String
    , language : Maybe String
    , owner : String
    , fork : Int
    , star : Int
    , watch : Int
    }

type alias Issue =
    { number : Int
    , title : String
    , state : String
    }


-- API

getRepos : (Result Http.Error (List Repo) -> msg) -> String -> Cmd msg
getRepos toMsg userName =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.github.com"
            [ "users", userName, "repos" ]
            []
        , expect =
            Http.expectJson
                toMsg
                reposDecoder
        }

getIssues : (Result Http.Error (List Issue) -> msg) -> String -> String -> Cmd msg
getIssues toMsg userName repoName =
    Http.get
        { url =
            Url.Builder.crossOrigin "https://api.github.com"
                [ "repos", userName, repoName, "issues" ]
                []
        , expect =
            Http.expectJson
                toMsg
                issuesDecoder
        }


-- URL

issueUrl : String -> String -> Int -> String
issueUrl userName repoName issueNumber =
    Url.Builder.crossOrigin
        "https://github.com"
        [ userName
        , repoName
        , "issues"
        , String.fromInt issueNumber
        ]
        []


-- DECODER

reposDecoder : D.Decoder (List Repo)
reposDecoder =
    D.list
        (D.map7 Repo
            (D.field "name" D.string)
            (D.maybe (D.field "description" D.string))
            (D.maybe (D.field "language" D.string))
            (D.at [ "owner", "login" ] D.string)
            (D.field "forks_count" D.int)
            (D.field "stargazers_count" D.int)
            (D.field "watchers_count" D.int)
        )

issuesDecoder : D.Decoder (List Issue)
issuesDecoder =
    D.list
        (D.map3 Issue
            (D.field "number" D.int)
            (D.field "title" D.string)
            (D.field "state" D.string)
        )

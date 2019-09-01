module Api.Endpoint exposing (Endpoint, request, habit, habits)

import Http
import Habit.Id as HabitId exposing (HabitId)
import Url.Builder exposing (QueryParameter)

request :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    , tracker : Maybe String
    }
    -> Cmd msg
request config =
    Http.request
        { method = config.method
        , headers = config.headers
        , url = unwrap config.url
        , body = config.body
        , expect = config.expect
        , timeout = config.timeout
        , tracker = config.tracker
        }


type Endpoint
    = Endpoint String

unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str

url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin "http://localhost:4200"
        paths
        queryParams
        |> Endpoint

habit : HabitId -> Endpoint
habit habitId =
    url [ "habits", HabitId.toString habitId ] []

habits : Endpoint
habits =
    url [ "habits" ] []
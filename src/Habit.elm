module Habit exposing (Habit)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Time
import Iso8601

type Habit = Habit Internals

type alias Internals =
    { name : String
    , description : String
    , timeLimit : Int
    , recurrences : List Int
    , nextDue : Time.Posix
    , doneCount : Int
    , streakCurrent : Int
    , streakMax : Int
    }

decoder : Decoder Habit
decoder =
    Decode.succeed Habit
        |> custom internalsDecoder

internalsDecoder : Decoder Internals
internalsDecoder =
    Decode.succeed Internals
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "timeLimit" Decode.int
        |> required "recurrences" (Decode.list Decode.int)
        |> required "nextDue" Iso8601.decoder
        |> required "doneCount" Decode.int
        |> required "streakCurrent" Decode.int
        |> required "streakMax" Decode.int
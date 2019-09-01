module Habit exposing (Habit, decoder)

import Habit.Id as HabitId exposing (HabitId)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Time
import Iso8601

type Habit = Habit Internals

type alias Internals =
    { id : HabitId
    , name : String
    , description : String
    , start : Time.Posix
    , timeLimit : Int
    , recurrences : List Int
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
        |> required "id" HabitId.decoder
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> required "start" Iso8601.decoder
        |> required "time_limit" Decode.int
        |> required "recurrences" (Decode.list Decode.int)
        |> required "done_count" Decode.int
        |> required "streak_current" Decode.int
        |> required "streak_max" Decode.int
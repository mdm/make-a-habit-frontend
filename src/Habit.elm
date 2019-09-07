module Habit exposing (Habit, id, name, description, start, timeLimit, recurrences, doneCount, streakCurrent, streakMax, decoder, list)

import Habit.Id as HabitId exposing (HabitId)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Http
import Time
import Iso8601
import Api
import Api.Endpoint as Endpoint

type Habit = Habit Internals

type alias Internals =
    { id : HabitId
    , name : String
    , description : Maybe String
    , start : Time.Posix
    , timeLimit : Int
    , recurrences : List Int
    , doneCount : Int
    , streakCurrent : Int
    , streakMax : Int
    }

id : Habit -> HabitId
id (Habit habit) =
    habit.id

name : Habit -> String
name (Habit habit) =
    habit.name

description : Habit -> Maybe String
description (Habit habit) =
    habit.description

start : Habit -> Time.Posix
start (Habit habit) =
    habit.start

timeLimit : Habit -> Int
timeLimit (Habit habit) =
    habit.timeLimit

recurrences : Habit -> List Int
recurrences (Habit habit) =
    habit.recurrences

doneCount : Habit -> Int
doneCount (Habit habit) =
    habit.doneCount

streakCurrent : Habit -> Int
streakCurrent (Habit habit) =
    habit.streakCurrent

streakMax : Habit -> Int
streakMax (Habit habit) =
    habit.streakMax

list : (Result Http.Error (List Habit) -> msg) -> Cmd msg
list toMsg =
    Decode.list decoder
        |> Api.get toMsg Endpoint.habits

decoder : Decoder Habit
decoder =
    Decode.succeed Habit
        |> custom internalsDecoder

internalsDecoder : Decoder Internals
internalsDecoder =
    Decode.succeed Internals
        |> required "id" HabitId.decoder
        |> required "name" Decode.string
        |> required "description" (Decode.nullable Decode.string)
        |> required "start" Iso8601.decoder
        |> required "time_limit" Decode.int
        |> required "recurrences" (Decode.list Decode.int)
        |> required "done_count" Decode.int
        |> required "streak_current" Decode.int
        |> required "streak_max" Decode.int
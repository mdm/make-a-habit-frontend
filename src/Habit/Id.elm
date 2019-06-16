module Habit.Id exposing (HabitId, decoder, toString, urlParser)

import Json.Decode as Decode exposing (Decoder)
import Url.Parser exposing (Parser)


type HabitId
    = HabitId Int


urlParser : Parser (HabitId -> a) a
urlParser =
    Url.Parser.map HabitId Url.Parser.int


decoder : Decoder HabitId
decoder =
    Decode.map HabitId Decode.int


toString : HabitId -> String
toString (HabitId int) =
    String.fromInt int

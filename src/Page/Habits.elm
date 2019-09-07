module Page.Habits exposing (..)

import Html exposing (Html, a, button, div, h2, h3, main_, text)
import Html.Attributes exposing (class, id, tabindex, type_)
import Habit exposing (Habit)
import Habit.Id exposing (HabitId)
import Route
import Session exposing (Session)
import Http
import Dict

type alias Model =
    { session : Session
    , habits: Status (List Habit)
    }

type Status a
    = Loading
    | Loaded a
    | Failed

init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , habits = Loading
      }
    , Habit.list CompletedHabitsLoad
    )

view : Model -> { title : String, content : Html msg }
view model =
    { title = "Habits"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ div [ class "Box" ] <|
                List.concat 
                    [ [ div [ class "Box-header" ]
                        [ h3 [ class "Box-title" ] [ text (viewHabitCount model ++ " Habits") ] ] ]
                    , viewHabits model
                    , [ div [ class "Box-footer text-right" ]
                        [ a [ class "btn btn-primary", Route.href Route.NewHabit] [ text "Add" ] ] ]
                    ]
            ]
    }

viewHabitCount : Model -> String
viewHabitCount model =
    case model.habits of
        Loaded habits ->
            List.length habits |> String.fromInt

        _ -> "0"

viewHabits : Model -> List (Html msg)
viewHabits model =
    case model.habits of
        Loading ->
            []

        Loaded habits ->
            List.map viewHabit habits

        Failed ->
            []

viewHabit : Habit -> Html msg
viewHabit habit =
    div [ class "Box-body d-flex flex-items-center" ]
        [ div [ class "flex-auto" ]
            [ h2 [] [ text <| Habit.name habit ]
            , text <| Maybe.withDefault "" (Habit.description habit)
            , div [ class "text-small text-gray-light" ] [ text <| "Repeats weekly on " ++ viewRecurrences habit ++ "." ]
            , div [ class "text-small text-gray-light" ] [ text <| "Must be completed within " ++ viewTimeLimit habit ++ "." ]
            ]
        , button [ type_ "button", class "btn mr-1" ] [ text "Edit" ]
        , button [ type_ "button", class "btn btn-danger" ] [ text "Delete" ]
        ]

viewRecurrences : Habit -> String
viewRecurrences habit =
    let
        days = [ "Monday", "Tueday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ]
            |> List.indexedMap Tuple.pair
            |> Dict.fromList
        exists maybe = 
            case maybe of
                Just _ ->
                    True
                Nothing ->
                    False
        recurrences = Habit.recurrences habit
            |> List.sort
            |> List.map (\recurrence -> Dict.get recurrence days)
            |> List.filter exists
            |> List.map (Maybe.withDefault "")
        numRecurrences = List.length recurrences
    in
    if numRecurrences == 1 then
        Maybe.withDefault "" <| List.head recurrences
    else
        String.join ", " (List.take (numRecurrences - 2) recurrences) ++ String.join " and " (List.drop (numRecurrences - 2) recurrences)
    
viewTimeLimit : Habit -> String
viewTimeLimit habit =
    let
        timeLimit = Habit.timeLimit habit
    in
    if timeLimit == 1 then
        "1 day"
    else
        String.fromInt timeLimit ++ " days"


type Msg
    = ClickedDelete HabitId
    | CompletedHabitsLoad (Result Http.Error (List Habit))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDelete _ ->
            ( model, Cmd.none )

        CompletedHabitsLoad (Ok habits) ->
            ( { model | habits = Loaded habits }, Cmd.none )

        CompletedHabitsLoad (Err error) ->
            ( { model | habits = Failed }, Cmd.none )

toSession : Model -> Session
toSession model =
    model.session

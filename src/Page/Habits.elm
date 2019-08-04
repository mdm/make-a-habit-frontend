module Page.Habits exposing (..)

import Html exposing (Html, a, div, h3, main_, text)
import Html.Attributes exposing (class, id, tabindex)
import Habit exposing (Habit)
import Habit.Id exposing (HabitId)
import Route
import Session exposing (Session)

type alias Model =
    { session : Session
    , habits: List Habit
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , habits = []
      }
    , Cmd.none
    )

view : Model -> { title : String, content : Html msg }
view model =
    { title = "Habits"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ div [ class "Box" ]
                [ div [ class "Box-header" ]
                    [ h3 [ class "Box-title" ] [ text (viewHabitCount model ++ " Habits") ] ]
                , div [ class "Box-footer text-right" ]
                    [ a [ class "btn btn-primary", Route.href Route.NewHabit] [ text "Add" ] ]
                ]
            ]
    }

viewHabitCount : Model -> String
viewHabitCount model =
    List.length model.habits |> String.fromInt

type Msg
    = ClickedDelete HabitId

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDelete _ ->
            ( model, Cmd.none )

toSession : Model -> Session
toSession model =
    model.session

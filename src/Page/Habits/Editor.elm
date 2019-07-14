module Page.Habits.Editor exposing (..)

import Html exposing (Html, h1, main_, text)
import Html.Attributes exposing (class, id, tabindex)
import Session exposing (Session)
import Habit.Id exposing (HabitId)

type alias Model =
    { session : Session
    , status : Status
    }

type Status
    = Editing HabitId
    | EditingNew

initNew : Session -> ( Model, Cmd Msg )
initNew session =
    ( { session = session
      , status = EditingNew
      }
    , Cmd.none
    )


initEdit : Session -> HabitId -> ( Model, Cmd Msg )
initEdit session habitId =
    ( { session = session
      , status = Editing habitId
      }
    , Cmd.none
    )

view : Model -> { title : String, content : Html Msg }
view model =
    { title =
        case getHabitId model.status of
            Just _ ->
                "Edit Habit"

            Nothing ->
                "New Habit"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Not Found" ]
            ]
    }


type Msg
    = ClickedCancel
    | ClickedSubmit

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCancel ->
            ( model, Cmd.none )

        ClickedSubmit ->
            ( model, Cmd.none )

toSession : Model -> Session
toSession model =
    model.session

getHabitId : Status -> Maybe HabitId
getHabitId status =
    case status of
        Editing habitId ->
            Just habitId
        EditingNew ->
            Nothing
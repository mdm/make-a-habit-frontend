module Page.Reminders exposing (..)

import Html exposing (Html, h1, main_, text)
import Html.Attributes exposing (class, id, tabindex)
import Habit.Id exposing (HabitId)
import Session exposing (Session)

type alias Model =
    { session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      }
    , Cmd.none
    )

view : Model -> { title : String, content : Html msg }
view model =
    { title = "Reminders"
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ h1 [] [ text "Reminders" ]
            ]
    }


type Msg
    = ClickedDone HabitId

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedDone _ ->
            ( model, Cmd.none )

toSession : Model -> Session
toSession model =
    model.session

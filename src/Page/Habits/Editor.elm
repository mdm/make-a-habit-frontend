module Page.Habits.Editor exposing (..)

import Html exposing (Html, Attribute, a, button, dd, div, dl, dt, form, h3, input, label, main_, option, select, strong, text)
import Html.Attributes exposing (checked, class, for, id, selected, tabindex, type_, value)
import Route
import Session exposing (Session)
import Habit.Id exposing (HabitId)

type alias Model =
    { session : Session
    , status : Status
    }

type Status
    = Loading HabitId
    | Editing HabitId (List Problem) Form
    | EditingNew (List Problem) Form

type Problem
    = ServerError String

type alias Form =
    { name : String
    , description : String
    , timeLimit : Int
    , recurrences : List Int  -- TODO: use custom Day type
    }



initNew : Session -> ( Model, Cmd Msg )
initNew session =
    ( { session = session
      , status =
            EditingNew []
                { name = "Test"
                , description = "Bla bla"
                , timeLimit = 3
                , recurrences = [3, 5]
                }
      }
    , Cmd.none
    )


initEdit : Session -> HabitId -> ( Model, Cmd Msg )
initEdit session habitId =
    ( { session = session
      , status = Loading habitId
      }
    , Cmd.none  -- TODO: load habit
    )

view : Model -> { title : String, content : Html Msg }
view model =
    let
        title =
            case getHabitId model.status of
                Just _ ->
                    "Edit Habit"

                Nothing ->
                    "New Habit"
        formHtml =
            case model.status of
                Loading _ ->
                    []

                Editing habitId problems form ->
                    [ viewForm form problems (editHabitSaveButton []) ]

                EditingNew problems form ->
                    [ viewForm form problems (newHabitSaveButton []) ]

    in
    { title = title
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ div [ class "Box" ]
                [ div [ class "Box-header" ]
                    [ h3 [ class "Box-title" ] [ text title ] ]
                , div [ class "Box-body" ]
                    formHtml
                , div [ class "Box-footer text-right" ] []
                ]
            ]
    }

viewForm : Form -> List Problem -> Html Msg -> Html Msg
viewForm fields problems saveButton =
    form []
        [ dl [ class "form-group" ]
            [ dt []
                [ label [ for "name" ] [ text "Name" ] ]
            , dd []
                [ input [ class "form-control", type_ "text", id "name", value fields.name ] [] ]
            ]
        , dl [ class "form-group" ]
            [ dt []
                [ label [ for "description" ] [ text "Description" ] ]
            , dd []
                [ input [ class "form-control", type_ "text", id "description", value fields.description ] [] ]
            ]
        , dl [ class "form-group" ]
            [ dt []
                [ label [ for "time-limit" ] [ text "Time limit for completion" ] ]
            , dd []
                [ select [ class "form-select", id "time-limit"]
                    (List.range 1 7 |> List.map (viewTimeLimit fields.timeLimit))
                ]
            ]
        , div [] <|
            strong [] [ text "Repeats every week on" ]
            :: List.indexedMap (viewRecurrence fields.recurrences) [ "Monday", "Tueday", "Wednesday", "Thursday", "Friday", "Satuday", "Sunday" ]
        , div [ class "form-actions" ]
            [ saveButton
            , a [ class "btn", Route.href Route.Habits] [ text "Cancel" ]
            ]
        ]

viewTimeLimit : Int -> Int -> Html Msg
viewTimeLimit selectedTimeLimit days =
    let
        formattedDays =
            case days of
                1 -> String.fromInt days ++ " day"
                _ -> String.fromInt days ++ " days"
    in
    option [ value <| String.fromInt days, selected <| days == selectedTimeLimit ] [ text formattedDays ]

viewRecurrence : List Int -> Int -> String -> Html Msg
viewRecurrence checkedRecurrences dayIndex dayName =
    div [ class "form-checkbox" ]
        [ label []
            [ input [ type_ "checkbox", id <| String.toLower dayName, checked (List.member dayIndex checkedRecurrences) ] []
            , text dayName
            ]
        ]

newHabitSaveButton : List (Attribute msg) -> Html msg
newHabitSaveButton extraAttrs =
    saveHabitButton "Create" extraAttrs

editHabitSaveButton : List (Attribute msg) -> Html msg
editHabitSaveButton extraAttrs =
    saveHabitButton "Save" extraAttrs

saveHabitButton : String -> List (Attribute msg) -> Html msg
saveHabitButton caption extraAttrs =
    button ([ class "btn btn-primary", type_ "submit" ] ++ extraAttrs) [ text caption ]


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
        Loading habitId ->
            Just habitId
        Editing habitId _ _ ->
            Just habitId
        EditingNew _ _ ->
            Nothing
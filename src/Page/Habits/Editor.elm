module Page.Habits.Editor exposing (..)

import Html exposing (Html, a, button, dd, div, dl, dt, form, h3, input, label, main_, option, select, strong, text)
import Html.Attributes exposing (checked, class, for, id, tabindex, type_, value)
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
                { name = ""
                , description = ""
                , timeLimit = 1
                , recurrences = []
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
    in
    { title = title
    , content =
        main_ [ id "content", class "container", tabindex -1 ]
            [ div [ class "Box" ]
                [ div [ class "Box-header" ]
                    [ h3 [ class "Box-title" ] [ text title ] ]
                , div [ class "Box-body" ]
                    [ form []
                        [ dl [ class "form-group" ]
                            [ dt []
                                [ label [ for "name" ] [ text "Name" ] ]
                            , dd []
                                [ input [ class "form-control", type_ "text", id "name"] [] ]
                            ]
                        , dl [ class "form-group" ]
                            [ dt []
                                [ label [ for "description" ] [ text "Description" ] ]
                            , dd []
                                [ input [ class "form-control", type_ "text", id "description"] [] ]
                            ]
                        , dl [ class "form-group" ]
                            [ dt []
                                [ label [ for "time-limit" ] [ text "Time limit for completion" ] ]
                            , dd []
                                [ select [ class "form-select", id "time-limit"]
                                    (List.range 1 7 |> List.map viewTimeLimit)
                                ]
                            ]
                        , div [] <|
                            strong [] [ text "Repeats every week on" ]
                            :: List.map viewRecurrence [ "Monday", "Tueday", "Wednesday", "Thursday", "Friday", "Satuday", "Sunday" ]
                        , div [ class "form-actions" ]
                            [ button [ class "btn btn-primary", type_ "submit" ] [ text "Submit" ]
                            , a [ class "btn", Route.href Route.Habits] [ text "Cancel" ]
                            ]
                        ]
                    ]
                , div [ class "Box-footer text-right" ] []
                ]
            ]
    }

viewTimeLimit : Int -> Html Msg
viewTimeLimit days =
    let
        formattedDays =
            case days of
                1 -> String.fromInt days ++ " day"
                _ -> String.fromInt days ++ " days"
    in
    option [ value formattedDays ] [ text formattedDays ]

viewRecurrence : String -> Html Msg
viewRecurrence day =
    div [ class "form-checkbox" ]
        [ label []
            [ input [ type_ "checkbox", id <| String.toLower day, checked True ] []
            , text day
            ]
        ]


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
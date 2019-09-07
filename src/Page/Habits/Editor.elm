module Page.Habits.Editor exposing (..)

import Html exposing (Html, Attribute, a, button, dd, div, dl, dt, form, h3, input, label, main_, option, select, strong, text)
import Html.Attributes exposing (checked, class, disabled, for, id, selected, tabindex, type_, value)
import Html.Events exposing (onCheck, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Route
import Session exposing (Session)
import Habit exposing (Habit)
import Habit.Id as HabitId exposing (HabitId)
import Api
import Api.Endpoint as Endpoint

type alias Model =
    { session : Session
    , status : Status
    }

type Status
    = Loading HabitId
    | LoadingFailed HabitId
    | Editing HabitId (List Problem) Form
    | Saving HabitId Form
    | EditingNew (List Problem) Form
    | Creating Form

type Problem
    = InvalidEntry ValidatedField String
    | ServerError String

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
        formHtml =
            case model.status of
                Loading _ ->
                    []

                Saving habitId form ->
                    [ viewForm form [] (editHabitSaveButton [ disabled True ]) ]

                Creating form ->
                    [ viewForm form [] (newHabitSaveButton [ disabled True ]) ]

                Editing habitId problems form ->
                    [ viewForm form problems (editHabitSaveButton []) ]

                EditingNew problems form ->
                    [ viewForm form problems (newHabitSaveButton []) ]

                LoadingFailed _ ->
                    [ text "Habit failed to load." ]
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
    form [ onSubmit ClickedSave ]
        [ dl [ class "form-group" ]
            [ dt []
                [ label [ for "name" ] [ text "Name" ] ]
            , dd []
                [ input [ class "form-control", type_ "text", id "name", value fields.name, onInput EnteredName ] [] ]
            ]
        , dl [ class "form-group" ]
            [ dt []
                [ label [ for "description" ] [ text "Description" ] ]
            , dd []
                [ input [ class "form-control", type_ "text", id "description", value fields.description, onInput EnteredDescription ] [] ]
            ]
        , dl [ class "form-group" ]
            [ dt []
                [ label [ for "time-limit" ] [ text "Time limit for completion" ] ]
            , dd []
                [ select [ class "form-select", id "time-limit", onInput SelectedTimeLimit ]
                    (List.range 1 7 |> List.map (viewTimeLimit fields.timeLimit))
                ]
            ]
        , div [] <|
            strong [] [ text "Repeats every week on" ]
            :: List.indexedMap (viewRecurrence fields.recurrences) [ "Monday", "Tueday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday" ]
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
            [ input [ type_ "checkbox", id <| String.toLower dayName, checked (List.member dayIndex checkedRecurrences), onCheck <| CheckedRecurrence dayIndex ] []
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
    = ClickedSave
    | EnteredName String
    | EnteredDescription String
    | SelectedTimeLimit String
    | CheckedRecurrence Int Bool
    | CompletedCreate (Result Http.Error Habit)
    | CompletedEdit (Result Http.Error Habit)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSave ->
            model.status
                |> save
                |> Tuple.mapFirst (\status -> { model | status = status })

        EnteredName name ->
            updateForm (\form -> { form | name = name }) model

        EnteredDescription description ->
            updateForm (\form -> { form | description = description }) model

        SelectedTimeLimit timeLimit ->
            updateForm (\form -> { form | timeLimit = Maybe.withDefault 1 (String.toInt timeLimit) }) model

        CheckedRecurrence dayIndex checked ->
            if checked then
                updateForm (\form -> { form | recurrences = dayIndex :: form.recurrences }) model
            else
                updateForm (\form -> { form | recurrences = List.filter (\recurrence -> recurrence /= dayIndex) form.recurrences }) model

        CompletedCreate (Ok habit) -> -- TODO: share habit with global state
            ( model
            , Route.replaceUrl (Session.navKey model.session) Route.Habits
            )

        CompletedCreate (Err error) ->
            ( { model | status = savingError error model.status }
            , Cmd.none
            )

        CompletedEdit (Ok habit) ->
            ( model
            , Route.replaceUrl (Session.navKey model.session) Route.Habits
            )

        CompletedEdit (Err error) ->
            ( { model | status = savingError error model.status }
            , Cmd.none
            )

save : Status -> ( Status, Cmd Msg )
save status =
    case status of
        Editing habitId _ form ->
            case validate form of
                Ok validForm ->
                    ( Saving habitId form
                    , edit habitId validForm
                    )

                Err problems ->
                    ( Editing habitId problems form
                    , Cmd.none
                    )

        EditingNew _ form ->
            case validate form of
                Ok validForm ->
                    ( Creating form
                    , create validForm
                    )

                Err problems ->
                    ( EditingNew problems form
                    , Cmd.none
                    )

        _ ->
            Debug.todo "Trying to submit even though we are not editing."

savingError : Http.Error -> Status -> Status
savingError error status =
    case status of
        Saving habitId form ->
            Editing habitId [ ServerError "Error saving habit" ] form
            
        Creating form ->
            EditingNew [ ServerError "Error creating habit" ] form
            
        _ ->
            status

updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    let
        newModel =
            case model.status of
                Loading _ ->
                    model

                LoadingFailed _ ->
                    model

                Creating form ->
                    { model | status = Creating (transform form) }

                Saving habitId form ->
                    { model | status = Saving habitId (transform form) }

                EditingNew errors form ->
                    { model | status = EditingNew errors (transform form) }

                Editing habitId errors form ->
                    { model | status = Editing habitId errors (transform form) }
    in
    ( newModel, Cmd.none )


type TrimmedForm
    = Trimmed Form

type ValidatedField
    = Name
    | Description
    | TimeLimit
    | Recurrences

fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Name
    , Description
    , TimeLimit
    , Recurrences
    ]

validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form    
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm
        problems ->
            Err problems

validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Name ->
                if String.isEmpty form.name then
                    [ "Name cannot be empty." ]
                
                else
                    []

            Description ->
                if String.isEmpty form.description then
                    [ "Description cannot be empty." ]
                
                else
                    []

            TimeLimit ->
                [] -- don't validate

            Recurrences ->
                if List.isEmpty form.recurrences then
                    [ "Habit must recur on at least one day." ]
                
                else
                    []

trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed { form | name = String.trim form.name, description = String.trim form.description }


edit : HabitId -> TrimmedForm -> Cmd Msg
edit habitId (Trimmed form) =
    let
        habit = -- TODO: avoid duplicating this code
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "description", Encode.string form.description )
                , ( "time_limit", Encode.int form.timeLimit )
                , ( "recurrences", Encode.list Encode.int form.recurrences )
                ]
        
        body =
            Http.jsonBody habit
    in
    Api.put CompletedCreate (Endpoint.habit habitId) body Habit.decoder

create : TrimmedForm -> Cmd Msg
create (Trimmed form) =
    let
        habit = -- TODO: avoid duplicating this code
            Encode.object
                [ ( "name", Encode.string form.name )
                , ( "description", Encode.string form.description )
                , ( "time_limit", Encode.int form.timeLimit )
                , ( "recurrences", Encode.list Encode.int form.recurrences )
                ]
        
        body =
            Http.jsonBody habit
    in
    Api.post CompletedCreate Endpoint.habits body Habit.decoder

toSession : Model -> Session
toSession model =
    model.session

getHabitId : Status -> Maybe HabitId
getHabitId status =
    case status of
        Loading habitId ->
            Just habitId

        LoadingFailed habitId ->
            Just habitId

        Saving habitId _ ->
            Just habitId

        Editing habitId _ _ ->
            Just habitId

        Creating _ ->
            Nothing

        EditingNew _ _ ->
            Nothing
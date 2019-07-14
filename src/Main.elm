module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Habit.Id exposing (HabitId)
import Page exposing (Page)
import Page.Habits as Habits
import Page.Habits.Editor as Editor
import Page.Reminders as Reminders
import Page.Blank as Blank
import Page.NotFound as NotFound
import Route exposing (Route)
import Session exposing (Session)
import Url



-- MODEL


type Model
    = Redirect Session
    | NotFound Session
    | Reminders Reminders.Model
    | Habits Habits.Model
    | Editor (Maybe HabitId) Editor.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    changeRouteTo (Route.fromUrl url) (Redirect (Session.Guest key))



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.view Page.Other Blank.view

        NotFound _ ->
            Page.view Page.Other NotFound.view

        Reminders reminders ->
            viewPage Page.Reminders GotRemindersMsg (Reminders.view reminders)

        Habits habits ->
            viewPage Page.Habits GotHabitsMsg (Habits.view habits)

        Editor _ editor ->
            viewPage Page.Habits GotEditorMsg (Editor.view editor)



-- UPDATE


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | GotRemindersMsg Reminders.Msg
    | GotHabitsMsg Habits.Msg
    | GotEditorMsg Editor.Msg

toSession : Model -> Session
toSession model =
    case model of
        Redirect session ->
            session
            
        NotFound session ->
            session

        Reminders reminders ->
            Reminders.toSession reminders

        Habits habits ->
            Habits.toSession habits

        Editor _ editor ->
            Editor.toSession editor

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )
        Just Route.Reminders ->
            Reminders.init session
                |> updateWith Reminders GotRemindersMsg model
        Just Route.Habits ->
            Habits.init session
                |> updateWith Habits GotHabitsMsg model
        Just Route.NewHabit ->
            Editor.initNew session
                |> updateWith (Editor Nothing) GotEditorMsg model
        Just (Route.EditHabit habitId) ->
            Editor.initEdit session habitId
                |> updateWith (Editor (Just habitId)) GotEditorMsg model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotRemindersMsg subMsg, Reminders reminders ) ->
            Reminders.update subMsg reminders
                |> updateWith Reminders GotRemindersMsg model

        ( GotHabitsMsg subMsg, Habits habits ) ->
            Habits.update subMsg habits
                |> updateWith Habits GotHabitsMsg model

        ( GotEditorMsg subMsg, Editor maybeHabitId editor ) ->
            Editor.update subMsg editor
                |> updateWith (Editor maybeHabitId) GotEditorMsg model

        ( _, _ ) ->
            ( model, Cmd.none )

updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }

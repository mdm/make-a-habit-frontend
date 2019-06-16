module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page exposing (Page)
import Page.Habits as Habits
import Page.Habits.Editor as Editor
import Page.Reminders as Reminders
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
    changeRouteTo (Route.fromUrl url) (Redirect (Guest key))



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Make A Habit"
    , body = CDN.stylesheet :: viewHeader model :: viewContent :: [ viewFooter ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    Navbar.config GotNavbarMsg
        --|> Navbar.withAnimation
        |> Navbar.brand [ href "#" ] [ text "Brand" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#" ] [ text "Item 1" ]
            , Navbar.itemLink [ href "#" ] [ text "Item 2" ]
            ]
        |> Navbar.view model.navbarState


viewContent : Html Msg
viewContent =
    text ""


viewFooter : Html Msg
viewFooter =
    text ""



-- UPDATE


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url.Url
    | ClickedLink Browser.UrlRequest
    | GotRemindersMsg Reminders.Msg
    | GotHabitsMsg Habits.Msg
    | GotEditorMsg Editor.Msg

changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( NotFound, Cmd.none )
        Just Route.Reminders ->
            Reminders.init
                |> updateWith Reminders GotRemindersMsg model
        Just Route.Habits ->
            Habits.init
                |> updateWith Habits GotHabitsMsg model
        Just Route.NewHabit ->
            Editor.initNew
                |> updateWith (Editor Nothing) GotEditorMsg model
        Just (Route.EditHabit habitId) ->
            Editor.initEdit habitId
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
            changeRouteTo routemodel

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

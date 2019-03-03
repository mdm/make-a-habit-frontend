module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url

import Bootstrap.CDN as CDN
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid


-- MODEL

type alias Model =
  { url : Url.Url
  , key : Nav.Key
  , navbarState : Navbar.State
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
    ( navbarState, navbarCmd ) =
      Navbar.initialState GotNavbarMsg
  in
  ( Model url key navbarState, navbarCmd )

-- UPDATE

type Msg
  = ClickedLink Browser.UrlRequest
  | ChangedUrl Url.Url
  | GotNavbarMsg Navbar.State

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ClickedLink urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )
        Browser.External href ->
          ( model, Nav.load href )
    ChangedUrl url ->
      ( { model | url = url }, Cmd.none )
    GotNavbarMsg state ->
      ( { model | navbarState = state }, Cmd.none )

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Make A Habit"
  , body = CDN.stylesheet :: viewHeader model :: viewContent :: [viewFooter]
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
viewContent = text ""

viewFooter : Html Msg
viewFooter = text ""

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

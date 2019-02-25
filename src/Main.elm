module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Url


-- MODEL

type alias Model =
  { url : Url.Url
  , key : Nav.Key
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model url key, Cmd.none )

-- UPDATE

type Msg
  = ClickedLink Browser.UrlRequest
  | ChangedUrl Url.Url

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

-- VIEW

view : Model -> Browser.Document Msg
view model =
  { title = "Make A Habit"
  , body =
    [ h1 [] [ text "Hello world!" ]
    ]
  }

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

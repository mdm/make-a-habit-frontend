module Page exposing (..)

import Browser exposing (Document)
import Html exposing (Html, a, div, h1, nav, text)
import Html.Attributes exposing (class, classList)
import Route exposing (Route)

type Page
    = Other
    | Reminders
    | Habits

view : Page -> { title : String, content : Html msg } -> Document msg
view page { title, content } =
    { title = title ++ " - Make A Habit"
    , body = viewHeader page :: [ content ]
    }

viewHeader : Page -> Html msg
viewHeader page =
    div []
        [ div [ class "bg-gray-dark" ]
            [ div [ class "container text-white" ]
                [ h1 []
                    [ text "Make A Habit" ]
                ]
            ]
        , div [ class "container" ]
            [ div [ class "tabnav" ]
                [ nav [ class "tabnav-tabs" ] <|
                    viewMenu page
                ]
            ]
        ]

viewMenu : Page -> List (Html msg)
viewMenu page =
    let
        linkTo =
            navbarLink page
    in
    [ linkTo Route.Reminders [ text "Reminders" ]
    , linkTo Route.Habits [ text "Habits" ]
    ]

navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    a [ classList [ ( "tabnav-tab", True ), ("selected", isActive page route ) ], Route.href route ] linkContent

isActive :  Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
    ( Reminders, Route.Reminders ) ->
        True

    ( Habits, Route.Habits ) ->
        True
    
    _ ->
        False

module Page exposing (..)

import Browser exposing (Document)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (class)

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
        ]
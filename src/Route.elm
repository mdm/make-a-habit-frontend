module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Habit.Id as HabitId exposing (HabitId)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Reminders
    | Habits
    | NewHabit
    | EditHabit HabitId


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Reminders Parser.top
        , Parser.map Habits (s "habits")
        , Parser.map NewHabit (s "habits" </> s "new")
        , Parser.map EditHabit (s "habits" </> HabitId.urlParser)
        ]


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl =
    Parser.parse parser


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Reminders ->
                    []

                Habits ->
                    [ "habits" ]

                NewHabit ->
                    [ "habits", "new" ]

                EditHabit habitId ->
                    [ "habits", HabitId.toString habitId ]
    in
    "/" ++ String.join "/" pieces

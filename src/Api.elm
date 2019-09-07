module Api exposing (get, post, put)

import Api.Endpoint as Endpoint exposing (Endpoint)
import Http exposing (Body)
import Json.Decode as Decode exposing (Decoder)

get : (Result Http.Error a -> msg) -> Endpoint -> Decoder a -> Cmd msg
get toMsg url decoder =
    Endpoint.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }

post : (Result Http.Error a -> msg) -> Endpoint -> Body -> Decoder a -> Cmd msg
post toMsg url body decoder =
    Endpoint.request
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }

put : (Result Http.Error a -> msg) -> Endpoint -> Body -> Decoder a -> Cmd msg
put toMsg url body decoder =
    Endpoint.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson toMsg decoder
        , timeout = Nothing
        , tracker = Nothing
        }

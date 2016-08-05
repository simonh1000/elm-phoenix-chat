module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Http
import Json.Decode as Json exposing ( (:=) )
import Json.Encode

import Task

import Phoenix.Socket as Socket exposing (withDebug)
import Phoenix.Channel as Channel
import Phoenix.Push

-- CONSTANTS

serverUrl = "http://localhost:4000/api/default"
socketUrl = "ws://localhost:4000/socket/websocket"
-- MODEL

type alias Model =
    { msg : String
    , phxSocket : Socket.Socket Msg
    }

init : (Model, Cmd Msg)
init =
    ( Model "Loading..." (Socket.init socketUrl |> withDebug)
    , loadData
    )

-- UPDATE

type Msg
    = Join
    | FetchSucceed String
    | FetchFail Http.Error
    | PhoenixMsg (Socket.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Join ->
            let
                payload = Json.Encode.object [ ("user_id", Json.Encode.string "123") ]
                channel = Channel.init "room:lobby" |> Channel.withPayload payload
                (socket', cmd) = Socket.join channel model.phxSocket
            in
            ( { model | phxSocket = socket' }
            , Cmd.map PhoenixMsg cmd
            )
        FetchSucceed str ->
            { model | msg = str } ! []
        FetchFail err ->
            { model | msg = toString err } ! []
        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) = Socket.update msg model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )
-- VIEW

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text model.msg ]
        , button
            [ onClick Join ]
            [ text "Join Channel" ]
        , div []
            [ text <| toString model ]
        ]

-- TASKS
loadData : Cmd Msg
loadData =
    Http.get ("data" := Json.string) serverUrl
    |> Task.perform FetchFail FetchSucceed

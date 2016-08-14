module App exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Http
import Json.Decode as Json exposing ( (:=) )
import Json.Encode as E

import Task

import Phoenix.Socket as Socket exposing (withDebug)
import Phoenix.Channel as Channel
import Phoenix.Push

import Login
import Chat

-- CONSTANTS

serverUrl = "http://localhost:4000/api/default"
socketUrl = "ws://localhost:4000/socket/websocket"

-- MODEL
type ViewType
    = Login
    | Chat

type alias Model =
    { viewType : ViewType
    , login : Login.Model
    , chat : Chat.Model
    , debugMsg : String
    , phxSocket : Socket.Socket Msg
    }

init : (Model, Cmd Msg)
init =
    ( Model
        Login Login.init Chat.init "Loading..."
        (Socket.init socketUrl |> withDebug)
    , loadData
    )

-- UPDATE

type Msg
    = Join
    -- Components
    | LoginMsg Login.Msg
    | ChatMsg Chat.Msg
    -- Http
    | FetchSucceed String
    | FetchFail Http.Error
    -- Channels
    | PhoenixMsg (Socket.Msg Msg)
    -- Channels - output
    | ReceiveChatMessage E.Value
    | ShowJoinedMessage String
    | ReceiveReply E.Value

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        LoginMsg msg ->
            let (m, e) = Login.update msg model.login
            in
            ( { model | login = m }
            , Cmd.map LoginMsg e
            )
            -- case msg of
            --     Login.Submit username ->
            --         let
            --             payload = E.object [ ("username", E.string username) ]
            --             channel =
            --                 Channel.init "room:lobby"
            --                 |> Channel.withPayload payload
            --                 |> Channel.onJoin (always (ShowJoinedMessage "rooms:lobby"))
            --             (socket', cmd) = Socket.join channel model.phxSocket
            --         in
            --         ( { model | phxSocket = socket' }
            --         , Cmd.map PhoenixMsg cmd
            --         )
            --     _ ->
            --         { model | login = Login.update msg model.login } ! []
        FetchSucceed str ->
            { model | debugMsg = str } ! []
        FetchFail err ->
            { model | debugMsg = toString err } ! []
        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    model.phxSocket
                    |> Socket.on "phx_reply" "room:lobby" ReceiveReply
                    |> Socket.on "new_msg" "room:lobby" ReceiveChatMessage
                    |> Socket.update msg
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )
        ShowJoinedMessage s ->
            { model | viewType = Chat } ! []
        _ ->
            { model | debugMsg = toString message } ! []
-- VIEW

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Simon's Elm-Phoenix Chat" ]
        , case model.viewType of
            Login ->
                Login.view model.login |> Html.map LoginMsg
            Chat ->
                Chat.view model.chat |> Html.map ChatMsg
        , div []
            [ text <| toString model.debugMsg ]
        ]

-- Commands

loadData : Cmd Msg
loadData =
    Http.get ("data" := Json.string) serverUrl
    |> Task.perform FetchFail FetchSucceed

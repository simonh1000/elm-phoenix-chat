module App exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)

import Http
import Json.Decode as Json exposing ( (:=) )
import Json.Encode as E

import Task

import Material
import Material.Typography as Typo
import Material.Grid as Grid exposing (..)
-- import Material.Options as Options
import Material.Layout as Layout

import Phoenix.Socket as Socket exposing (withDebug)
import Phoenix.Channel as Channel
import Phoenix.Push as Push

import Login
import Chat

-- CONSTANTS

serverUrl = "/api/default"
socketUrl = "ws://192.168.0.10:4000/socket/websocket"

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
    , mdl : Material.Model
    }

init : (Model, Cmd Msg)
init =
    ( Model
        Login Login.init Chat.init ""
        (Socket.init socketUrl |> withDebug)
        Material.model
    , Cmd.none
    )
    -- Model
    --     Login Login.init Chat.init "Loading..."
    --     (Socket.init socketUrl |> withDebug)
    --     Material.model
    -- |> joinChat

-- UPDATE

type Msg
    -- Components
    = LoginMsg Login.Msg
    | Join
    -- Chat
    | ChatMsg Chat.Msg
    | Send

    -- Http
    | FetchSucceed String
    | FetchFail Http.Error
    -- Channels
    | PhoenixMsg (Socket.Msg Msg)
    -- Channels - output
    | ReceiveChatMessage E.Value
    | ShowJoinedMessage String
    | ReceiveReply E.Value
    -- Mdl
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        LoginMsg msg ->
            let
                (m, c1, maybeMsg) = Login.update Join msg model.login
                newModel = { model | login = m }
                c1' = Cmd.map LoginMsg c1
            in
            case maybeMsg of
                Just msg' ->
                    let (newModel', c2) = update msg' newModel
                    in (newModel', Cmd.batch [c1', c2])
                _ ->
                    (newModel, c1')
        Join ->
            joinChat model
        ChatMsg msg ->
            let
                (m, c1, maybeMsg) = Chat.update Send msg model.chat
                newModel = { model | chat = m }
                c1' = Cmd.map ChatMsg c1
            in
            case maybeMsg of
                Just msg' ->
                    let (newModel', c2) = update msg' newModel
                    in (newModel', Cmd.batch [c1', c2])
                _ ->
                    (newModel, c1')
        Send ->
            let
                payload = E.object [ ("body", E.string model.chat.newMessage) ]
                push =
                    Push.init "new_msg" "room:lobby"
                    |> Push.withPayload payload
                (socket', socketCmd) =
                    Socket.push push model.phxSocket
            in
            ( { model | phxSocket = socket' }
            , Cmd.map PhoenixMsg socketCmd
            )
        -- ReceiveChatMessage jsonValue ->
        --     update (ChatMsg Chat.NewMessage jsonValue) model
        FetchSucceed str ->
            { model | debugMsg = str } ! []
        FetchFail err ->
            { model | debugMsg = toString err } ! []
        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    model.phxSocket
                    |> Socket.on "phx_reply" "room:lobby" ReceiveReply
                    |> Socket.on "new_msg" "room:lobby" (ChatMsg << Chat.NewMessage)
                    |> Socket.on "new_member" "room:lobby" (ChatMsg << Chat.NewMember)
                    |> Socket.on "lost_member" "room:lobby" (ChatMsg << Chat.LostMember)
                    |> Socket.update msg
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )
        ShowJoinedMessage s ->
            { model | viewType = Chat } ! []
        _ ->
            { model | debugMsg = toString message } ! []


joinChat : Model -> (Model, Cmd Msg)
joinChat model =
    let
        payload = E.object [ ("username", E.string model.login.username) ]
        channel =
            Channel.init "room:lobby"
            |> Channel.withPayload payload
            |> Channel.onJoin (always (ShowJoinedMessage "room:lobby"))
        (socket', cmd) = Socket.join channel model.phxSocket
    in
    ( { model | phxSocket = socket' }
    , Cmd.map PhoenixMsg cmd
    )

-- VIEW
view : Model -> Html Msg
view model =
    Layout.render Mdl model.mdl
        [ Layout.fixedHeader
        ]
        { header =
            [ h1
                [ style [("padding", "2rem")] ]
                [ text "Simon's Elm-Phoenix Chat" ]
            ]
        , drawer = []
        , tabs = ([], [])
        , main = [ viewBody model ]
        }

viewBody : Model -> Html Msg
viewBody model =
    div [ class "main-container" ]
        [ case model.viewType of
            Login ->
                Login.view model.login |> Html.map LoginMsg
            Chat ->
                viewApp model
        , text model.debugMsg
        ]

viewApp model =
    grid []
        [ cell
            [ size All 6
            , stretch
            ] [ Chat.view model.chat |> Html.map ChatMsg ]
        , cell
            [ size All 6
            , stretch
            ] [ text "tbc" ]
        ]
-- Commands

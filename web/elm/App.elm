module App exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)

import Http
import Json.Encode exposing (Value, null)
import List as L

-- import Task

import Material
import Material.Helpers exposing (map1st, map2nd, delay, pure, cssTransitionStep)
import Material.Typography as Typo
import Material.Grid as Grid exposing (..)
import Material.Layout as Layout
import Material.Snackbar as Snackbar
import Material.Options exposing (cs)

import Phoenix.Socket as Socket exposing (withDebug)
import Phoenix.Channel as Channel
import Phoenix.Push as Push

import Common exposing (..)
import SocketHelpers as SH exposing (..)
import Login
import Chat

-- CONSTANTS

serverUrl = "/api/default"
lobby = "room:lobby"
-- MODEL
type ViewType
    = Login
    | Chat

type alias Model =
    { username : String
    , login : Login.Model
    , chat : Chat.Model
    , viewType : ViewType
    , debugMsg : String
    , phxSocket : Socket.Socket Msg
    , mdl : Material.Model
    , errors : Snackbar.Model ()
    }

type alias Flags = String

init : Flags -> (Model, Cmd Msg)
init loc =
    ( Model
        "" Login.init Chat.init Login ""
        (initPhxSocket loc) Material.model Snackbar.model
    , Cmd.none )

initPhxSocket : String -> Socket.Socket Msg
initPhxSocket loc =
    SH.init loc

-- UPDATE

type Msg
    -- Login Page and follow-on messages
    = LoginMsg Login.Msg
    | Signin String
    | JoinLobby Value
    -- Chat Page and follow-on messages
    | ChatMsg Chat.Msg
    | SendChat Value
    | ReceiveChatMessage Value
    | SendSound Value
    | MyChannelSuccess Value

    -- Channels
    | PhoenixMsg (Socket.Msg Msg)
    -- Channels - output
    | ShowJoinedMessage Value
    | JoinError String Value
    -- Mdl
    | Mdl (Material.Msg Msg)
    | SnackbarMsg (Snackbar.Msg ())
    | Toast String
    -- | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        -- Login Page
        LoginMsg msg ->
            let
                (m1, c1, maybeMsg) = Login.update Signin msg model.login
                res1 = ({ model | login = m1 }, Cmd.map LoginMsg c1)
            in bind' res1 update maybeMsg
        Signin username ->
            { model | username = username }
            |> joinChat
            |> bind makeIndividualChannel
        JoinLobby val ->
            { model | viewType = Chat }
            |> update (ChatMsg <| Chat.JoinLobby val)
        -- Chat component
        ChatMsg msg ->
            let
                config = Chat.Config SendChat Toast SendSound
                (m1, c1, maybeMsg) = Chat.update config msg model.chat
                res1 = ({ model | chat = m1 }, Cmd.map ChatMsg c1)
            in bind' res1 update maybeMsg
        SendChat payload ->
            pushSocket "new_msg" lobby PhoenixMsg payload model
        SendSound payload ->
            pushSocket "send_sound" (myRoom model.username) PhoenixMsg payload model
        -- Other
        PhoenixMsg msg ->
            let (phxSocket, phxCmd) = Socket.update msg model.phxSocket
            in
            ( { model | phxSocket = phxSocket }
            , Cmd.map PhoenixMsg phxCmd
            )
        JoinError channel body ->
            update (Toast <| channel ++ ": " ++ toString body) model

        Mdl msg ->
            Material.update msg model
        SnackbarMsg msg ->
            Snackbar.update msg model.errors
                |> map1st (\s -> { model | errors = s })
                |> map2nd (Cmd.map SnackbarMsg)
        Toast error ->
            let
                _ = Debug.log "**Toast: " error
                -- snackbar : a -> String -> String -> Contents a
                -- snack =
                    -- Snackbar.snackbar () "Error" error
                    -- Snackbar.Contents "Error" (Just error) () 6000 250
                toast =
                    Snackbar.Contents error Nothing () 6000 250
                -- add : Contents a -> Model a -> (Model a, Cmd (Msg a))
                (errors, c1) = Snackbar.add toast model.errors
            in
            ( { model | errors = errors }
            , Cmd.map SnackbarMsg c1
            )
        _ ->
            { model | debugMsg = toString message } ! []


-- VIEW
view : Model -> Html Msg
view model =
    Layout.render Mdl model.mdl
        [ Layout.fixedHeader
        ]
        { header =
            [ h1 [] [ text "Simon's Elm-Phoenix Chat" ] ]
        , drawer = []
        , tabs = ([], [])
        , main =
            [ viewBody model
            , Snackbar.view model.errors |> Html.map SnackbarMsg
            ]
        }

viewBody : Model -> Html Msg
viewBody model =
    case model.viewType of
        Login ->
            Login.view model.login |> Html.map LoginMsg
        Chat ->
            Chat.view model.chat |> Html.map ChatMsg

-- Commands

joinChat : Model -> (Model, Cmd Msg)
joinChat model =
    let payload = Login.encoder model.login
    in joinChannel payload lobby JoinLobby (JoinError lobby)
        [ ("new_msg", ChatMsg << Chat.NewMessage)
        , ("new_member", ChatMsg << Chat.NewMember)
        , ("lost_member", ChatMsg << Chat.LostMember)
        ] PhoenixMsg model

makeIndividualChannel : Model -> (Model, Cmd Msg)
makeIndividualChannel model =
    let indivRoom = myRoom model.username
    in joinChannel null indivRoom (ChatMsg << Chat.LoadSoundPlayer) (JoinError indivRoom)
        [ ("receive_sound", ChatMsg << Chat.Receive)
        ] PhoenixMsg model

myRoom username =
    "user:" ++ username

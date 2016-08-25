module Chat exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode exposing (decodeValue, Value)

import List as L
import Date exposing (Date)
import Platform.Cmd exposing (Cmd)

import Material
import Material.Grid as Grid exposing (grid, cell, size, offset, Device(..))
import Material.Textfield as Textfield
import Material.Button as Button
import Material.List as MList
import Material.Helpers exposing (..)

import Decoders exposing (..)

type alias Model =
    { messages : Messages
    , newMessage : String
    , members : List String
    , mdl : Material.Model
    }

init = Model [] "" [] Material.model

type alias Messages =
    List Message

type alias Message =
    { userName : String
    , body : String
    -- , time : Date
    }

type Msg
    = NewMessage Value
    | NewMember Value
    | LostMember Value
    | Send
    | UpdateInput String
    | Mdl (Material.Msg Msg)


update : msg -> Msg -> Model -> (Model, Cmd Msg, Maybe msg)
update sendMsg message model =
    case message of
        NewMessage jsonMessage ->
            case decodeValue (messageDecoder Message) jsonMessage of
                Result.Ok newMsg ->
                    ( { model
                            | messages = newMsg :: model.messages
                            , newMessage = "" }
                    , Cmd.none, Nothing
                    )
                Result.Err err ->
                    let _ = Debug.log "error" err
                    in (model, Cmd.none, Nothing)
        NewMember jsonMessage ->
            case decodeValue newMemberDecoder jsonMessage of
                Result.Ok (newUsername, members) ->
                    let newMessage = Message "system" <| newUsername ++ " joined"
                    in
                    ( { model
                            | messages = newMessage :: model.messages
                            , members = members }
                    , Cmd.none, Nothing
                    )
                Result.Err err ->
                    let _ = Debug.log "error" err
                    in (model, Cmd.none, Nothing)
        LostMember jsonMessage ->
            case decodeValue (messageDecoder Message) jsonMessage of
                Result.Ok lostMember ->
                    let newMessage = Message "system" <| lostMember.userName ++ " left"
                    in
                    ( { model
                            | messages = newMessage :: model.messages
                            , members = model.members |> L.filter ((/=) lostMember.userName) }
                    , Cmd.none, Nothing
                    )
                Result.Err err ->
                    let _ = Debug.log "error" err
                    in (model, Cmd.none, Nothing)
        UpdateInput s ->
            ({ model | newMessage = s}, Cmd.none, Nothing)
        Send ->
            (model, Cmd.none, Just sendMsg)
        Mdl msg ->
            let (m, c) = Material.update msg model
            in (m, c, Nothing)


view : Model -> Html Msg
view model =
    div [ id "chat" ]
        [ model.messages |> L.reverse |> L.map viewMessage |> div [ class "messages" ]
        , text <| toString model.members
        , inputView model
        ]

viewMessage : Message -> Html Msg
viewMessage msg =
    MList.li []
        [ MList.content []
            [ span [ class <| "user-name " ++ msg.userName] [ text msg.userName ]
            , span [ class "message" ] [ text <| ": " ++ msg.body ]
            ]
        ]

inputView model =
    Html.form
        [ onSubmit Send
        , class "message-input"
        ]
        [ Textfield.render Mdl [0] model.mdl
            [ Textfield.label "New message"
            , Textfield.floatingLabel
            , Textfield.value model.newMessage
            , Textfield.onInput UpdateInput
            ]
        ,  Button.render Mdl [1] model.mdl
            [ Button.ripple
            , if model.newMessage == "" then
                Button.disabled
              else
                Button.raised
            ]
            [ text "Send"]
        ]

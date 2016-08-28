module SoundPlayer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Json exposing (decodeValue, Value)
import Json.Encode as E
import List as L

import Material
import Material.Toggles as Toggles
import Material.Button as Button

import Ports exposing (..)
import Decoders exposing (..)
import Common exposing (..)

type alias User = String
type alias Sound = String

type alias Model =
    { selectedUser : User
    , mdl : Material.Model
    }

type alias PlayCommand =
    { target : String
    , soundfile : String
    }

init = Model "" Material.model

type Msg
    = Send User Sound
    | Receive Value
    | Mdl (Material.Msg Msg)

type alias Config msg =
    { username : String
    , sendMsg : E.Value -> msg
    }

update : Config msg -> Msg -> Model -> (Model, Cmd Msg, Maybe msg)
update config message model =
    case Debug.log "SP.update" message of
        Receive jsonMessage ->
            case decodeValue receiveSoundDecoder jsonMessage of
                Result.Ok soundfile ->
                    (model, player soundfile, Nothing)
                Result.Err err ->
                    model |> nothing
        Send user sound ->
            let payload = encodeSendSound user sound
            in (model, Cmd.none, Just <| config.sendMsg payload)
        Mdl msg ->
            let (m, c) = Material.update msg model
            in (m, c, Nothing)

view : List User -> Maybe Model -> Html Msg
view users model =
    model
    |> Maybe.map (viewMain users)
    |> Maybe.withDefault (text "This component will shown when the user-channel is available")

viewMain : List User -> Model -> Html Msg
viewMain users model =
    let
        radioToggle idx user =
            Button.render Mdl [idx] model.mdl
                [ Button.ripple
                , Button.raised
                , Button.onClick (Send user "guitar")
                ]
                [ text user ]
            -- Toggles.radio Mdl [0] model.mdl
            --     [ Toggles.value True
            --     , Toggles.group "users"
            --     , Toggles.ripple
            --     , Toggles.onClick (Send user "guitar")
            --     ] [ text user ]
    in
        div [] <|
            L.indexedMap radioToggle users

        -- [ text "Emacs" ]
        -- button
        --     [ onClick (Send "Jona" "") ]
        --     [ text "send sound" ]

encodeSendSound : String -> String -> E.Value
encodeSendSound username soundfile =
    E.object
        [ ("target", E.string username)
        , ("soundfile", E.string soundfile)
        ]

-- div
--   []
--   [ Toggles.radio Mdl [0] model.mdl
--       [ Toggles.value True
--       , Toggles.group "MyRadioGroup"
--       , Toggles.ripple
--       , Toggles.onClick MyRadioMsg1
--       ]
--       [ text "Emacs" ]
--   , Toggles.radio Mdl [1] model.mdl
--       [ Toggles.value False
--       , Toggles.group "MyRadioGroup"
--       , Toggles.ripple
--       , Toggles.onClick MyRadioMsg2
--       ]
--       [ text "Vim" ]
--   ]

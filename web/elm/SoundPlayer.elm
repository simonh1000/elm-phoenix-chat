module SoundPlayer exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode exposing (decodeValue, Value)
import List as L

import Material
import Material.Toggles as Toggles
import Material.Button as Button

import Ports exposing (..)
import Decoders exposing (..)

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
    , sendMsg : User -> Sound -> msg
    }

update : Config msg -> Msg -> Model -> (Model, Cmd Msg, Maybe msg)
update config message model =
    case message of
        Receive jsonMessage ->
            case decodeValue (messageDecoder PlayCommand) jsonMessage of
                Result.Ok newCommand ->
                    if newCommand.target == config.username then
                        (model, player newCommand.soundfile, Nothing)
                    else
                        (model, Cmd.none, Nothing)
                _ ->
                    (model, Cmd.none, Nothing)
        Send user sound ->
            (model, Cmd.none, Just <| config.sendMsg user sound)
        Mdl msg ->
            let (m, c) = Material.update msg model
            in (m, c, Nothing)

view : List User -> Model -> Html Msg
view users model =
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

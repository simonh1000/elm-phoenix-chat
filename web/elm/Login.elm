module Login exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Material
import Material.Textfield as Textfield
import Material.Helpers

type alias Model =
    { username : String
    , mdl : Material.Model
    }

init = Model "" Material.model

type Msg
    = UpdateUserName String
    | Submit Model
    | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
    case message of
        UpdateUserName un ->
            { model | username = un } ! []
        Mdl msg ->
           Material.update msg model
        _ ->
            model ! []

view : Model -> Html Msg
view model =
    div
        [ class "login" ]
        [ Textfield.render Mdl [0] model.mdl
            [ Textfield.label "Chat name"
            , Textfield.floatingLabel
            , Textfield.value model.username
            , Textfield.onInput UpdateUserName
            ]
        -- , input
        --     [ onInput UpdateUserName ]
        --     [ text model ]
        , button
            [ onClick (Submit model) ]
            [ text "Submit" ]
        ]

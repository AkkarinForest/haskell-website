module Main exposing (Model, Msg(..), defaultModel, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events as Events



-- PROGRAM


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( defaultModel, Cmd.none )


type alias Flags =
    {}



-- MODEL


type alias Model =
    { input : String }


defaultModel =
    { input = "init" }



-- UPDATE


type Msg
    = Post


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Post ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    button [ Events.onClick Post ] [ text "Submit" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

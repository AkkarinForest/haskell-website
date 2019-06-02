module Main exposing (Model, Msg(..), defaultModel, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode



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
    { input : String, id : String }


defaultModel =
    { input = "init", id = "no id" }



-- UPDATE


type Msg
    = Post
    | GotId (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Post ->
            ( model, postSymbol )

        GotId result ->
            case result of
                Ok id ->
                    ( { model | id = id }, Cmd.none )

                Err error ->
                    ( { model | id = "error" }, Cmd.none )


postSymbol : Cmd Msg
postSymbol =
    Http.post
        { url = "http://localhost:8001/symbols"
        , body = Http.jsonBody symbol
        , expect = Http.expectString GotId
        }


symbol : Encode.Value
symbol =
    Encode.object [ ( "symbols", Encode.string "b" ) ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ Events.onClick Post ] [ text "Submit" ]
        , text model.id
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

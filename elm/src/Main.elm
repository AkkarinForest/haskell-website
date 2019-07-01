module Main exposing (Model, Msg(..), defaultModel, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Time



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
    { id : String, content : String, symbols : List InputAction, previousTime : Int }


defaultModel =
    { id = "no id", content = "", symbols = [], previousTime = 0 }


type alias InputAction =
    { symbol : String, time : Int }



-- UPDATE


type Msg
    = Post
    | GotId (Result Http.Error String)
    | UpdateContent String
    | RecordInput Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Post ->
            let
                postAllSymbols =
                    Cmd.batch (List.map postSymbol model.symbols)
            in
            ( model, postAllSymbols )

        GotId result ->
            case result of
                Ok id ->
                    ( { model | id = id }, Cmd.none )

                Err error ->
                    ( { model | id = "error" }, Cmd.none )

        UpdateContent content ->
            ( { model | content = content }, Task.perform RecordInput Time.now )

        RecordInput newTime ->
            let
                timeDiff =
                    Time.posixToMillis newTime - model.previousTime

                symbols =
                    { symbol = String.right 1 model.content, time = timeDiff } :: model.symbols

                x =
                    Debug.log "elm" symbols
            in
            ( { model | symbols = symbols, previousTime = Time.posixToMillis newTime }, Cmd.none )


postSymbol : InputAction -> Cmd Msg
postSymbol inputAction =
    Http.post
        { url = "http://localhost:8001/symbols"
        , body = Http.jsonBody (encodeSymbol inputAction)
        , expect = Http.expectString GotId
        }


encodeSymbol : InputAction -> Encode.Value
encodeSymbol inputAction =
    Encode.object [ ( "symbol", Encode.string inputAction.symbol ), ( "time", Encode.int inputAction.time ) ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ Attr.value model.content
            , Events.onInput UpdateContent
            ]
            []
        , button [ Events.onClick Post ] [ text "Submit" ]
        , text model.id
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

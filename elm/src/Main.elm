module Main exposing (Model, Msg(..), defaultModel, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..), WebData)
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
    { id : String
    , content : String
    , symbols : List InputAction
    , previousTime : Int
    , stats : WebData (List Stat)
    }


type alias Stat =
    { symbol : String
    , time : Int
    }


defaultModel =
    { id = "no id"
    , content = ""
    , symbols = []
    , previousTime = 0
    , stats = NotAsked
    }


type alias InputAction =
    { symbol : String, time : Int }



-- UPDATE


type Msg
    = Post
    | GotId (Result Http.Error String)
    | UpdateContent String
    | RecordInput Time.Posix
    | NewStats (WebData (List Stat))
    | ExecCmd (Cmd Msg)


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

        NewStats stats ->
            ( { model | stats = stats }, Cmd.none )

        ExecCmd cmd ->
            ( model, cmd )


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
        [ viewInput model
        , viewStats model
        ]


viewStats model =
    div []
        [ button [ Events.onClick (ExecCmd getStats) ] [ text "Stats" ]
        , case model.stats of
            Success stats ->
                div [] (List.map viewStat stats)

            Loading ->
                div [] [ text "loading" ]

            NotAsked ->
                div [] [ text "not asked" ]

            Failure err ->
                div [] [ text ("Error: " ++ Debug.toString err) ]
        ]


getStats : Cmd Msg
getStats =
    Http.get
        { url = "http://localhost:8001/symbols_stats"
        , expect = Http.expectJson (RemoteData.fromResult >> NewStats) symbolsDecoder
        }


symbolsDecoder : Decode.Decoder (List Stat)
symbolsDecoder =
    let
        symbolDecoder =
            Decode.map2 Stat
                (Decode.field "symbol" Decode.string)
                (Decode.field "time" Decode.int)
    in
    Decode.list symbolDecoder


viewStat stat =
    div []
        [ text stat.symbol
        , text " : "
        , text (String.fromInt stat.time)
        ]


viewInput model =
    div
        []
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

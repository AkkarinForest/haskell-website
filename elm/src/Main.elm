module Main exposing (Model, Msg(..), defaultModel, main, subscriptions, update, view)

import Browser
import Element as Elem exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
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
    , stats = Success [ { symbol = "a", time = 1 }, { symbol = "b", time = 2 }, { symbol = "c", time = 3 } ]
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
    Elem.layout [] <|
        Elem.column [ Elem.width Elem.fill ]
            [ viewInput model.content
            , viewStats model.stats
            ]


viewInput : String -> Element Msg
viewInput content =
    Elem.column [ Elem.width Elem.fill, Elem.padding 50, Elem.spacing 20 ]
        [ Input.text [ Elem.spacing 20 ]
            { text = content
            , label =
                Input.labelAbove
                    [ Elem.centerX
                    ]
                    (Elem.text "Type the sentence")
            , onChange = UpdateContent
            , placeholder = Nothing
            }
        , Input.button
            [ Elem.centerX
            , Elem.padding 10
            ]
            { onPress = Just Post, label = Elem.text "Submit" }
        ]


viewStats : WebData (List Stat) -> Element Msg
viewStats statsData =
    Elem.column [ Elem.spacing 10, Elem.padding 10 ]
        [ Input.button [] { onPress = Just (ExecCmd getStats), label = Elem.text "Stats" }
        , case statsData of
            Success stats ->
                Elem.wrappedRow [] (List.map viewStat stats)

            Loading ->
                Elem.el [] (Elem.text "loading")

            NotAsked ->
                Elem.el [] (Elem.text "not asked")

            Failure err ->
                Elem.el [] (Elem.text ("Error: " ++ Debug.toString err))
        ]


viewStat : Stat -> Element Msg
viewStat stat =
    Elem.row []
        [ Elem.text stat.symbol
        , Elem.text " : "
        , Elem.text (String.fromInt stat.time)
        , Elem.text ", "
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

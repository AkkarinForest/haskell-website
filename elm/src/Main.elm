module Main exposing (Model, Msg(..), defaultModel, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
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
    { input : String, id : String, content : String }


defaultModel =
    { input = "init", id = "no id", content = "" }



-- UPDATE


type Msg
    = Post
    | GotId (Result Http.Error String)
    | UpdateContent String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Post ->
            ( model, postSymbol model.content )

        GotId result ->
            case result of
                Ok id ->
                    ( { model | id = id }, Cmd.none )

                Err error ->
                    ( { model | id = "error" }, Cmd.none )

        UpdateContent content ->
            ( { model | content = content }, Cmd.none )


postSymbol : String -> Cmd Msg
postSymbol symbol =
    Http.post
        { url = "http://localhost:8001/symbols"
        , body = Http.jsonBody (encodeSymbol symbol)
        , expect = Http.expectString GotId
        }


encodeSymbol : String -> Encode.Value
encodeSymbol symbol =
    Encode.object [ ( "symbol", Encode.string symbol ) ]



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

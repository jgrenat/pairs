module Main exposing (main)

import Browser
import Html exposing (Html, div, text)


type alias Model =
    ()


type alias Msg =
    ()


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init =
    always ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ text "Nothing to see here" ]

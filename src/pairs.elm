module Main exposing (main)

import Browser
import Html exposing (Html, div, text)


type alias Model =
    { state : State
    , numPairs : Int
    , cards : List Int
    , matched : List Int
    }


type State
    = InProgress Phase
    | Solved


type Phase
    = Hidden
    | OneRevealed Int
    | TwoRevealed Int Int


type Msg
    = Click Int
    | TimeOut
    | Shuffle (List Int)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init =
    always ( initialModel, shuffleCards initialModel.cards )


initialModel : Model
initialModel =
    let
        numPairs =
            10
    in
        { state = InProgress Hidden
        , numPairs = numPairs
        , cards = List.range 1 (2 * numPairs)
        , matched = []
        }


shuffleCards : List Int -> Cmd Msg
shuffleCards cards =
    Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] [ text "Nothing to see here" ]

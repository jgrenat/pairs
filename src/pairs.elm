module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra as List


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
    | Restart


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
    let
        columns =
            calculateColumns (List.length model.cards)
    in
        div []
            (createHeader model.state
                :: List.map createRow (List.groupsOf columns model.cards)
            )


{-| Try for equal number of rows and columns,
favoring more columns if numCards is not a perfect square
-}
calculateColumns : Int -> Int
calculateColumns numCards =
    Maybe.withDefault numCards
        (List.find
            (\n -> modBy n numCards == 0)
            (List.range
                (numCards
                    |> toFloat
                    |> sqrt
                    |> ceiling
                )
                numCards
            )
        )


createHeader : State -> Html Msg
createHeader state =
    div []
        (case state of
            Solved ->
                [ text "Congratulations!"
                , button [ onClick Restart ] [ text "Play again" ]
                ]

            other ->
                [ text "Click on the cards to reveal them" ]
        )


createRow : List Int -> Html Msg
createRow cards =
    div [] (List.map createButton cards)


createButton : Int -> Html Msg
createButton card =
    button
        [ onClick (Click card) ]
        [ text (String.fromInt card) ]

module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra as List
import Time exposing (every)


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
    ( case msg of
        TimeOut ->
            case model.state of
                InProgress (TwoRevealed _ _) ->
                    { model | state = InProgress Hidden }

                other ->
                    model

        Click card ->
            case model.state of
                InProgress Hidden ->
                    { model | state = InProgress (OneRevealed card) }

                InProgress (OneRevealed card1) ->
                    { model | state = InProgress (TwoRevealed card1 card) }

                other ->
                    model

        other ->
            model
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        InProgress (TwoRevealed _ _) ->
            every 2000 (always TimeOut)

        other ->
            Sub.none


view : Model -> Html Msg
view model =
    let
        columns =
            calculateColumns (List.length model.cards)
    in
        div []
            (createHeader model.state
                :: List.map (createRow model) (List.groupsOf columns model.cards)
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


createRow : Model -> List Int -> Html Msg
createRow model cards =
    div [] (List.map (createButton model) cards)


createButton : Model -> Int -> Html Msg
createButton model card =
    button
        [ onClick (Click card) ]
        [ text (buttonText model card) ]


buttonText : Model -> Int -> String
buttonText model number =
    let
        text =
            String.fromInt (modBy (List.length model.cards // 2) number)
    in
        if List.member number model.matched then
            text
        else
            case model.state of
                InProgress (OneRevealed card1) ->
                    if card1 == number then
                        text
                    else
                        "X"

                InProgress (TwoRevealed card1 card2) ->
                    if List.member number [ card1, card2 ] then
                        text
                    else
                        "X"

                other ->
                    "X"

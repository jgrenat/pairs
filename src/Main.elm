module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra as List
import Random
import Random.List as Random
import Time


type alias Model =
    { state : State
    , numPairs : Int
    , cards : List Card
    , matched : List Card
    }


type State
    = Hidden
    | OneRevealed Card
    | TwoRevealed Card Card
    | Solved


type Card
    = Card Int


type Msg
    = Click Card
    | TimeOut
    | Shuffle (List Card)
    | Restart


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, shuffleCards initialModel.cards )


initialModel : Model
initialModel =
    let
        numPairs : Int
        numPairs =
            10
    in
    { state = Hidden
    , numPairs = numPairs
    , cards =
        List.range 1 (numPairs * 2)
            |> List.map Card
    , matched = []
    }


shuffleCards : List Card -> Cmd Msg
shuffleCards cards =
    Random.generate Shuffle (Random.shuffle cards)


index : Card -> Int
index (Card card) =
    card


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeOut ->
            ( case model.state of
                TwoRevealed _ _ ->
                    { model | state = Hidden }

                _ ->
                    model
            , Cmd.none
            )

        Click card ->
            ( if List.member card model.matched then
                model

              else
                case model.state of
                    Hidden ->
                        { model | state = OneRevealed card }

                    OneRevealed card1 ->
                        revealAnother model card1 card

                    _ ->
                        model
            , Cmd.none
            )

        Restart ->
            ( { model | state = Hidden, matched = [] }
            , shuffleCards model.cards
            )

        Shuffle cards ->
            ( { model | cards = cards }
            , Cmd.none
            )


revealAnother : Model -> Card -> Card -> Model
revealAnother model alreadyRevealed toReveal =
    if toReveal == alreadyRevealed then
        model

    else
        let
            matched : List Card
            matched =
                if matching model.numPairs alreadyRevealed toReveal then
                    alreadyRevealed :: toReveal :: model.matched

                else
                    model.matched
        in
        { model
            | state =
                if List.length matched == model.numPairs * 2 then
                    Solved

                else
                    TwoRevealed alreadyRevealed toReveal
            , matched = matched
        }


matching : Int -> Card -> Card -> Bool
matching numPairs card1 card2 =
    modBy numPairs (index card1) == modBy numPairs (index card2)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        TwoRevealed _ _ ->
            Time.every 2000 (always TimeOut)

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    let
        columns : Int
        columns =
            numColumns (List.length model.cards)
    in
    Html.div []
        (header model
            :: List.map
                (Html.div [] << List.map (cardButton model))
                (List.groupsOf columns model.cards)
        )


{-| Try for equal number of rows and columns,
favoring more columns if numCards is not a perfect square
-}
numColumns : Int -> Int
numColumns numCards =
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


header : Model -> Html Msg
header model =
    Html.div []
        (case model.state of
            Solved ->
                [ Html.text "Congratulations!"
                , Html.button [ Events.onClick Restart ] [ Html.text "Play again" ]
                ]

            TwoRevealed card1 card2 ->
                [ Html.text
                    (if matching model.numPairs card1 card2 then
                        "A match!"

                     else
                        "Not a match, try again"
                    )
                ]

            _ ->
                [ Html.text "Click on the cards to reveal them" ]
        )


cardButton : Model -> Card -> Html Msg
cardButton model card =
    Html.button
        [ Events.onClick (Click card)
        , List.member
            card
            model.matched
            || (case model.state of
                    TwoRevealed _ _ ->
                        True

                    OneRevealed card1 ->
                        card == card1

                    _ ->
                        False
               )
            |> Attrs.disabled
        ]
        [ Html.text (buttonText model card) ]


buttonText : Model -> Card -> String
buttonText model card =
    let
        textRevealed : String
        textRevealed =
            modBy (List.length model.cards // 2) (index card)
                |> String.fromInt

        textHidden : String
        textHidden =
            "X"
    in
    if List.member card model.matched then
        textRevealed

    else
        case model.state of
            OneRevealed card1 ->
                if card == card1 then
                    textRevealed

                else
                    textHidden

            TwoRevealed card1 card2 ->
                if List.member card [ card1, card2 ] then
                    textRevealed

                else
                    textHidden

            _ ->
                textHidden

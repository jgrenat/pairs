module Main exposing (main)

import Animal exposing (Emoji, activeBomb, inactiveBomb)
import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List
import List.Extra as List
import Random
import Random.List as Random
import Time


type alias Model =
    { state : State
    , numPairs : Int
    , cards : Maybe (List Card)
    , matched : List Card
    }


type State
    = Hidden
    | OneRevealed Card
    | TwoRevealed Card Card
    | BombRevealed Instance
    | OneCardAndOneBombRevealed Card Instance
    | Solved
    | Lost Instance


type Card
    = Card Emoji Instance
    | InactiveBomb Instance
    | ActiveBomb Instance


type Instance
    = A
    | B


type Msg
    = Click Card
    | TimeOut
    | NewGame (List Card)
    | Restart Difficulty
    | ActivateBomb


type Difficulty
    = Easy
    | Medium
    | Hard


numPairsEasy : Int
numPairsEasy =
    4


numPairsMedium : Int
numPairsMedium =
    10


numPairsHard : Int
numPairsHard =
    20


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
    ( initialModel, newGame initialModel.numPairs )


initialModel : Model
initialModel =
    let
        numPairs : Int
        numPairs =
            numPairsEasy
    in
    { state = Hidden
    , numPairs = numPairs
    , cards = Nothing
    , matched = []
    }


newGame : Int -> Cmd Msg
newGame numPairs =
    Random.generate
        NewGame
        (Random.shuffle Animal.emojis
            |> Random.andThen
                (\emojis ->
                    createCards emojis numPairs
                        |> Random.shuffle
                )
        )


createCards : List Emoji -> Int -> List Card
createCards emojis numPairs =
    List.take numPairs emojis
        |> List.concatMap (\emoji -> [ Card emoji A, Card emoji B ])
        |> List.append [ InactiveBomb A, InactiveBomb B ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeOut ->
            ( case model.state of
                TwoRevealed _ _ ->
                    { model | state = Hidden }

                Hidden ->
                    model

                OneRevealed _ ->
                    model

                BombRevealed _ ->
                    model

                Solved ->
                    model

                OneCardAndOneBombRevealed _ _ ->
                    model

                Lost instance ->
                    model
            , Cmd.none
            )

        Click card ->
            ( if List.member card model.matched then
                model

              else
                case model.state of
                    Hidden ->
                        case card of
                            ActiveBomb instance ->
                                { model | state = Lost instance }

                            InactiveBomb instance ->
                                { model | state = BombRevealed instance }

                            _ ->
                                { model | state = OneRevealed card }

                    OneRevealed card1 ->
                        case card of
                            ActiveBomb instance ->
                                { model | state = Lost instance }

                            InactiveBomb instance ->
                                { model | state = OneCardAndOneBombRevealed card1 instance }

                            _ ->
                                revealAnother model card1 card

                    BombRevealed _ ->
                        model

                    Solved ->
                        model

                    TwoRevealed _ _ ->
                        model

                    OneCardAndOneBombRevealed _ _ ->
                        model

                    Lost _ ->
                        model
            , Cmd.none
            )

        Restart difficulty ->
            let
                numPairs : Int
                numPairs =
                    case difficulty of
                        Easy ->
                            numPairsEasy

                        Medium ->
                            numPairsMedium

                        Hard ->
                            numPairsHard
            in
            ( { model
                | state = Hidden
                , matched = []
                , cards = Nothing
                , numPairs = numPairs
              }
            , newGame numPairs
            )

        NewGame cards ->
            ( { model | cards = Just cards }
            , Cmd.none
            )

        ActivateBomb ->
            case model.state of
                BombRevealed instance ->
                    ( { model | state = Hidden, cards = Maybe.map (activateBomb instance) model.cards }, Cmd.none )

                Hidden ->
                    ( model, Cmd.none )

                OneRevealed _ ->
                    ( model, Cmd.none )

                TwoRevealed _ _ ->
                    ( model, Cmd.none )

                Solved ->
                    ( model, Cmd.none )

                OneCardAndOneBombRevealed _ instance ->
                    ( { model | state = Hidden, cards = Maybe.map (activateBomb instance) model.cards }, Cmd.none )

                Lost _ ->
                    ( model, Cmd.none )


activateBomb : Instance -> List Card -> List Card
activateBomb instance cards =
    List.map
        (\card ->
            if card == InactiveBomb instance then
                ActiveBomb instance

            else
                card
        )
        cards


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
    case ( card1, card2 ) of
        ( Card index1 _, Card index2 _ ) ->
            index1 == index2

        _ ->
            False


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        TwoRevealed _ _ ->
            Time.every 500 (always TimeOut)

        Hidden ->
            Sub.none

        OneRevealed _ ->
            Sub.none

        BombRevealed _ ->
            Time.every 1000 (always ActivateBomb)

        Solved ->
            Sub.none

        OneCardAndOneBombRevealed _ _ ->
            Time.every 1000 (always ActivateBomb)

        Lost _ ->
            Sub.none


view : Model -> Html Msg
view model =
    case model.cards of
        Just cards ->
            let
                numCards : Int
                numCards =
                    List.length cards

                columns : Int
                columns =
                    numColumns numCards

                rows : Int
                rows =
                    numCards // columns
            in
            Html.div []
                [ header model
                , Html.div
                    (grid rows columns)
                    (List.map
                        (cardView model.matched model.state)
                        cards
                    )
                , case model.state of
                    Lost _ ->
                        Html.p messageStyle [ Html.text "You've lost 😅" ]

                    _ ->
                        Html.text ""
                ]

        Nothing ->
            Html.span messageStyle [ Html.text "Shuffling …" ]


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
    Html.div headerStyle
        (case model.state of
            Solved ->
                [ Html.span messageStyle [ Html.text "Congratulations!" ]
                , playAgainView
                ]

            Lost _ ->
                [ playAgainView
                ]

            TwoRevealed card1 card2 ->
                [ Html.span messageStyle
                    [ Html.text
                        (if matching model.numPairs card1 card2 then
                            "A match!"

                         else
                            "Not a match, try again"
                        )
                    ]
                ]

            _ ->
                [ Html.span messageStyle [ Html.text "Click on the cards to reveal them" ] ]
        )


playAgainView : Html Msg
playAgainView =
    Html.div []
        (Html.span messageStyle [ Html.text "Play again?" ]
            :: List.map
                (\( difficulty, label ) ->
                    Html.button (Events.onClick (Restart difficulty) :: restartButtonStyle) [ Html.text label ]
                )
                [ ( Easy, "Easy" ), ( Medium, "Medium" ), ( Hard, "Hard" ) ]
        )


cardView : List Card -> State -> Card -> Html Msg
cardView matched state card =
    if List.member card matched then
        cardRevealedView card

    else
        case state of
            OneRevealed card1 ->
                if card == card1 then
                    cardRevealedView card

                else
                    cardHiddenView matched state card

            TwoRevealed card1 card2 ->
                if List.member card [ card1, card2 ] then
                    cardRevealedView card

                else
                    cardHiddenView matched state card

            Hidden ->
                cardHiddenView matched state card

            BombRevealed instance ->
                if card == InactiveBomb instance then
                    cardRevealedView card

                else
                    cardHiddenView matched state card

            Solved ->
                cardHiddenView matched state card

            OneCardAndOneBombRevealed card1 instance ->
                if card == InactiveBomb instance || card == card1 then
                    cardRevealedView card

                else
                    cardHiddenView matched state card

            Lost _ ->
                cardRevealedView card


cardRevealedView : Card -> Html Msg
cardRevealedView card =
    Html.span cardStyle
        [ case card of
            Card emoji _ ->
                Animal.toString emoji
                    |> Html.text

            InactiveBomb _ ->
                Animal.toString inactiveBomb
                    |> Html.text

            ActiveBomb _ ->
                Animal.toString activeBomb
                    |> Html.text
        ]


cardHiddenView : List Card -> State -> Card -> Html Msg
cardHiddenView matched state card =
    Html.button
        (Events.onClick (Click card)
            :: (List.member
                    card
                    matched
                    || (case state of
                            TwoRevealed _ _ ->
                                True

                            _ ->
                                False
                       )
                    |> Attrs.disabled
               )
            :: cardStyle
        )
        [ Html.text "❓" ]


grid : Int -> Int -> List (Html.Attribute Msg)
grid rows columns =
    [ Attrs.style "display" "grid"
    , Attrs.style "grid-template-columns" (String.join " " (List.repeat columns "60pt"))
    , Attrs.style "grid-template-rows" (String.join " " (List.repeat rows "60pt"))
    ]


cardStyle : List (Html.Attribute Msg)
cardStyle =
    [ Attrs.style "font-size" "40pt"
    , Attrs.style "margin" "5px"
    , Attrs.style "padding" "2px"
    , Attrs.style "border-radius" "1px"
    ]


restartButtonStyle : List (Html.Attribute Msg)
restartButtonStyle =
    [ Attrs.style "font-size" "20pt"
    , Attrs.style "margin" "5px"
    ]


messageStyle : List (Html.Attribute Msg)
messageStyle =
    [ Attrs.style "font-size" "20pt"
    , Attrs.style "margin" "5px"
    , Attrs.style "padding" "2px"
    ]


headerStyle : List (Html.Attribute Msg)
headerStyle =
    [ Attrs.style "padding" "10px" ]

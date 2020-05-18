module Main exposing (main)

import Animal exposing (Emoji)
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
    , cards : Maybe (List Card)
    , matched : List Card
    }


type State
    = Hidden
    | OneRevealed Card
    | TwoRevealed Card Card
    | Solved


type Card
    = Card Emoji Instance


type Instance
    = A
    | B


type Msg
    = Click Card
    | TimeOut
    | NewGame (List Card)
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
    ( initialModel, newGame initialModel.numPairs )


initialModel : Model
initialModel =
    let
        numPairs : Int
        numPairs =
            10
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
            ( { model | state = Hidden, matched = [], cards = Nothing }
            , newGame model.numPairs
            )

        NewGame cards ->
            ( { model | cards = Just cards }
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
matching numPairs (Card index1 _) (Card index2 _) =
    index1 == index2


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        TwoRevealed _ _ ->
            Time.every 2000 (always TimeOut)

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    case model.cards of
        Just cards ->
            let
                columns : Int
                columns =
                    numColumns (List.length cards)
            in
            Html.div []
                (header model
                    :: List.map
                        (Html.div [] << List.map (cardButton cards model.matched model.state))
                        (List.groupsOf columns cards)
                )

        Nothing ->
            Html.text "Shuffling …"


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


cardButton : List Card -> List Card -> State -> Card -> Html Msg
cardButton cards matched state card =
    Html.button
        [ Events.onClick (Click card)
        , List.member
            card
            matched
            || (case state of
                    TwoRevealed _ _ ->
                        True

                    OneRevealed card1 ->
                        card == card1

                    _ ->
                        False
               )
            |> Attrs.disabled
        ]
        [ Html.text (buttonText cards matched state card) ]


buttonText : List Card -> List Card -> State -> Card -> String
buttonText cards matched state card =
    let
        textRevealed : String
        textRevealed =
            case card of
                Card emoji _ ->
                    Animal.toString emoji

        textHidden : String
        textHidden =
            "X"
    in
    if List.member card matched then
        textRevealed

    else
        case state of
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

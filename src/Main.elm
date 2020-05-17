module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import List.Extra as List
import Random
import Random.List as Random
import Set exposing (Set)
import Time


type alias Model =
    { state : State
    , numPairs : Int
    , cards : List Int
    , matched : Set Int
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
    { state = InProgress Hidden
    , numPairs = numPairs
    , cards = List.range 1 (numPairs * 2)
    , matched = Set.empty
    }


shuffleCards : List Int -> Cmd Msg
shuffleCards cards =
    Random.generate Shuffle (Random.shuffle cards)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeOut ->
            ( case model.state of
                InProgress (TwoRevealed _ _) ->
                    { model | state = InProgress Hidden }

                _ ->
                    model
            , Cmd.none
            )

        Click card ->
            ( if Set.member card model.matched then
                model

              else
                case model.state of
                    InProgress Hidden ->
                        { model | state = InProgress (OneRevealed card) }

                    InProgress (OneRevealed card1) ->
                        revealAnother model card1 card

                    _ ->
                        model
            , Cmd.none
            )

        Restart ->
            ( { model | state = InProgress Hidden, matched = Set.empty }
            , shuffleCards model.cards
            )

        Shuffle cards ->
            ( { model | cards = cards }
            , Cmd.none
            )


revealAnother : Model -> Int -> Int -> Model
revealAnother model alreadyRevealed toReveal =
    if toReveal == alreadyRevealed then
        model

    else
        let
            matched : Set Int
            matched =
                if matching model.numPairs alreadyRevealed toReveal then
                    Set.insert alreadyRevealed model.matched
                        |> Set.insert toReveal

                else
                    model.matched
        in
        { model
            | state =
                if Set.size matched == model.numPairs * 2 then
                    Solved

                else
                    InProgress (TwoRevealed alreadyRevealed toReveal)
            , matched = matched
        }


matching : Int -> Int -> Int -> Bool
matching numPairs card1 card2 =
    modBy numPairs card1 == modBy numPairs card2


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        InProgress (TwoRevealed _ _) ->
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

            InProgress (TwoRevealed card1 card2) ->
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


cardButton : Model -> Int -> Html Msg
cardButton model index =
    Html.button
        [ Events.onClick (Click index)
        , Set.member
            index
            model.matched
            || (case model.state of
                    InProgress (TwoRevealed _ _) ->
                        True

                    InProgress (OneRevealed card) ->
                        index == card

                    _ ->
                        False
               )
            |> Attrs.disabled
        ]
        [ Html.text (buttonText model index) ]


buttonText : Model -> Int -> String
buttonText model index =
    let
        textRevealed : String
        textRevealed =
            modBy (List.length model.cards // 2) index
                |> String.fromInt

        textHidden : String
        textHidden =
            "X"
    in
    if Set.member index model.matched then
        textRevealed

    else
        case model.state of
            InProgress (OneRevealed card) ->
                if card == index then
                    textRevealed

                else
                    textHidden

            InProgress (TwoRevealed card1 card2) ->
                if List.member index [ card1, card2 ] then
                    textRevealed

                else
                    textHidden

            _ ->
                textHidden

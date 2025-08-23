module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (Html, button, div, h1, h2, input, label, span, text)
import Html.Attributes as Attr exposing (class, disabled, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Page exposing (Page)
import Poker.Cards as Cards exposing (Card, Rank(..), Suit(..))
import Poker.Hands as Hands
import Poker.Probability as Probability
import Random
import Route exposing (Route)
import Shared
import View exposing (View)



-- MODEL


type alias Model =
    { playerCount : Int
    , holeCards : List Card
    , communityCards : List Card
    , gameStage : GameStage
    , currentCardEntry : CardEntry
    , foldedPlayers : Int
    , seed : Random.Seed
    , odds : Maybe Probability.PokerOdds
    }


type GameStage
    = SelectingHoleCards
    | ShowingPreFlopOdds
    | SelectingFlop
    | ShowingFlopOdds
    | SelectingTurn
    | ShowingTurnOdds
    | SelectingRiver
    | ShowingFinalOdds


type CardEntry
    = SelectingSuit
    | SelectingRank Suit


type Msg
    = SetPlayerCount String
    | SelectSuit Suit
    | SelectRank Rank
    | NextStage
    | PlayerFolded
    | ResetHand
    | CalculateOdds



-- INIT


init : () -> ( Model, Effect Msg )
init _ =
    ( { playerCount = 4
      , holeCards = []
      , communityCards = []
      , gameStage = SelectingHoleCards
      , currentCardEntry = SelectingSuit
      , foldedPlayers = 0
      , seed = Random.initialSeed 42
      , odds = Nothing
      }
    , Effect.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SetPlayerCount countStr ->
            case String.toInt countStr of
                Just count ->
                    ( { model | playerCount = clamp 2 8 count }, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        SelectSuit suit ->
            ( { model | currentCardEntry = SelectingRank suit }, Effect.none )

        SelectRank rank ->
            case model.currentCardEntry of
                SelectingRank suit ->
                    let
                        newCard =
                            Cards.card rank suit

                        updatedModel =
                            addCardToCurrentStage newCard model
                    in
                    ( { updatedModel | currentCardEntry = SelectingSuit }, Effect.none )

                SelectingSuit ->
                    ( model, Effect.none )

        NextStage ->
            ( advanceGameStage model, Effect.none )

        PlayerFolded ->
            if model.foldedPlayers < model.playerCount - 2 then
                ( { model | foldedPlayers = model.foldedPlayers + 1 }
                    |> calculateCurrentOdds
                , Effect.none
                )

            else
                ( model, Effect.none )

        ResetHand ->
            init ()

        CalculateOdds ->
            ( calculateCurrentOdds model, Effect.none )


addCardToCurrentStage : Card -> Model -> Model
addCardToCurrentStage card model =
    case model.gameStage of
        SelectingHoleCards ->
            let
                newHoleCards =
                    card :: model.holeCards

                newStage =
                    if List.length newHoleCards >= 2 then
                        ShowingPreFlopOdds

                    else
                        SelectingHoleCards
            in
            { model
                | holeCards = newHoleCards
                , gameStage = newStage
            }
                |> calculateCurrentOdds

        SelectingFlop ->
            let
                newCommunityCards =
                    card :: model.communityCards

                newStage =
                    if List.length newCommunityCards >= 3 then
                        ShowingFlopOdds

                    else
                        SelectingFlop
            in
            { model
                | communityCards = newCommunityCards
                , gameStage = newStage
            }
                |> calculateCurrentOdds

        SelectingTurn ->
            { model
                | communityCards = card :: model.communityCards
                , gameStage = ShowingTurnOdds
            }
                |> calculateCurrentOdds

        SelectingRiver ->
            { model
                | communityCards = card :: model.communityCards
                , gameStage = ShowingFinalOdds
            }
                |> calculateCurrentOdds

        _ ->
            model


advanceGameStage : Model -> Model
advanceGameStage model =
    case model.gameStage of
        ShowingPreFlopOdds ->
            { model | gameStage = SelectingFlop }

        ShowingFlopOdds ->
            { model | gameStage = SelectingTurn }

        ShowingTurnOdds ->
            { model | gameStage = SelectingRiver }

        _ ->
            model


calculateCurrentOdds : Model -> Model
calculateCurrentOdds model =
    if List.length model.holeCards == 2 then
        let
            gameState =
                { holeCards = model.holeCards
                , communityCards = model.communityCards
                , numOpponents = model.playerCount - 1
                , numFolded = model.foldedPlayers
                , potSize = Nothing
                , betSize = Nothing
                }

            ( odds, newSeed ) =
                Probability.calculateOdds gameState 1000 model.seed
        in
        { model
            | odds = Just odds
            , seed = newSeed
        }

    else
        model



-- PAGE


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> View Msg
view model =
    { title = "PokerChance - Live Odds Calculator"
    , body =
        [ div [ class "poker-calculator" ]
            [ headerSection
            , playerCountSection model
            , cardEntrySection model
            , oddsSection model
            , actionButtonsSection model
            ]
        ]
    }


headerSection : Html Msg
headerSection =
    div [ class "header" ]
        [ h1 [ class "title" ] [ text "🃏 PokerChance" ]
        , div [ class "subtitle" ] [ text "Live Texas Hold'em Odds Calculator" ]
        ]


playerCountSection : Model -> Html Msg
playerCountSection model =
    div [ class "player-count-section" ]
        [ label [ class "player-count-label" ] [ text "Number of Players" ]
        , div [ class "player-count-controls" ]
            [ input
                [ type_ "range"
                , Attr.min "2"
                , Attr.max "8"
                , step "1"
                , value (String.fromInt model.playerCount)
                , onInput SetPlayerCount
                , class "player-slider"
                ]
                []
            , div [ class "player-count-display" ]
                [ span [ class "player-count-number" ] [ text (String.fromInt model.playerCount) ]
                , span [ class "player-count-text" ] [ text " players" ]
                ]
            ]
        , if model.foldedPlayers > 0 then
            div [ class "folded-info" ]
                [ text (String.fromInt model.foldedPlayers ++ " folded, " ++ String.fromInt (model.playerCount - model.foldedPlayers) ++ " active") ]

          else
            text ""
        ]


cardEntrySection : Model -> Html Msg
cardEntrySection model =
    div [ class "card-entry-section" ]
        [ div [ class "stage-header" ]
            [ h2 [ class "stage-title" ] [ text (getStageTitle model.gameStage) ]
            , div [ class "cards-needed" ] [ text (getCardsNeededText model.gameStage model) ]
            ]
        , currentCardsDisplay model
        , cardSelector model
        ]


getStageTitle : GameStage -> String
getStageTitle stage =
    case stage of
        SelectingHoleCards ->
            "Your Hole Cards"

        ShowingPreFlopOdds ->
            "Pre-Flop"

        SelectingFlop ->
            "The Flop"

        ShowingFlopOdds ->
            "Post-Flop"

        SelectingTurn ->
            "The Turn"

        ShowingTurnOdds ->
            "Post-Turn"

        SelectingRiver ->
            "The River"

        ShowingFinalOdds ->
            "Final Hand"


getCardsNeededText : GameStage -> Model -> String
getCardsNeededText stage model =
    case stage of
        SelectingHoleCards ->
            let
                needed =
                    2 - List.length model.holeCards
            in
            if needed > 0 then
                "Select " ++ String.fromInt needed ++ " more card(s)"

            else
                "Ready!"

        SelectingFlop ->
            let
                needed =
                    3 - List.length model.communityCards
            in
            if needed > 0 then
                "Select " ++ String.fromInt needed ++ " more card(s)"

            else
                "Flop complete!"

        SelectingTurn ->
            "Select the turn card"

        SelectingRiver ->
            "Select the river card"

        _ ->
            ""


currentCardsDisplay : Model -> Html Msg
currentCardsDisplay model =
    div [ class "current-cards" ]
        [ if not (List.isEmpty model.holeCards) then
            div [ class "hole-cards" ]
                [ div [ class "card-section-title" ] [ text "Your Cards:" ]
                , div [ class "cards-row" ] (List.map viewCard model.holeCards)
                ]

          else
            text ""
        , if not (List.isEmpty model.communityCards) then
            div [ class "community-cards" ]
                [ div [ class "card-section-title" ] [ text "Board:" ]
                , div [ class "cards-row" ] (List.map viewCard model.communityCards)
                ]

          else
            text ""
        ]


viewCard : Card -> Html Msg
viewCard card =
    div [ class ("card " ++ getSuitColorClass (Cards.suit card)) ]
        [ span [ class "rank" ] [ text (Cards.rankToString (Cards.rank card)) ]
        , span [ class "suit" ] [ text (Cards.suitToSymbol (Cards.suit card)) ]
        ]


getSuitColorClass : Suit -> String
getSuitColorClass suit =
    case suit of
        Hearts ->
            "red"

        Diamonds ->
            "red"

        Clubs ->
            "black"

        Spades ->
            "black"


cardSelector : Model -> Html Msg
cardSelector model =
    case model.currentCardEntry of
        SelectingSuit ->
            suitSelector

        SelectingRank suit ->
            rankSelector suit model


suitSelector : Html Msg
suitSelector =
    div [ class "card-selector" ]
        [ div [ class "selector-title" ] [ text "Select Suit:" ]
        , div [ class "suit-buttons" ]
            [ suitButton Hearts "♥" "red"
            , suitButton Diamonds "♦" "red"
            , suitButton Clubs "♣" "black"
            , suitButton Spades "♠" "black"
            ]
        ]


suitButton : Suit -> String -> String -> Html Msg
suitButton suit symbol color =
    button
        [ onClick (SelectSuit suit)
        , class ("suit-button " ++ color)
        ]
        [ span [ class "suit-symbol" ] [ text symbol ]
        , span [ class "suit-name" ] [ text (suitToString suit) ]
        ]


suitToString : Suit -> String
suitToString suit =
    case suit of
        Hearts ->
            "Hearts"

        Diamonds ->
            "Diamonds"

        Clubs ->
            "Clubs"

        Spades ->
            "Spades"


rankSelector : Suit -> Model -> Html Msg
rankSelector suit model =
    div [ class "card-selector" ]
        [ div [ class "selector-title" ]
            [ text "Select Rank for "
            , span [ class (getSuitColorClass suit) ] [ text (Cards.suitToSymbol suit) ]
            , text ":"
            ]
        , div [ class "rank-buttons" ]
            (List.map (rankButton suit model) Cards.allRanks)
        ]


rankButton : Suit -> Model -> Rank -> Html Msg
rankButton suit model rank =
    let
        targetCard =
            Cards.card rank suit

        allUsedCards =
            model.holeCards ++ model.communityCards

        isCardTaken =
            List.member targetCard allUsedCards
    in
    button
        [ onClick (SelectRank rank)
        , class ("rank-button " ++ getSuitColorClass suit)
        , disabled isCardTaken
        ]
        [ text (Cards.rankToString rank) ]


oddsSection : Model -> Html Msg
oddsSection model =
    case model.odds of
        Just odds ->
            div [ class "odds-section" ]
                [ div [ class "odds-display" ]
                    [ div [ class "win-percentage" ]
                        [ div [ class ("percentage-circle " ++ getOddsColorClass odds.winProbability) ]
                            [ span [ class "percentage-number" ] [ text (formatPercentage (odds.winProbability * 100)) ]
                            , span [ class "percentage-label" ] [ text "WIN" ]
                            ]
                        ]
                    , div [ class "hand-info" ]
                        [ div [ class "current-hand" ]
                            [ span [ class "hand-label" ] [ text "Current Hand:" ]
                            , span [ class "hand-name" ] [ text (Hands.handRankingToString odds.handStrength) ]
                            ]
                        , if odds.outs > 0 then
                            div [ class "outs-info" ]
                                [ span [ class "outs-label" ] [ text "Outs:" ]
                                , span [ class "outs-count" ] [ text (String.fromInt odds.outs ++ " cards") ]
                                ]

                          else
                            text ""
                        ]
                    ]
                ]

        Nothing ->
            text ""


formatPercentage : Float -> String
formatPercentage pct =
    String.fromInt (round pct) ++ "%"


getOddsColorClass : Float -> String
getOddsColorClass winProb =
    if winProb >= 0.7 then
        "excellent"

    else if winProb >= 0.5 then
        "good"

    else if winProb >= 0.3 then
        "okay"

    else
        "poor"


actionButtonsSection : Model -> Html Msg
actionButtonsSection model =
    div [ class "action-buttons" ]
        [ case model.gameStage of
            ShowingPreFlopOdds ->
                div [ class "stage-actions" ]
                    [ button [ onClick NextStage, class "next-button primary" ] [ text "See Flop" ]
                    , button [ onClick PlayerFolded, class "fold-button secondary", disabled (model.foldedPlayers >= model.playerCount - 2) ]
                        [ text "Player Folds" ]
                    ]

            ShowingFlopOdds ->
                div [ class "stage-actions" ]
                    [ button [ onClick NextStage, class "next-button primary" ] [ text "Turn Card" ]
                    , button [ onClick PlayerFolded, class "fold-button secondary", disabled (model.foldedPlayers >= model.playerCount - 2) ]
                        [ text "Player Folds" ]
                    ]

            ShowingTurnOdds ->
                div [ class "stage-actions" ]
                    [ button [ onClick NextStage, class "next-button primary" ] [ text "River Card" ]
                    , button [ onClick PlayerFolded, class "fold-button secondary", disabled (model.foldedPlayers >= model.playerCount - 2) ]
                        [ text "Player Folds" ]
                    ]

            ShowingFinalOdds ->
                div [ class "stage-actions" ]
                    [ button [ onClick ResetHand, class "reset-button primary" ] [ text "New Hand" ]
                    ]

            _ ->
                text ""
        , button [ onClick ResetHand, class "reset-button tertiary" ] [ text "Reset" ]
        ]

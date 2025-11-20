module Pages.Calculator exposing (Model, Msg, page)

import Array
import Browser.Dom
import Browser.Events
import Effect exposing (Effect)
import Html exposing (Html, button, div, h1, h2, h3, input, label, span, text)
import Html.Attributes as Attr exposing (class, classList, disabled, id, placeholder, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Page exposing (Page)
import Poker.Cards as Cards exposing (Card, Rank(..), Suit(..))
import Poker.Hands as Hands
import Poker.Probability as Probability
import Random
import Route exposing (Route)
import Shared
import Task
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
    , potSize : String
    , betAmount : String
    , bigBlind : String
    , showCardModal : Bool
    , selectedSuit : Maybe Suit
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
    | SetPotSize String
    | SetBetAmount String
    | SetBigBlind String
    | OpenCardModal
    | CloseCardModal
    | KeyPressed String
    | FocusResult (Result Browser.Dom.Error ())



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
      , potSize = ""
      , betAmount = ""
      , bigBlind = "2"
      , showCardModal = False
      , selectedSuit = Nothing
      }
    , Effect.none
    )



-- HELPER FUNCTIONS


foldPlayer : Model -> ( Model, Effect Msg )
foldPlayer model =
    if model.foldedPlayers < model.playerCount - 2 then
        ( { model | foldedPlayers = model.foldedPlayers + 1 }
            |> calculateCurrentOdds
        , Effect.none
        )

    else
        ( model, Effect.none )


resetHand : Model -> ( Model, Effect Msg )
resetHand model =
    let
        ( newModel, effects ) =
            init ()
    in
    ( { newModel | playerCount = model.playerCount, bigBlind = model.bigBlind }, effects )


advanceStage : Model -> ( Model, Effect Msg )
advanceStage model =
    let
        newModel =
            advanceGameStage model

        -- Auto-open modal if advancing to a card selection stage
        shouldOpenModal =
            case newModel.gameStage of
                SelectingFlop ->
                    True

                SelectingTurn ->
                    True

                SelectingRiver ->
                    True

                _ ->
                    False

        modelWithModal =
            if shouldOpenModal then
                { newModel | showCardModal = True, selectedSuit = Nothing }

            else
                newModel
    in
    ( modelWithModal, Effect.none )



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
            advanceStage model

        PlayerFolded ->
            foldPlayer model

        ResetHand ->
            resetHand model

        CalculateOdds ->
            ( calculateCurrentOdds model, Effect.none )

        SetPotSize potStr ->
            ( { model | potSize = potStr }, Effect.none )

        SetBetAmount betStr ->
            ( { model | betAmount = betStr }, Effect.none )

        SetBigBlind blindStr ->
            ( { model | bigBlind = blindStr }, Effect.none )

        OpenCardModal ->
            ( { model | showCardModal = True, selectedSuit = Nothing }, Effect.none )

        CloseCardModal ->
            ( { model | showCardModal = False, selectedSuit = Nothing }, Effect.none )

        KeyPressed key ->
            handleKeyPress key model

        FocusResult _ ->
            -- Ignore focus results (success or failure)
            ( model, Effect.none )


handleKeyPress : String -> Model -> ( Model, Effect Msg )
handleKeyPress key model =
    case key of
        " " ->
            -- Space: Open card modal
            if needsCardSelection model && not model.showCardModal then
                ( { model | showCardModal = True, selectedSuit = Nothing }, Effect.none )

            else
                ( model, Effect.none )

        "Escape" ->
            -- Escape: Close modal
            ( { model | showCardModal = False, selectedSuit = Nothing }, Effect.none )

        "r" ->
            -- R: Reset hand
            resetHand model

        "f" ->
            -- F: Fold a player
            foldPlayer model

        "Enter" ->
            -- Enter: Confirm modal or next stage
            if model.showCardModal then
                ( { model | showCardModal = False }, Effect.none )

            else
                advanceStage model

        _ ->
            -- Handle suit and rank keys
            if model.showCardModal then
                handleCardKey key model

            else
                ( model, Effect.none )


handleCardKey : String -> Model -> ( Model, Effect Msg )
handleCardKey key model =
    case ( model.selectedSuit, key ) of
        -- Suit selection
        ( Nothing, "h" ) ->
            ( { model | selectedSuit = Just Hearts }, Effect.none )

        ( Nothing, "d" ) ->
            ( { model | selectedSuit = Just Diamonds }, Effect.none )

        ( Nothing, "c" ) ->
            ( { model | selectedSuit = Just Clubs }, Effect.none )

        ( Nothing, "s" ) ->
            ( { model | selectedSuit = Just Spades }, Effect.none )

        -- Rank selection (when suit is selected)
        ( Just suit, rankKey ) ->
            case keyToRank rankKey of
                Just rank ->
                    let
                        newCard =
                            Cards.card rank suit

                        updatedModel =
                            addCardToCurrentStage newCard model
                                |> (\m ->
                                        if needsCardSelection m then
                                            { m | selectedSuit = Nothing }
                                            -- Keep modal open, reset suit selection

                                        else
                                            { m | showCardModal = False, selectedSuit = Nothing }
                                    -- Close modal
                                   )

                        focusEffect =
                            if not (needsCardSelection updatedModel) && List.length updatedModel.holeCards >= 2 then
                                -- Focus pot size input after completing card entry (only if we have hole cards)
                                Effect.sendCmd (Task.attempt FocusResult (Browser.Dom.focus "pot-size-input"))

                            else
                                Effect.none
                    in
                    ( updatedModel, focusEffect )

                Nothing ->
                    ( model, Effect.none )

        _ ->
            ( model, Effect.none )


keyToRank : String -> Maybe Rank
keyToRank key =
    case key of
        "2" ->
            Just Two

        "3" ->
            Just Three

        "4" ->
            Just Four

        "5" ->
            Just Five

        "6" ->
            Just Six

        "7" ->
            Just Seven

        "8" ->
            Just Eight

        "9" ->
            Just Nine

        "t" ->
            Just Ten

        "j" ->
            Just Jack

        "q" ->
            Just Queen

        "k" ->
            Just King

        "a" ->
            Just Ace

        _ ->
            Nothing


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
            -- Auto-calculate minimum pot if not manually specified
            bigBlindFloat =
                String.toFloat model.bigBlind |> Maybe.withDefault 2.0

            activePlayers =
                model.playerCount - model.foldedPlayers

            -- Exclude ourselves from pot calculation (we haven't contributed yet)
            otherPlayers =
                activePlayers - 1

            -- Only auto-calculate pot pre-flop (when players haven't seen community cards yet)
            isPreFlop =
                List.isEmpty model.communityCards

            autoPotSize =
                if isPreFlop && otherPlayers > 0 then
                    toFloat otherPlayers * bigBlindFloat

                else
                    0.0

            -- No auto-calculation post-flop
            potSizeFloat =
                case String.toFloat model.potSize of
                    Just manualPot ->
                        if manualPot > 0 then
                            Just manualPot

                        else if isPreFlop then
                            Just autoPotSize

                        else
                            Nothing

                    -- Require manual entry post-flop
                    Nothing ->
                        if isPreFlop then
                            Just autoPotSize

                        else
                            Nothing

            -- Require manual entry post-flop
            betAmountFloat =
                String.toFloat model.betAmount

            gameState =
                { holeCards = model.holeCards
                , communityCards = model.communityCards
                , numOpponents = model.playerCount - 1
                , numFolded = model.foldedPlayers
                , potSize = potSizeFloat
                , betSize = betAmountFloat
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
    Browser.Events.onKeyDown (Decode.map KeyPressed (Decode.field "key" Decode.string))


view : Model -> View Msg
view model =
    { title = "PokerChance - Live Odds Calculator"
    , body =
        [ div [ class "poker-calculator" ]
            [ calculatorView model
            ]
        , if model.showCardModal then
            cardModal model

          else
            text ""
        ]
    }


calculatorView : Model -> Html Msg
calculatorView model =
    let
        hasStartedGame =
            List.length model.holeCards > 0
    in
    div []
        [ headerSection hasStartedGame
        , playerCountSection model hasStartedGame
        , cardEntrySection model
        , potOddsSection model
        , oddsSection model
        , actionButtonsSection model
        ]


headerSection : Bool -> Html Msg
headerSection hasStartedGame =
    div
        [ classList
            [ ( "header", True )
            , ( "header--mobile-hidden", hasStartedGame )
            ]
        ]
        [ h1 [ class "title" ] [ text "🃏 PokerChance" ]
        , div [ class "subtitle" ] [ text "Live Texas Hold'em Odds Calculator" ]
        ]


playerCountSection : Model -> Bool -> Html Msg
playerCountSection model hasStartedGame =
    div
        [ classList
            [ ( "player-count-section", True )
            , ( "player-count-section--mobile-hidden", hasStartedGame )
            ]
        ]
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
        , div [ class "blind-controls" ]
            [ label [ class "blind-label" ] [ text "Big Blind" ]
            , input
                [ type_ "number"
                , value model.bigBlind
                , onInput SetBigBlind
                , class "blind-input"
                , placeholder "2"
                , Attr.min "0.01"
                , step "0.01"
                ]
                []
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
        , if needsCardSelection model then
            button
                [ class "open-card-modal-btn"
                , onClick OpenCardModal
                ]
                [ text "📋 Select Card (Space)" ]

          else
            text ""
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


{-| Determine if card selection interface should be shown
-}
needsCardSelection : Model -> Bool
needsCardSelection model =
    case model.gameStage of
        SelectingHoleCards ->
            List.length model.holeCards < 2

        SelectingFlop ->
            List.length model.communityCards < 3

        SelectingTurn ->
            List.length model.communityCards < 4

        SelectingRiver ->
            List.length model.communityCards < 5

        _ ->
            False


potOddsSection : Model -> Html Msg
potOddsSection model =
    if List.length model.holeCards >= 2 then
        div [ class "pot-odds-section" ]
            [ h3 [ class "pot-odds-title" ] [ text "💰 Pot Odds Calculator" ]
            , div [ class "pot-odds-inputs" ]
                [ div [ class "input-group" ]
                    [ label [ class "input-label" ] [ text "Pot Size" ]
                    , input
                        [ type_ "number"
                        , value model.potSize
                        , onInput SetPotSize
                        , class "pot-input"
                        , id "pot-size-input"
                        , placeholder "100"
                        , Attr.step "0.01"
                        , Attr.min "0"
                        ]
                        []
                    ]
                , div [ class "input-group" ]
                    [ label [ class "input-label" ] [ text "Bet to Call" ]
                    , input
                        [ type_ "number"
                        , value model.betAmount
                        , onInput SetBetAmount
                        , class "bet-input"
                        , placeholder "25"
                        , Attr.step "0.01"
                        , Attr.min "0"
                        ]
                        []
                    ]
                ]
            , potOddsDisplay model
            ]

    else
        text ""


potOddsDisplay : Model -> Html Msg
potOddsDisplay model =
    let
        -- Use same logic as calculateCurrentOdds
        bigBlindFloat =
            String.toFloat model.bigBlind |> Maybe.withDefault 2.0

        activePlayers =
            model.playerCount - model.foldedPlayers

        -- Exclude ourselves from pot calculation (we haven't contributed yet)
        otherPlayers =
            activePlayers - 1

        -- Only auto-calculate pot pre-flop
        isPreFlop =
            List.isEmpty model.communityCards

        autoPotSize =
            if isPreFlop && otherPlayers > 0 then
                toFloat otherPlayers * bigBlindFloat

            else
                0.0

        effectivePotSize =
            case String.toFloat model.potSize of
                Just manualPot ->
                    if manualPot > 0 then
                        manualPot

                    else if isPreFlop then
                        autoPotSize

                    else
                        0.0

                -- No pot size available post-flop
                Nothing ->
                    if isPreFlop then
                        autoPotSize

                    else
                        0.0

        -- No pot size available post-flop
    in
    case ( String.toFloat model.betAmount, model.odds ) of
        ( Just betAmount, Just odds ) ->
            if effectivePotSize > 0 && betAmount > 0 then
                let
                    potOddsRatio =
                        betAmount / (effectivePotSize + betAmount)

                    requiredEquity =
                        potOddsRatio * 100

                    currentEquity =
                        odds.winProbability * 100

                    expectedValue =
                        Probability.expectedValue odds effectivePotSize betAmount

                    isGoodCall =
                        currentEquity >= requiredEquity

                    recommendation =
                        if isGoodCall then
                            "✅ CALL"

                        else
                            "❌ FOLD"

                    evColor =
                        if expectedValue >= 0 then
                            "ev-positive"

                        else
                            "ev-negative"
                in
                div [ class "pot-odds-display" ]
                    [ div [ class "pot-size-info" ]
                        [ text ("Pot: $" ++ String.fromFloat (toFloat (round (effectivePotSize * 100)) / 100))
                        , if effectivePotSize == autoPotSize && isPreFlop then
                            span [ class "auto-pot-indicator" ]
                                [ text (" (Auto: " ++ String.fromInt otherPlayers ++ " other players × $" ++ String.fromFloat bigBlindFloat ++ ")") ]

                          else if effectivePotSize > 0 then
                            text " (Manual)"

                          else
                            text ""
                        ]
                    , div [ class "pot-odds-row" ]
                        [ div [ class "pot-odds-item" ]
                            [ div [ class "pot-odds-label" ] [ text "Pot Odds" ]
                            , div [ class "pot-odds-value" ]
                                [ text (String.fromFloat (toFloat (round (potOddsRatio * 100 * 100)) / 100) ++ "%") ]
                            ]
                        , div [ class "pot-odds-item" ]
                            [ div [ class "pot-odds-label" ] [ text "Required Equity" ]
                            , div [ class "pot-odds-value" ]
                                [ text (String.fromFloat (toFloat (round (requiredEquity * 100)) / 100) ++ "%") ]
                            ]
                        ]
                    , div [ class "pot-odds-row" ]
                        [ div [ class "pot-odds-item" ]
                            [ div [ class "pot-odds-label" ] [ text "Your Equity" ]
                            , div [ class "pot-odds-value" ]
                                [ text (String.fromFloat (toFloat (round (currentEquity * 100)) / 100) ++ "%") ]
                            ]
                        , div [ class ("pot-odds-item " ++ evColor) ]
                            [ div [ class "pot-odds-label" ] [ text "Expected Value" ]
                            , div [ class "pot-odds-value" ]
                                [ text ("$" ++ String.fromFloat (toFloat (round (expectedValue * 100)) / 100)) ]
                            ]
                        ]
                    , div [ class "recommendation" ]
                        [ div [ classList [ ( "recommendation-text", True ), ( "call", isGoodCall ), ( "fold", not isGoodCall ) ] ]
                            [ text recommendation ]
                        ]
                    ]

            else
                div [ class "pot-odds-placeholder" ]
                    [ if isPreFlop then
                        text "Enter bet amount to calculate pot odds (pot auto-calculated)"

                      else
                        text "Enter pot size and bet amount to calculate pot odds"
                    ]

        _ ->
            div [ class "pot-odds-placeholder" ]
                [ if List.isEmpty model.communityCards then
                    text "Enter bet amount to calculate pot odds (pot auto-calculated)"

                  else
                    text "Enter pot size and bet amount to calculate pot odds"
                ]


cardModal : Model -> Html Msg
cardModal model =
    div [ class "modal-overlay", onClick CloseCardModal ]
        [ div [ class "modal-content" ]
            [ div [ class "modal-header" ]
                [ h3 [ class "modal-title" ] [ text "🃏 Select Card" ]
                , button [ class "modal-close", onClick CloseCardModal ] [ text "×" ]
                ]
            , div [ class "modal-body" ]
                [ div [ class "card-selection-step" ]
                    [ div [ class "step-title" ]
                        [ text
                            (if model.selectedSuit == Nothing then
                                "1. Choose Suit:"

                             else
                                "2. Choose Rank:"
                            )
                        ]
                    , if model.selectedSuit == Nothing then
                        suitModalButtons model

                      else
                        rankModalButtons model
                    ]
                , div [ class "keyboard-hints" ]
                    [ div [ class "hint" ] [ text "💡 Keyboard shortcuts:" ]
                    , div [ class "hint-row" ]
                        [ span [ class "hint-key" ] [ text "H D C S" ]
                        , span [ class "hint-desc" ] [ text "Hearts Diamonds Clubs Spades" ]
                        ]
                    , div [ class "hint-row" ]
                        [ span [ class "hint-key" ] [ text "2-9 T J Q K A" ]
                        , span [ class "hint-desc" ] [ text "Card ranks" ]
                        ]
                    , div [ class "hint-row" ]
                        [ span [ class "hint-key" ] [ text "Esc" ]
                        , span [ class "hint-desc" ] [ text "Close modal" ]
                        ]
                    ]
                ]
            ]
        ]


suitModalButtons : Model -> Html Msg
suitModalButtons model =
    div [ class "suit-buttons-modal" ]
        [ suitModalButton Hearts "♥" "red" "H"
        , suitModalButton Diamonds "♦" "red" "D"
        , suitModalButton Clubs "♣" "black" "C"
        , suitModalButton Spades "♠" "black" "S"
        ]


suitModalButton : Suit -> String -> String -> String -> Html Msg
suitModalButton suit symbol color keyHint =
    button
        [ onClick (KeyPressed (String.toLower keyHint))
        , class ("suit-button-modal " ++ color)
        ]
        [ span [ class "suit-symbol-large" ] [ text symbol ]
        , span [ class "suit-name" ] [ text (suitToString suit) ]
        , span [ class "key-hint" ] [ text keyHint ]
        ]


rankModalButtons : Model -> Html Msg
rankModalButtons model =
    div [ class "rank-buttons-modal" ]
        [ div [ class "rank-row" ]
            (List.map (rankModalButton model) [ Two, Three, Four, Five, Six, Seven ])
        , div [ class "rank-row" ]
            (List.map (rankModalButton model) [ Eight, Nine, Ten, Jack, Queen, King, Ace ])
        ]


rankModalButton : Model -> Rank -> Html Msg
rankModalButton model rank =
    let
        targetCard =
            case model.selectedSuit of
                Just suit ->
                    Cards.card rank suit

                Nothing ->
                    Cards.card rank Hearts

        -- fallback
        allUsedCards =
            model.holeCards ++ model.communityCards

        isCardTaken =
            List.member targetCard allUsedCards

        keyHint =
            case rank of
                Ten ->
                    "T"

                Jack ->
                    "J"

                Queen ->
                    "Q"

                King ->
                    "K"

                Ace ->
                    "A"

                _ ->
                    Cards.rankToString rank

        suitColor =
            case model.selectedSuit of
                Just Hearts ->
                    "red"

                Just Diamonds ->
                    "red"

                _ ->
                    "black"
    in
    button
        [ onClick (KeyPressed (String.toLower keyHint))
        , class ("rank-button-modal " ++ suitColor)
        , disabled isCardTaken
        ]
        [ span [ class "rank-symbol" ] [ text (Cards.rankToString rank) ]
        , span [ class "key-hint" ] [ text keyHint ]
        ]


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


viewPlayersVisual : Int -> List (Html Msg)
viewPlayersVisual numPlayers =
    List.range 1 numPlayers
        |> List.map (\playerNum ->
            div [ class "player-visual-item", Attr.title ("Player " ++ String.fromInt playerNum) ]
                [ div [ class "player-card-back" ] []
                , div [ class "player-card-back player-card-back--overlap" ] []
                , span [ class "player-number" ] [ text (String.fromInt playerNum) ]
                ]
           )


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
    div [ class "action-buttons action-buttons-section" ]
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

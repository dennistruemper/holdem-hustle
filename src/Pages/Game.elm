module Pages.Game exposing (Model, Msg, page)

import Array
import Effect exposing (Effect)
import Html exposing (Html, button, div, h1, h2, h3, input, label, span, text)
import Html.Attributes as Attr exposing (class, classList, disabled, id, min, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Page exposing (Page)
import Poker.Cards as Cards exposing (Card, Rank(..), Suit(..))
import Poker.Probability as Probability
import Random
import Route exposing (Route)
import Shared
import View exposing (View)



-- MODEL


type alias Model =
    { holeCards : List Card
    , communityCards : List Card
    , seed : Random.Seed
    , gameStatus : GameStatus
    , gameLife : Int
    , gameRounds : Int
    , gamePlayerCountMin : Int
    , gamePlayerCountMax : Int
    , gameKnownCardsConfigs : List KnownCardsConfig
    , gameCurrentNumPlayers : Int
    , gameActualOdds : Maybe Float
    , gamePlayerGuess : Maybe Int
    , gameGuessSubmitted : Bool
    }


type GameStatus
    = NotStarted
    | InProgress
    | GameOver


type KnownCardsConfig
    = JustHand
    | Flop
    | Turn
    | River


type Msg
    = IncrementMin
    | DecrementMin
    | IncrementMax
    | DecrementMax
    | SetMinSlider String
    | SetMaxSlider String
    | SetKnownCardsConfig KnownCardsConfig
    | StartGame
    | SetPlayerGuess String
    | SubmitGuess
    | NextRound
    | RestartGame



-- INIT


init : () -> ( Model, Effect Msg )
init _ =
    ( { holeCards = []
      , communityCards = []
      , seed = Random.initialSeed 42
      , gameStatus = NotStarted
      , gameLife = 100
      , gameRounds = 0
      , gamePlayerCountMin = 2
      , gamePlayerCountMax = 4
      , gameKnownCardsConfigs = [ Flop ]
      , gameCurrentNumPlayers = 4
      , gameActualOdds = Nothing
      , gamePlayerGuess = Nothing
      , gameGuessSubmitted = False
      }
    , Effect.none
    )



-- HELPER FUNCTIONS


getCommunityCardsNeeded : KnownCardsConfig -> Int
getCommunityCardsNeeded config =
    case config of
        JustHand ->
            0

        Flop ->
            3

        Turn ->
            4

        River ->
            5


generateRandomCardsForGame : Model -> ( Model, Effect Msg )
generateRandomCardsForGame model =
    let
        usedCards =
            []

        availableCards =
            Cards.allCards

        -- Deal hole cards (always 2)
        ( holeCards, seed1 ) =
            dealCardsForGame 2 availableCards model.seed

        remainingCards =
            List.filter (\card -> not (List.member card holeCards)) availableCards

        -- Randomly select one config from the selected configs
        selectedConfigs =
            if List.isEmpty model.gameKnownCardsConfigs then
                [ Flop ]

            else
                model.gameKnownCardsConfigs

        configCount =
            List.length selectedConfigs

        ( randomConfigIndex, seed1a ) =
            Random.step (Random.int 0 (configCount - 1)) seed1

        selectedConfig =
            List.drop randomConfigIndex selectedConfigs
                |> List.head
                |> Maybe.withDefault Flop

        -- Deal community cards based on selected config
        communityCardsNeeded =
            getCommunityCardsNeeded selectedConfig

        ( communityCards, seed2 ) =
            dealCardsForGame communityCardsNeeded remainingCards seed1a

        -- Randomly select player count between min and max
        playerCountRange =
            model.gamePlayerCountMax - model.gamePlayerCountMin + 1

        ( randomOffset, seed3 ) =
            Random.step (Random.int 0 (playerCountRange - 1)) seed2

        randomPlayerCount =
            model.gamePlayerCountMin + randomOffset

        -- Calculate actual odds
        gameState =
            { holeCards = holeCards
            , communityCards = communityCards
            , numOpponents = randomPlayerCount - 1
            , numFolded = 0
            , potSize = Nothing
            , betSize = Nothing
            }

        ( odds, seed4 ) =
            Probability.calculateOdds gameState 1000 seed3

        actualWinProbability =
            odds.winProbability
    in
    ( { model
        | holeCards = holeCards
        , communityCards = communityCards
        , gameCurrentNumPlayers = randomPlayerCount
        , gameActualOdds = Just actualWinProbability
        , gamePlayerGuess = Nothing
        , gameGuessSubmitted = False
        , seed = seed4
      }
    , Effect.none
    )


dealCardsForGame : Int -> List Card -> Random.Seed -> ( List Card, Random.Seed )
dealCardsForGame numCards availableCards seed =
    if numCards <= 0 || List.isEmpty availableCards then
        ( [], seed )

    else
        let
            ( shuffled, newSeed ) =
                shuffleCards availableCards seed
        in
        ( List.take numCards shuffled, newSeed )


shuffleCards : List Card -> Random.Seed -> ( List Card, Random.Seed )
shuffleCards list seed =
    let
        array =
            Array.fromList list

        arrayLength =
            Array.length array

        shuffleStep : Int -> Random.Seed -> Array.Array Card -> ( Array.Array Card, Random.Seed )
        shuffleStep i currentSeed currentArray =
            if i <= 0 then
                ( currentArray, currentSeed )

            else
                let
                    ( randomIndex, newSeed ) =
                        Random.step (Random.int 0 i) currentSeed

                    swappedArray =
                        swapArrayElements i randomIndex currentArray
                in
                shuffleStep (i - 1) newSeed swappedArray

        ( shuffledArray, finalSeed ) =
            shuffleStep (arrayLength - 1) seed array
    in
    ( Array.toList shuffledArray, finalSeed )


swapArrayElements : Int -> Int -> Array.Array Card -> Array.Array Card
swapArrayElements i j array =
    case ( Array.get i array, Array.get j array ) of
        ( Just elementI, Just elementJ ) ->
            array
                |> Array.set i elementJ
                |> Array.set j elementI

        _ ->
            array





-- UPDATE


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        IncrementMin ->
            let
                newMin = clamp 2 8 (model.gamePlayerCountMin + 1)
                adjustedMax = if newMin > model.gamePlayerCountMax then newMin else model.gamePlayerCountMax
            in
            ( { model | gamePlayerCountMin = newMin, gamePlayerCountMax = adjustedMax }, Effect.none )

        DecrementMin ->
            let
                newMin = clamp 2 8 (model.gamePlayerCountMin - 1)
            in
            ( { model | gamePlayerCountMin = newMin }, Effect.none )

        IncrementMax ->
            let
                newMax = clamp 2 8 (model.gamePlayerCountMax + 1)
            in
            ( { model | gamePlayerCountMax = newMax }, Effect.none )

        DecrementMax ->
            let
                newMax = clamp 2 8 (model.gamePlayerCountMax - 1)
                adjustedMin = if newMax < model.gamePlayerCountMin then newMax else model.gamePlayerCountMin
            in
            ( { model | gamePlayerCountMax = newMax, gamePlayerCountMin = adjustedMin }, Effect.none )

        SetMinSlider countStr ->
            case String.toInt countStr of
                Just count ->
                    let
                        clampedCount = clamp 2 8 count
                        adjustedMax = if clampedCount > model.gamePlayerCountMax then clampedCount else model.gamePlayerCountMax
                    in
                    ( { model | gamePlayerCountMin = clampedCount, gamePlayerCountMax = adjustedMax }, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        SetMaxSlider countStr ->
            case String.toInt countStr of
                Just count ->
                    let
                        clampedCount = clamp 2 8 count
                        adjustedMin = if clampedCount < model.gamePlayerCountMin then clampedCount else model.gamePlayerCountMin
                    in
                    ( { model | gamePlayerCountMax = clampedCount, gamePlayerCountMin = adjustedMin }, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        SetKnownCardsConfig config ->
            let
                currentConfigs =
                    model.gameKnownCardsConfigs

                newConfigs =
                    if List.member config currentConfigs then
                        -- Remove if already selected
                        let
                            removed =
                                List.filter ((/=) config) currentConfigs
                        in
                        -- Ensure at least one is selected
                        if List.isEmpty removed then
                            currentConfigs

                        else
                            removed

                    else
                        -- Add if not selected
                        config :: currentConfigs
            in
            ( { model | gameKnownCardsConfigs = newConfigs }, Effect.none )

        StartGame ->
            let
                ( newModel, _ ) =
                    generateRandomCardsForGame
                        { model
                            | gameStatus = InProgress
                            , gameLife = 100
                            , gameRounds = 0
                        }
            in
            ( newModel, Effect.none )

        SetPlayerGuess guessStr ->
            case String.toInt guessStr of
                Just guess ->
                    let
                        clampedGuess =
                            clamp 0 100 guess
                    in
                    ( { model | gamePlayerGuess = Just clampedGuess }, Effect.none )

                Nothing ->
                    if guessStr == "" then
                        ( { model | gamePlayerGuess = Nothing }, Effect.none )

                    else
                        ( model, Effect.none )

        SubmitGuess ->
            case ( model.gamePlayerGuess, model.gameActualOdds ) of
                ( Just guess, Just actualProb ) ->
                    let
                        actualPercent =
                            round (actualProb * 100)

                        difference =
                            abs (actualPercent - guess)

                        newLife =
                            max 0 (model.gameLife - difference)

                        newStatus =
                            if newLife == 0 then
                                GameOver

                            else
                                InProgress
                    in
                    ( { model
                        | gameGuessSubmitted = True
                        , gameLife = newLife
                        , gameStatus = newStatus
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        NextRound ->
            if model.gameLife > 0 then
                let
                    ( newModel, _ ) =
                        generateRandomCardsForGame
                            { model
                                | gameRounds = model.gameRounds + 1
                                , gameGuessSubmitted = False
                            }
                in
                ( newModel, Effect.none )

            else
                ( model, Effect.none )

        RestartGame ->
            let
                ( newModel, _ ) =
                    generateRandomCardsForGame
                        { model
                            | gameStatus = InProgress
                            , gameLife = 100
                            , gameRounds = 0
                            , gameGuessSubmitted = False
                        }
            in
            ( newModel, Effect.none )





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
    { title = "PokerChance - Game Mode"
    , body =
        [ div [ class "poker-game" ]
            [ gameView model
            ]
        ]
    }


gameView : Model -> Html Msg
gameView model =
    div []
        [ gameHeaderSection
        , case model.gameStatus of
            NotStarted ->
                gameSettingsSection model

            InProgress ->
                gamePlayingSection model

            GameOver ->
                gameOverSection model
        ]


gameHeaderSection : Html Msg
gameHeaderSection =
    div [ class "game-header" ]
        [ h1 [ class "game-title" ] [ text "🎮 Poker Odds Game" ]
        , div [ class "game-subtitle" ] [ text "Test your poker probability skills!" ]
        ]


gameSettingsSection : Model -> Html Msg
gameSettingsSection model =
    div [ class "game-settings" ]
        [ h2 [ class "settings-title" ] [ text "Game Settings" ]
        , div [ class "settings-content" ]
            [ div [ class "setting-group" ]
                [ label [ class "setting-label" ] [ text "Player Count Range" ]
                , div [ class "range-controls" ]
                    [ div [ class "range-slider-group" ]
                        [ label [ class "range-slider-label" ] [ text "From:" ]
                        , div [ class "range-slider-container" ]
                            [ button
                                [ class "range-button range-button--decrement"
                                , onClick DecrementMin
                                , disabled (model.gamePlayerCountMin <= 2)
                                ]
                                [ text "−" ]
                            , input
                                [ type_ "range"
                                , Attr.min "2"
                                , Attr.max "8"
                                , value (String.fromInt model.gamePlayerCountMin)
                                , onInput SetMinSlider
                                , class "range-slider"
                                ]
                                []
                            , button
                                [ class "range-button range-button--increment"
                                , onClick IncrementMin
                                , disabled (model.gamePlayerCountMin >= 8 || model.gamePlayerCountMin >= model.gamePlayerCountMax)
                                ]
                                [ text "+" ]
                            , span [ class "range-value" ] [ text (String.fromInt model.gamePlayerCountMin) ]
                            ]
                        ]
                    , div [ class "range-slider-group" ]
                        [ label [ class "range-slider-label" ] [ text "To:" ]
                        , div [ class "range-slider-container" ]
                            [ button
                                [ class "range-button range-button--decrement"
                                , onClick DecrementMax
                                , disabled (model.gamePlayerCountMax <= 2 || model.gamePlayerCountMax <= model.gamePlayerCountMin)
                                ]
                                [ text "−" ]
                            , input
                                [ type_ "range"
                                , Attr.min "2"
                                , Attr.max "8"
                                , value (String.fromInt model.gamePlayerCountMax)
                                , onInput SetMaxSlider
                                , class "range-slider"
                                ]
                                []
                            , button
                                [ class "range-button range-button--increment"
                                , onClick IncrementMax
                                , disabled (model.gamePlayerCountMax >= 8)
                                ]
                                [ text "+" ]
                            , span [ class "range-value" ] [ text (String.fromInt model.gamePlayerCountMax) ]
                            ]
                        ]
                    ]
                , div [ class "range-display" ]
                    [ text
                        ("Players: "
                            ++ String.fromInt model.gamePlayerCountMin
                            ++ " - "
                            ++ String.fromInt model.gamePlayerCountMax
                        )
                    ]
                ]
            , div [ class "setting-group" ]
                [ label [ class "setting-label" ] [ text "Known Cards (select one or more)" ]
                , div [ class "checkbox-group" ]
                    [ checkboxButton "Just Hand (2 cards)" (List.member JustHand model.gameKnownCardsConfigs) (SetKnownCardsConfig JustHand)
                    , checkboxButton "Flop (2 + 3 cards)" (List.member Flop model.gameKnownCardsConfigs) (SetKnownCardsConfig Flop)
                    , checkboxButton "Turn (2 + 4 cards)" (List.member Turn model.gameKnownCardsConfigs) (SetKnownCardsConfig Turn)
                    , checkboxButton "River (2 + 5 cards)" (List.member River model.gameKnownCardsConfigs) (SetKnownCardsConfig River)
                    ]
                ]
            , button [ class "start-game-button", onClick StartGame ] [ text "Start Game" ]
            ]
        ]


checkboxButton : String -> Bool -> Msg -> Html Msg
checkboxButton labelText isChecked msg =
    label [ class "checkbox-button-label" ]
        [ input
            [ type_ "checkbox"
            , Attr.checked isChecked
            , onClick msg
            , class "checkbox-input"
            ]
            []
        , span [ class "checkbox-text" ] [ text labelText ]
        ]


gamePlayingSection : Model -> Html Msg
gamePlayingSection model =
    div [ class "game-playing" ]
        [ div [ class "game-stats" ]
            [ div [ class "stat-item" ]
                [ div [ class "stat-label" ] [ text "Life" ]
                , div [ classList [ ( "stat-value", True ), ( "stat-value--life", True ) ] ]
                    [ text (String.fromInt model.gameLife) ]
                ]
            , div [ class "stat-item" ]
                [ div [ class "stat-label" ] [ text "Round" ]
                , div [ class "stat-value" ] [ text (String.fromInt (model.gameRounds + 1)) ]
                ]
            , div [ class "stat-item stat-item--players" ]
                [ div [ class "stat-label" ] [ text "Other Players" ]
                , div [ class "players-visual" ] (viewPlayersVisual (model.gameCurrentNumPlayers - 1))
                ]
            ]
        , div [ class "game-cards-display" ]
            [ div [ class "card-section-title" ] [ text "Your Hand:" ]
            , div [ class "cards-row" ] (List.map viewCard model.holeCards)
            , if not (List.isEmpty model.communityCards) then
                div []
                    [ div [ class "card-section-title" ] [ text "Board:" ]
                    , div [ class "cards-row" ] (List.map viewCard model.communityCards)
                    ]

              else
                text ""
            ]
        , if not model.gameGuessSubmitted then
            div [ class "guess-section" ]
                [ label [ class "guess-label" ] [ text "What's your win probability? (0-100%)" ]
                , div [ class "guess-input-group" ]
                    [ input
                        [ type_ "number"
                        , Attr.min "0"
                        , Attr.max "100"
                        , value
                            (case model.gamePlayerGuess of
                                Just guess ->
                                    String.fromInt guess

                                Nothing ->
                                    ""
                            )
                        , onInput SetPlayerGuess
                        , class "guess-input"
                        , id "guess-input"
                        ]
                        []
                    , button
                        [ class "submit-guess-button"
                        , onClick SubmitGuess
                        , disabled (model.gamePlayerGuess == Nothing)
                        ]
                        [ text "Submit" ]
                    ]
                ]

          else
            gameResultsSection model
        ]


gameResultsSection : Model -> Html Msg
gameResultsSection model =
    case ( model.gamePlayerGuess, model.gameActualOdds ) of
        ( Just guess, Just actualProb ) ->
            let
                actualPercent =
                    round (actualProb * 100)

                difference =
                    abs (actualPercent - guess)

                lifeLost =
                    difference
            in
            div [ class "game-results" ]
                [ h3 [ class "results-title" ] [ text "Results" ]
                , div [ class "results-content" ]
                    [ div [ class "result-item" ]
                        [ div [ class "result-label" ] [ text "Actual Probability:" ]
                        , div [ class "result-value" ] [ text (String.fromInt actualPercent ++ "%") ]
                        ]
                    , div [ class "result-item" ]
                        [ div [ class "result-label" ] [ text "Your Guess:" ]
                        , div [ class "result-value" ] [ text (String.fromInt guess ++ "%") ]
                        ]
                    , div [ class "result-item" ]
                        [ div [ class "result-label" ] [ text "Difference:" ]
                        , div [ class "result-value result-value--loss" ]
                            [ text ("-" ++ String.fromInt lifeLost ++ "% life") ]
                        ]
                    , div [ class "result-item" ]
                        [ div [ class "result-label" ] [ text "Remaining Life:" ]
                        , div [ classList [ ( "result-value", True ), ( "result-value--life", True ) ] ]
                            [ text (String.fromInt model.gameLife) ]
                        ]
                    ]
                , if model.gameLife > 0 then
                    button [ class "next-round-button", onClick NextRound ] [ text "Next Round" ]

                  else
                    text ""
                ]

        _ ->
            text ""


gameOverSection : Model -> Html Msg
gameOverSection model =
    div [ class "game-over" ]
        [ h2 [ class "game-over-title" ] [ text "Game Over!" ]
        , div [ class "game-over-stats" ]
            [ div [ class "final-stat" ]
                [ div [ class "final-stat-label" ] [ text "Rounds Survived" ]
                , div [ class "final-stat-value" ] [ text (String.fromInt model.gameRounds) ]
                ]
            ]
        , button [ class "restart-game-button", onClick RestartGame ] [ text "Play Again" ]
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



module Components.OddsDisplay exposing
    ( Config, defaultConfig
    , view
    )

{-| Odds display component for showing poker probability results.


# Configuration

@docs Config, defaultConfig


# View

@docs view

-}

import Html exposing (Html, div, h3, span, text)
import Html.Attributes exposing (class, classList, style, title)
import Poker.Hands as Hands exposing (HandRanking)
import Poker.Probability exposing (PokerOdds)



-- TYPES


{-| Configuration for the odds display
-}
type alias Config =
    { showPercentages : Bool
    , showOuts : Bool
    , showHandStrength : Bool
    , showConfidence : Bool
    , compact : Bool
    }


{-| Default configuration for odds display
-}
defaultConfig : Config
defaultConfig =
    { showPercentages = True
    , showOuts = True
    , showHandStrength = True
    , showConfidence = True
    , compact = False
    }



-- VIEW


{-| Render the odds display component
-}
view : Config -> Maybe PokerOdds -> Html msg
view config maybeOdds =
    case maybeOdds of
        Just odds ->
            viewOdds config odds

        Nothing ->
            viewPlaceholder config


{-| Render odds information
-}
viewOdds : Config -> PokerOdds -> Html msg
viewOdds config odds =
    div [ class "odds-display" ]
        [ if config.compact then
            viewCompactOdds config odds

          else
            viewDetailedOdds config odds
        ]


{-| Render compact odds view
-}
viewCompactOdds : Config -> PokerOdds -> Html msg
viewCompactOdds config odds =
    div [ class "odds-display--compact" ]
        [ viewWinProbabilityGauge odds
        , if config.showHandStrength then
            viewHandStrengthCompact odds.handStrength

          else
            text ""
        ]


{-| Render detailed odds view
-}
viewDetailedOdds : Config -> PokerOdds -> Html msg
viewDetailedOdds config odds =
    div [ class "odds-display--detailed" ]
        [ viewWinProbabilitySection config odds
        , if config.showHandStrength then
            viewHandStrengthSection odds.handStrength

          else
            text ""
        , if config.showOuts then
            viewOutsSection odds.outs

          else
            text ""
        , if config.showConfidence then
            viewConfidenceSection odds.confidence

          else
            text ""
        ]


{-| Render win probability gauge
-}
viewWinProbabilityGauge : PokerOdds -> Html msg
viewWinProbabilityGauge odds =
    let
        percentage =
            odds.winProbability * 100

        color =
            getOddsColor percentage

        gaugeStyle =
            style "background" ("conic-gradient(" ++ color ++ " " ++ String.fromFloat (percentage * 3.6) ++ "deg, #e0e0e0 0deg)")
    in
    div [ class "odds-display__gauge" ]
        [ div
            [ class "odds-display__gauge-circle"
            , gaugeStyle
            ]
            [ div [ class "odds-display__gauge-inner" ]
                [ div [ class "odds-display__percentage", style "color" color ]
                    [ text (formatPercentage percentage) ]
                , div [ class "odds-display__label" ]
                    [ text "Win %" ]
                ]
            ]
        ]


{-| Render win probability section
-}
viewWinProbabilitySection : Config -> PokerOdds -> Html msg
viewWinProbabilitySection config odds =
    div [ class "odds-display__section" ]
        [ h3 [ class "odds-display__section-title" ]
            [ text "Winning Odds" ]
        , div [ class "odds-display__probabilities" ]
            [ viewProbabilityBar "Win" odds.winProbability "#4CAF50"
            , viewProbabilityBar "Lose" odds.lossProbability "#F44336"
            , if odds.tieProbability > 0 then
                viewProbabilityBar "Tie" odds.tieProbability "#FF9800"

              else
                text ""
            ]
        ]


{-| Render probability bar
-}
viewProbabilityBar : String -> Float -> String -> Html msg
viewProbabilityBar label probability color =
    let
        percentage =
            probability * 100

        barStyle =
            style "width" (String.fromFloat percentage ++ "%")
    in
    div [ class "odds-display__probability-row" ]
        [ span [ class "odds-display__probability-label" ]
            [ text label ]
        , div [ class "odds-display__probability-bar-container" ]
            [ div
                [ class "odds-display__probability-bar"
                , style "background-color" color
                , barStyle
                ]
                []
            ]
        , span [ class "odds-display__probability-value" ]
            [ text (formatPercentage percentage) ]
        ]


{-| Render hand strength section
-}
viewHandStrengthSection : HandRanking -> Html msg
viewHandStrengthSection handRanking =
    div [ class "odds-display__section" ]
        [ h3 [ class "odds-display__section-title" ]
            [ text "Current Hand" ]
        , div [ class "odds-display__hand-strength" ]
            [ span
                [ class "odds-display__hand-ranking"
                , classList [ ( getHandStrengthClass handRanking, True ) ]
                ]
                [ text (Hands.handRankingToString handRanking) ]
            ]
        ]


{-| Render compact hand strength
-}
viewHandStrengthCompact : HandRanking -> Html msg
viewHandStrengthCompact handRanking =
    div [ class "odds-display__hand-strength--compact" ]
        [ span
            [ class "odds-display__hand-ranking--compact"
            , classList [ ( getHandStrengthClass handRanking, True ) ]
            ]
            [ text (Hands.handRankingToString handRanking) ]
        ]


{-| Render outs section
-}
viewOutsSection : Int -> Html msg
viewOutsSection outs =
    div [ class "odds-display__section" ]
        [ h3 [ class "odds-display__section-title" ]
            [ text "Outs" ]
        , div [ class "odds-display__outs" ]
            [ span [ class "odds-display__outs-count" ]
                [ text (String.fromInt outs) ]
            , span [ class "odds-display__outs-label" ]
                [ text
                    (if outs == 1 then
                        " card"

                     else
                        " cards"
                    )
                ]
            , span [ class "odds-display__outs-description" ]
                [ text " can improve your hand" ]
            ]
        ]


{-| Render confidence section
-}
viewConfidenceSection : Float -> Html msg
viewConfidenceSection confidence =
    let
        percentage =
            confidence * 100
    in
    div [ class "odds-display__section odds-display__section--small" ]
        [ span [ class "odds-display__confidence" ]
            [ text ("Confidence: " ++ formatPercentage percentage) ]
        ]


{-| Render placeholder when no odds available
-}
viewPlaceholder : Config -> Html msg
viewPlaceholder config =
    div [ class "odds-display odds-display--placeholder" ]
        [ div [ class "odds-display__placeholder-content" ]
            [ text "Select your cards to see odds" ]
        ]



-- UTILITIES


{-| Format percentage with appropriate decimal places
-}
formatPercentage : Float -> String
formatPercentage percentage =
    if percentage < 0.1 then
        "<0.1%"

    else if percentage > 99.9 then
        ">99.9%"

    else if percentage < 10 then
        String.fromFloat (toFloat (round (percentage * 10)) / 10) ++ "%"

    else
        String.fromInt (round percentage) ++ "%"


{-| Get color based on odds percentage
-}
getOddsColor : Float -> String
getOddsColor percentage =
    if percentage >= 70 then
        "#4CAF50"

    else if percentage >= 50 then
        "#8BC34A"

    else if percentage >= 30 then
        "#FF9800"

    else if percentage >= 15 then
        "#FF5722"

    else
        "#F44336"


{-| Get CSS class for hand strength
-}
getHandStrengthClass : HandRanking -> String
getHandStrengthClass handRanking =
    case handRanking of
        Hands.StraightFlush _ ->
            "odds-display__hand--premium"

        Hands.FourOfAKind _ _ ->
            "odds-display__hand--premium"

        Hands.FullHouse _ _ ->
            "odds-display__hand--strong"

        Hands.Flush _ ->
            "odds-display__hand--strong"

        Hands.Straight _ ->
            "odds-display__hand--good"

        Hands.ThreeOfAKind _ _ ->
            "odds-display__hand--good"

        Hands.TwoPair _ _ _ ->
            "odds-display__hand--medium"

        Hands.OnePair _ _ ->
            "odds-display__hand--weak"

        Hands.HighCard _ ->
            "odds-display__hand--weak"

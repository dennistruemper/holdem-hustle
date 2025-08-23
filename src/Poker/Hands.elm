module Poker.Hands exposing
    ( Hand, HandRanking(..)
    , evaluateHand, handRanking, bestFiveCardHand
    , compareHands
    , handRankingToString
    , handRankingToInt
    , isFlush, isStraight, groupByRank, sortByRank
    )

{-| This module provides hand evaluation logic for Texas Hold'em poker.


# Types

@docs Hand, HandRanking


# Hand Evaluation

@docs evaluateHand, handRanking, bestFiveCardHand


# Comparison

@docs compareHands


# String Conversion

@docs handRankingToString


# Numeric Conversion

@docs handRankingToInt


# Hand Analysis Utilities

@docs isFlush, isStraight, groupByRank, sortByRank

-}

import Dict exposing (Dict)
import Poker.Cards as Cards exposing (Card, Rank(..), Suit(..))



-- TYPES


{-| Represents a poker hand (up to 7 cards for Texas Hold'em)
-}
type alias Hand =
    List Card


{-| Poker hand rankings from highest to lowest
-}
type HandRanking
    = StraightFlush (List Rank)
    | FourOfAKind Rank Rank
    | FullHouse Rank Rank
    | Flush (List Rank)
    | Straight (List Rank)
    | ThreeOfAKind Rank (List Rank)
    | TwoPair Rank Rank Rank
    | OnePair Rank (List Rank)
    | HighCard (List Rank)



-- HAND EVALUATION


{-| Evaluate a hand and return the best possible 5-card hand ranking
-}
evaluateHand : Hand -> HandRanking
evaluateHand cards =
    cards
        |> bestFiveCardHand
        |> handRanking


{-| Determine the ranking of exactly 5 cards
-}
handRanking : List Card -> HandRanking
handRanking fiveCards =
    let
        ranks =
            List.map Cards.rank fiveCards
                |> List.sortWith Cards.compareRanks
                |> List.reverse

        suits =
            List.map Cards.suit fiveCards

        rankGroups =
            groupByRank fiveCards

        isFlushHand =
            isFlush fiveCards

        isStraightHand =
            isStraight ranks
    in
    case ( isFlushHand, isStraightHand ) of
        ( True, True ) ->
            StraightFlush ranks

        ( False, False ) ->
            case List.map List.length (Dict.values rankGroups) |> List.sortBy negate of
                [ 4, 1 ] ->
                    -- Four of a kind
                    let
                        fourKind =
                            getRankFromGroups rankGroups 4

                        kicker =
                            getRankFromGroups rankGroups 1
                    in
                    FourOfAKind fourKind kicker

                [ 3, 2 ] ->
                    -- Full house
                    let
                        threeKind =
                            getRankFromGroups rankGroups 3

                        pair =
                            getRankFromGroups rankGroups 2
                    in
                    FullHouse threeKind pair

                [ 3, 1, 1 ] ->
                    -- Three of a kind
                    let
                        threeKind =
                            getRankFromGroups rankGroups 3

                        kickers =
                            getRanksFromGroups rankGroups 1
                    in
                    ThreeOfAKind threeKind kickers

                [ 2, 2, 1 ] ->
                    -- Two pair
                    let
                        pairs =
                            getRanksFromGroups rankGroups 2

                        kicker =
                            getRankFromGroups rankGroups 1
                    in
                    case pairs of
                        [ high, low ] ->
                            TwoPair high low kicker

                        _ ->
                            HighCard ranks

                [ 2, 1, 1, 1 ] ->
                    -- One pair
                    let
                        pairRank =
                            getRankFromGroups rankGroups 2

                        kickers =
                            getRanksFromGroups rankGroups 1
                    in
                    OnePair pairRank kickers

                _ ->
                    HighCard ranks

        ( True, False ) ->
            Flush ranks

        ( False, True ) ->
            Straight ranks


{-| Find the best 5-card hand from up to 7 cards
-}
bestFiveCardHand : Hand -> List Card
bestFiveCardHand cards =
    if List.length cards <= 5 then
        cards

    else
        -- Generate all possible 5-card combinations and pick the best
        let
            allCombinations =
                combinations 5 cards

            rankedCombinations =
                List.map (\combo -> ( combo, handRanking combo )) allCombinations

            bestCombo =
                rankedCombinations
                    |> maximumBy (\( _, ranking ) -> handRankingToInt ranking)
                    |> Maybe.map Tuple.first
                    |> Maybe.withDefault (List.take 5 cards)
        in
        bestCombo


{-| Extract the cards that form a specific hand ranking
-}
extractCardsFromRanking : List Card -> HandRanking -> List Card
extractCardsFromRanking cards ranking =
    -- For now, return the first 5 cards (this is a simplified implementation)
    -- In a full implementation, we'd extract the specific cards that form the ranking
    List.take 5 cards



-- COMPARISON


{-| Compare two hands to determine which is better
-}
compareHands : Hand -> Hand -> Order
compareHands hand1 hand2 =
    let
        ranking1 =
            evaluateHand hand1

        ranking2 =
            evaluateHand hand2
    in
    compare (handRankingToInt ranking1) (handRankingToInt ranking2)



-- STRING CONVERSION


{-| Convert hand ranking to human-readable string
-}
handRankingToString : HandRanking -> String
handRankingToString ranking =
    case ranking of
        StraightFlush _ ->
            "Straight Flush"

        FourOfAKind rank _ ->
            "Four of a Kind (" ++ Cards.rankToString rank ++ "s)"

        FullHouse threeRank pairRank ->
            "Full House (" ++ Cards.rankToString threeRank ++ "s over " ++ Cards.rankToString pairRank ++ "s)"

        Flush _ ->
            "Flush"

        Straight _ ->
            "Straight"

        ThreeOfAKind rank _ ->
            "Three of a Kind (" ++ Cards.rankToString rank ++ "s)"

        TwoPair high low _ ->
            "Two Pair (" ++ Cards.rankToString high ++ "s and " ++ Cards.rankToString low ++ "s)"

        OnePair rank _ ->
            "One Pair (" ++ Cards.rankToString rank ++ "s)"

        HighCard ranks ->
            case ranks of
                topRank :: _ ->
                    "High Card (" ++ Cards.rankToString topRank ++ " high)"

                [] ->
                    "High Card"



-- NUMERIC CONVERSION


{-| Convert hand ranking to integer for comparison (higher = better)
-}
handRankingToInt : HandRanking -> Int
handRankingToInt ranking =
    case ranking of
        StraightFlush ranks ->
            800000000 + rankListToInt ranks

        FourOfAKind rank kicker ->
            700000000 + (Cards.rankToInt rank * 100) + Cards.rankToInt kicker

        FullHouse threeRank pairRank ->
            600000000 + (Cards.rankToInt threeRank * 100) + Cards.rankToInt pairRank

        Flush ranks ->
            500000000 + rankListToInt ranks

        Straight ranks ->
            400000000 + rankListToInt ranks

        ThreeOfAKind rank kickers ->
            300000000 + (Cards.rankToInt rank * 10000) + rankListToInt kickers

        TwoPair high low kicker ->
            200000000 + (Cards.rankToInt high * 10000) + (Cards.rankToInt low * 100) + Cards.rankToInt kicker

        OnePair rank kickers ->
            100000000 + (Cards.rankToInt rank * 1000000) + rankListToInt kickers

        HighCard ranks ->
            rankListToInt ranks


{-| Convert a list of ranks to an integer for comparison
-}
rankListToInt : List Rank -> Int
rankListToInt ranks =
    let
        sortedRanks =
            List.sortWith Cards.compareRanks ranks
    in
    case sortedRanks of
        [ Two, Three, Four, Five, Ace ] ->
            -- Special case: wheel straight (A-2-3-4-5) should be valued as 5-high
            [ Two, Three, Four, Five, Five ]
                |> List.indexedMap (\i rank -> Cards.rankToInt rank * (15 ^ (4 - i)))
                |> List.sum

        _ ->
            ranks
                |> List.take 5
                |> List.indexedMap (\i rank -> Cards.rankToInt rank * (15 ^ (4 - i)))
                |> List.sum



-- HAND ANALYSIS UTILITIES


{-| Check if 5 cards form a flush
-}
isFlush : List Card -> Bool
isFlush cards =
    case cards of
        first :: rest ->
            let
                firstSuit =
                    Cards.suit first
            in
            List.all (\card -> Cards.suit card == firstSuit) rest

        [] ->
            False


{-| Check if ranks form a straight
-}
isStraight : List Rank -> Bool
isStraight ranks =
    case List.sortWith Cards.compareRanks ranks of
        [ Two, Three, Four, Five, Ace ] ->
            -- Special case: A-2-3-4-5 straight (wheel)
            True

        sortedRanks ->
            let
                rankInts =
                    List.map Cards.rankToInt sortedRanks
            in
            case rankInts of
                first :: rest ->
                    List.foldl
                        (\current ( expected, isValid ) ->
                            ( expected + 1, isValid && current == expected )
                        )
                        ( first + 1, True )
                        rest
                        |> Tuple.second

                [] ->
                    False


{-| Group cards by rank
-}
groupByRank : List Card -> Dict Int (List Card)
groupByRank cards =
    List.foldl
        (\card acc ->
            let
                cardRankInt =
                    Cards.rank card |> Cards.rankToInt
            in
            Dict.update cardRankInt
                (\maybeCards ->
                    case maybeCards of
                        Just existingCards ->
                            Just (card :: existingCards)

                        Nothing ->
                            Just [ card ]
                )
                acc
        )
        Dict.empty
        cards


{-| Sort cards by rank (highest first)
-}
sortByRank : List Card -> List Card
sortByRank cards =
    List.sortWith (\c1 c2 -> Cards.compareRanks (Cards.rank c2) (Cards.rank c1)) cards



-- UTILITY FUNCTIONS


{-| Convert rank integer back to Rank type
-}
intToRank : Int -> Rank
intToRank int =
    case int of
        2 ->
            Two

        3 ->
            Three

        4 ->
            Four

        5 ->
            Five

        6 ->
            Six

        7 ->
            Seven

        8 ->
            Eight

        9 ->
            Nine

        10 ->
            Ten

        11 ->
            Jack

        12 ->
            Queen

        13 ->
            King

        14 ->
            Ace

        _ ->
            Two


{-| Get rank from rank groups by integer key
-}
getRankFromGroups : Dict Int (List Card) -> Int -> Rank
getRankFromGroups groups targetCount =
    groups
        |> Dict.toList
        |> List.filter (\( _, cards ) -> List.length cards == targetCount)
        |> List.head
        |> Maybe.map (Tuple.first >> intToRank)
        |> Maybe.withDefault Two


{-| Get multiple ranks from rank groups by integer key, sorted by rank
-}
getRanksFromGroups : Dict Int (List Card) -> Int -> List Rank
getRanksFromGroups groups targetCount =
    groups
        |> Dict.toList
        |> List.filter (\( _, cards ) -> List.length cards == targetCount)
        |> List.map (Tuple.first >> intToRank)
        |> List.sortWith Cards.compareRanks
        |> List.reverse


{-| Generate all combinations of k elements from a list
-}
combinations : Int -> List a -> List (List a)
combinations k list =
    if k <= 0 then
        [ [] ]

    else
        case list of
            [] ->
                []

            head :: tail ->
                let
                    withHead =
                        combinations (k - 1) tail
                            |> List.map ((::) head)

                    withoutHead =
                        combinations k tail
                in
                withHead ++ withoutHead


{-| Find the maximum element by a comparison function
-}
maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy toComparable list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            let
                folder item currentBest =
                    if toComparable item > toComparable currentBest then
                        item

                    else
                        currentBest
            in
            Just (List.foldl folder first rest)

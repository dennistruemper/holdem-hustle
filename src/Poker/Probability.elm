module Poker.Probability exposing
    ( PokerOdds, GameState
    , calculateOdds, calculateWinProbability
    , monteCarloSimulation
    , countOuts, potOdds, expectedValue
    )

{-| This module provides probability calculations for Texas Hold'em poker.


# Types

@docs PokerOdds, GameState


# Main Calculations

@docs calculateOdds, calculateWinProbability


# Simulation

@docs monteCarloSimulation


# Analysis

@docs countOuts, potOdds, expectedValue

-}

import Array
import Poker.Cards as Cards exposing (Card)
import Poker.Hands as Hands exposing (Hand, HandRanking)
import Random exposing (Generator, Seed)



-- TYPES


{-| Results of poker odds calculation
-}
type alias PokerOdds =
    { winProbability : Float
    , tieProbability : Float
    , lossProbability : Float
    , handStrength : HandRanking
    , outs : Int
    , confidence : Float
    }


{-| Current state of the poker game
-}
type alias GameState =
    { holeCards : List Card
    , communityCards : List Card
    , numOpponents : Int
    , numFolded : Int
    , potSize : Maybe Float
    , betSize : Maybe Float
    }



-- MAIN CALCULATIONS


{-| Calculate comprehensive odds for a given game state
-}
calculateOdds : GameState -> Int -> Seed -> ( PokerOdds, Seed )
calculateOdds gameState iterations seed =
    let
        ( winProb, newSeed ) =
            calculateWinProbability gameState iterations seed

        currentHand =
            gameState.holeCards ++ gameState.communityCards

        handStrength =
            if List.length currentHand >= 5 then
                Hands.evaluateHand currentHand

            else
                Hands.HighCard
                    (List.map Cards.rank gameState.holeCards
                        |> List.sortWith Cards.compareRanks
                        |> List.reverse
                    )

        outs =
            countOuts gameState

        confidence =
            calculateConfidence iterations
    in
    ( { winProbability = winProb
      , tieProbability = 0.0 -- Simplified for now
      , lossProbability = 1.0 - winProb
      , handStrength = handStrength
      , outs = outs
      , confidence = confidence
      }
    , newSeed
    )


{-| Calculate win probability using Monte Carlo simulation
-}
calculateWinProbability : GameState -> Int -> Seed -> ( Float, Seed )
calculateWinProbability gameState iterations seed =
    monteCarloSimulation gameState iterations seed


{-| Run Monte Carlo simulation to estimate win probability
-}
monteCarloSimulation : GameState -> Int -> Seed -> ( Float, Seed )
monteCarloSimulation gameState iterations seed =
    let
        usedCards =
            gameState.holeCards ++ gameState.communityCards

        availableCards =
            List.filter (\card -> not (List.member card usedCards)) Cards.allCards

        communityCardsNeeded =
            5 - List.length gameState.communityCards

        activeOpponents =
            gameState.numOpponents - gameState.numFolded

        simulateGame : Seed -> ( Bool, Seed )
        simulateGame currentSeed =
            let
                -- Deal remaining community cards
                ( communityCards, seed1 ) =
                    dealCards communityCardsNeeded availableCards currentSeed

                finalCommunityCards =
                    gameState.communityCards ++ communityCards

                playerHand =
                    gameState.holeCards ++ finalCommunityCards

                playerRanking =
                    Hands.evaluateHand playerHand

                -- Simulate opponent hands
                remainingCards =
                    List.filter (\card -> not (List.member card playerHand)) availableCards

                ( opponentWins, seed2 ) =
                    simulateOpponents activeOpponents remainingCards finalCommunityCards gameState.holeCards seed1
            in
            ( opponentWins == 0, seed2 )

        runSimulations : Int -> Seed -> Int -> ( Int, Seed )
        runSimulations remaining currentSeed wins =
            if remaining <= 0 then
                ( wins, currentSeed )

            else
                let
                    ( playerWon, newSeed ) =
                        simulateGame currentSeed

                    newWins =
                        if playerWon then
                            wins + 1

                        else
                            wins
                in
                runSimulations (remaining - 1) newSeed newWins

        ( totalWins, finalSeed ) =
            runSimulations iterations seed 0

        winProbability =
            toFloat totalWins / toFloat iterations
    in
    ( winProbability, finalSeed )


{-| Simulate opponent hands and count how many beat the player
-}
simulateOpponents : Int -> List Card -> List Card -> List Card -> Seed -> ( Int, Seed )
simulateOpponents numOpponents availableCards communityCards playerHoleCards seed =
    let
        simulateOpponent : Seed -> ( HandRanking, Seed )
        simulateOpponent currentSeed =
            let
                ( opponentHole, newSeed ) =
                    dealCards 2 availableCards currentSeed

                opponentHand =
                    opponentHole ++ communityCards

                opponentRanking =
                    Hands.evaluateHand opponentHand
            in
            ( opponentRanking, newSeed )

        playerHand =
            playerHoleCards ++ communityCards

        playerRanking =
            Hands.evaluateHand playerHand

        simulateAllOpponents : Int -> Seed -> Int -> ( Int, Seed )
        simulateAllOpponents remaining currentSeed betterHands =
            if remaining <= 0 then
                ( betterHands, currentSeed )

            else
                let
                    ( opponentRanking, newSeed ) =
                        simulateOpponent currentSeed

                    newBetterHands =
                        if Hands.handRankingToInt opponentRanking > Hands.handRankingToInt playerRanking then
                            betterHands + 1

                        else
                            betterHands
                in
                simulateAllOpponents (remaining - 1) newSeed newBetterHands
    in
    simulateAllOpponents numOpponents seed 0



-- ANALYSIS


{-| Count the number of "outs" (cards that improve the hand)
-}
countOuts : GameState -> Int
countOuts gameState =
    let
        currentHand =
            gameState.holeCards ++ gameState.communityCards

        currentRanking =
            if List.length currentHand >= 5 then
                Hands.evaluateHand currentHand

            else
                Hands.HighCard
                    (List.map Cards.rank gameState.holeCards
                        |> List.sortWith Cards.compareRanks
                        |> List.reverse
                    )

        usedCards =
            gameState.holeCards ++ gameState.communityCards

        availableCards =
            List.filter (\card -> not (List.member card usedCards)) Cards.allCards

        communityCardsNeeded =
            5 - List.length gameState.communityCards

        -- Count cards that improve the hand
        improvingCards =
            List.filter (wouldImproveHand gameState currentRanking) availableCards
    in
    List.length improvingCards


{-| Check if adding a card would improve the current hand
-}
wouldImproveHand : GameState -> HandRanking -> Card -> Bool
wouldImproveHand gameState currentRanking card =
    let
        newHand =
            card :: (gameState.holeCards ++ gameState.communityCards)

        newRanking =
            if List.length newHand >= 5 then
                Hands.evaluateHand newHand

            else
                Hands.HighCard
                    (List.map Cards.rank newHand
                        |> List.sortWith Cards.compareRanks
                        |> List.reverse
                    )
    in
    Hands.handRankingToInt newRanking > Hands.handRankingToInt currentRanking


{-| Calculate pot odds given pot size and bet size
-}
potOdds : Float -> Float -> Float
potOdds potSize betSize =
    betSize / (potSize + betSize)


{-| Calculate expected value of a decision
-}
expectedValue : PokerOdds -> Float -> Float -> Float
expectedValue odds potSize betSize =
    let
        winnings =
            potSize

        cost =
            betSize

        ev =
            (odds.winProbability * winnings) - (odds.lossProbability * cost)
    in
    ev



-- UTILITIES


{-| Deal a specified number of cards from available cards
-}
dealCards : Int -> List Card -> Seed -> ( List Card, Seed )
dealCards numCards availableCards seed =
    if numCards <= 0 || List.isEmpty availableCards then
        ( [], seed )

    else
        let
            ( shuffled, newSeed ) =
                shuffle availableCards seed
        in
        ( List.take numCards shuffled, newSeed )


{-| Shuffle a list of cards using Fisher-Yates algorithm
-}
shuffle : List a -> Seed -> ( List a, Seed )
shuffle list seed =
    let
        array =
            Array.fromList list

        arrayLength =
            Array.length array

        shuffleStep : Int -> Seed -> Array.Array a -> ( Array.Array a, Seed )
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


{-| Swap two elements in an array
-}
swapArrayElements : Int -> Int -> Array.Array a -> Array.Array a
swapArrayElements i j array =
    case ( Array.get i array, Array.get j array ) of
        ( Just elementI, Just elementJ ) ->
            array
                |> Array.set i elementJ
                |> Array.set j elementI

        _ ->
            array


{-| Calculate confidence level based on number of iterations
-}
calculateConfidence : Int -> Float
calculateConfidence iterations =
    -- Simple confidence calculation based on sample size
    let
        baseConfidence =
            0.95

        adjustedConfidence =
            baseConfidence * (1.0 - (1.0 / sqrt (toFloat iterations)))
    in
    min adjustedConfidence 0.99

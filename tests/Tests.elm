module Tests exposing (suite)

import Poker.CardsTest
import Poker.HandsTest
import Test exposing (Test, describe)


suite : Test
suite =
    describe "PokerChance Test Suite"
        [ Poker.CardsTest.suite
        , Poker.HandsTest.suite
        ]

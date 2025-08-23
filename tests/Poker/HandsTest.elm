module Poker.HandsTest exposing (suite)

import Dict
import Expect
import Poker.Cards as Cards exposing (Card, Rank(..), Suit(..))
import Poker.Hands as Hands exposing (Hand, HandRanking(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Poker.Hands"
        [ handRankingRecognitionTests
        , handComparisonTests
        , utilityFunctionTests
        , bestFiveCardHandTests
        , stringConversionTests
        , numericConversionTests
        , edgeCaseTests
        ]



-- HELPER FUNCTIONS


{-| Create a card for easier test writing
-}
card : Rank -> Suit -> Card
card =
    Cards.card


{-| Create test hands with clear notation
-}
royalFlush : List Card
royalFlush =
    [ card Ace Spades, card King Spades, card Queen Spades, card Jack Spades, card Ten Spades ]


straightFlush : List Card
straightFlush =
    [ card Nine Hearts, card Eight Hearts, card Seven Hearts, card Six Hearts, card Five Hearts ]


fourOfAKind : List Card
fourOfAKind =
    [ card Ace Spades, card Ace Hearts, card Ace Diamonds, card Ace Clubs, card King Spades ]


fullHouse : List Card
fullHouse =
    [ card King Spades, card King Hearts, card King Diamonds, card Three Spades, card Three Hearts ]


flush : List Card
flush =
    [ card Ace Spades, card Jack Spades, card Nine Spades, card Seven Spades, card Three Spades ]


straight : List Card
straight =
    [ card Ten Spades, card Jack Hearts, card Queen Diamonds, card King Clubs, card Ace Spades ]


wheelStraight : List Card
wheelStraight =
    [ card Ace Spades, card Two Hearts, card Three Diamonds, card Four Clubs, card Five Spades ]


threeOfAKind : List Card
threeOfAKind =
    [ card Ace Spades, card Ace Hearts, card Ace Diamonds, card King Spades, card Queen Hearts ]


twoPair : List Card
twoPair =
    [ card Ace Spades, card Ace Hearts, card King Diamonds, card King Spades, card Queen Hearts ]


onePair : List Card
onePair =
    [ card Ace Spades, card Ace Hearts, card King Diamonds, card Queen Spades, card Jack Hearts ]


highCard : List Card
highCard =
    [ card Ace Spades, card King Hearts, card Queen Diamonds, card Jack Spades, card Nine Hearts ]



-- HAND RANKING RECOGNITION TESTS


handRankingRecognitionTests : Test
handRankingRecognitionTests =
    describe "Hand Ranking Recognition"
        [ describe "Royal Flush"
            [ test "recognizes royal flush" <|
                \_ ->
                    royalFlush
                        |> Hands.handRanking
                        |> isStraightFlush
                        |> Expect.equal True
            , test "royal flush is highest straight flush" <|
                \_ ->
                    let
                        royalFlushRanking =
                            Hands.handRanking royalFlush

                        regularStraightFlushRanking =
                            Hands.handRanking straightFlush
                    in
                    Hands.handRankingToInt royalFlushRanking
                        |> Expect.greaterThan (Hands.handRankingToInt regularStraightFlushRanking)
            ]
        , describe "Straight Flush"
            [ test "recognizes straight flush" <|
                \_ ->
                    straightFlush
                        |> Hands.handRanking
                        |> isStraightFlush
                        |> Expect.equal True
            , test "different from regular flush" <|
                \_ ->
                    let
                        straightFlushRanking =
                            Hands.handRanking straightFlush

                        flushRanking =
                            Hands.handRanking flush
                    in
                    Hands.handRankingToInt straightFlushRanking
                        |> Expect.greaterThan (Hands.handRankingToInt flushRanking)
            ]
        , describe "Four of a Kind"
            [ test "recognizes four of a kind" <|
                \_ ->
                    fourOfAKind
                        |> Hands.handRanking
                        |> isFourOfAKind
                        |> Expect.equal True
            , test "extracts correct quad rank and kicker" <|
                \_ ->
                    case Hands.handRanking fourOfAKind of
                        FourOfAKind quadRank kicker ->
                            Expect.all
                                [ \_ -> quadRank |> Expect.equal Ace
                                , \_ -> kicker |> Expect.equal King
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected FourOfAKind"
            ]
        , describe "Full House"
            [ test "recognizes full house" <|
                \_ ->
                    fullHouse
                        |> Hands.handRanking
                        |> isFullHouse
                        |> Expect.equal True
            , test "extracts correct trip and pair ranks" <|
                \_ ->
                    case Hands.handRanking fullHouse of
                        FullHouse tripRank pairRank ->
                            Expect.all
                                [ \_ -> tripRank |> Expect.equal King
                                , \_ -> pairRank |> Expect.equal Three
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected FullHouse"
            ]
        , describe "Flush"
            [ test "recognizes flush" <|
                \_ ->
                    flush
                        |> Hands.handRanking
                        |> isFlush
                        |> Expect.equal True
            , test "extracts correct high cards in order" <|
                \_ ->
                    case Hands.handRanking flush of
                        Flush ranks ->
                            ranks
                                |> Expect.equal [ Ace, Jack, Nine, Seven, Three ]

                        _ ->
                            Expect.fail "Expected Flush"
            ]
        , describe "Straight"
            [ test "recognizes regular straight" <|
                \_ ->
                    straight
                        |> Hands.handRanking
                        |> isStraight
                        |> Expect.equal True
            , test "recognizes wheel straight (A-2-3-4-5)" <|
                \_ ->
                    wheelStraight
                        |> Hands.handRanking
                        |> isStraight
                        |> Expect.equal True
            , test "wheel straight is lowest straight" <|
                \_ ->
                    let
                        wheelRanking =
                            Hands.handRanking wheelStraight

                        regularStraightRanking =
                            Hands.handRanking straight
                    in
                    Hands.handRankingToInt wheelRanking
                        |> Expect.lessThan (Hands.handRankingToInt regularStraightRanking)
            ]
        , describe "Three of a Kind"
            [ test "recognizes three of a kind" <|
                \_ ->
                    threeOfAKind
                        |> Hands.handRanking
                        |> isThreeOfAKind
                        |> Expect.equal True
            , test "extracts correct trip rank and kickers" <|
                \_ ->
                    case Hands.handRanking threeOfAKind of
                        ThreeOfAKind tripRank kickers ->
                            Expect.all
                                [ \_ -> tripRank |> Expect.equal Ace
                                , \_ -> kickers |> Expect.equal [ King, Queen ]
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected ThreeOfAKind"
            ]
        , describe "Two Pair"
            [ test "recognizes two pair" <|
                \_ ->
                    twoPair
                        |> Hands.handRanking
                        |> isTwoPair
                        |> Expect.equal True
            , test "extracts correct pair ranks and kicker" <|
                \_ ->
                    case Hands.handRanking twoPair of
                        TwoPair highPair lowPair kicker ->
                            Expect.all
                                [ \_ -> highPair |> Expect.equal Ace
                                , \_ -> lowPair |> Expect.equal King
                                , \_ -> kicker |> Expect.equal Queen
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected TwoPair"
            ]
        , describe "One Pair"
            [ test "recognizes one pair" <|
                \_ ->
                    onePair
                        |> Hands.handRanking
                        |> isOnePair
                        |> Expect.equal True
            , test "extracts correct pair rank and kickers" <|
                \_ ->
                    case Hands.handRanking onePair of
                        OnePair pairRank kickers ->
                            Expect.all
                                [ \_ -> pairRank |> Expect.equal Ace
                                , \_ -> kickers |> Expect.equal [ King, Queen, Jack ]
                                ]
                                ()

                        _ ->
                            Expect.fail "Expected OnePair"
            ]
        , describe "High Card"
            [ test "recognizes high card" <|
                \_ ->
                    highCard
                        |> Hands.handRanking
                        |> isHighCard
                        |> Expect.equal True
            , test "extracts correct high cards in order" <|
                \_ ->
                    case Hands.handRanking highCard of
                        HighCard ranks ->
                            ranks
                                |> Expect.equal [ Ace, King, Queen, Jack, Nine ]

                        _ ->
                            Expect.fail "Expected HighCard"
            ]
        ]



-- HAND COMPARISON TESTS


handComparisonTests : Test
handComparisonTests =
    describe "Hand Comparison"
        [ describe "Hand Type Hierarchy"
            [ test "straight flush beats four of a kind" <|
                \_ ->
                    Hands.compareHands straightFlush fourOfAKind
                        |> Expect.equal GT
            , test "four of a kind beats full house" <|
                \_ ->
                    Hands.compareHands fourOfAKind fullHouse
                        |> Expect.equal GT
            , test "full house beats flush" <|
                \_ ->
                    Hands.compareHands fullHouse flush
                        |> Expect.equal GT
            , test "flush beats straight" <|
                \_ ->
                    Hands.compareHands flush straight
                        |> Expect.equal GT
            , test "straight beats three of a kind" <|
                \_ ->
                    Hands.compareHands straight threeOfAKind
                        |> Expect.equal GT
            , test "three of a kind beats two pair" <|
                \_ ->
                    Hands.compareHands threeOfAKind twoPair
                        |> Expect.equal GT
            , test "two pair beats one pair" <|
                \_ ->
                    Hands.compareHands twoPair onePair
                        |> Expect.equal GT
            , test "one pair beats high card" <|
                \_ ->
                    Hands.compareHands onePair highCard
                        |> Expect.equal GT
            ]
        , describe "Tie-Breaking Within Same Hand Type"
            [ test "higher flush beats lower flush" <|
                \_ ->
                    let
                        higherFlush =
                            [ card Ace Spades, card King Spades, card Queen Spades, card Jack Spades, card Ten Spades ]

                        lowerFlush =
                            [ card Ace Hearts, card King Hearts, card Queen Hearts, card Jack Hearts, card Nine Hearts ]
                    in
                    Hands.compareHands higherFlush lowerFlush
                        |> Expect.equal GT
            , test "higher full house beats lower full house" <|
                \_ ->
                    let
                        higherFullHouse =
                            [ card Ace Spades, card Ace Hearts, card Ace Diamonds, card Two Spades, card Two Hearts ]

                        lowerFullHouse =
                            [ card King Spades, card King Hearts, card King Diamonds, card Ace Spades, card Ace Hearts ]
                    in
                    Hands.compareHands higherFullHouse lowerFullHouse
                        |> Expect.equal GT
            , test "higher four of a kind beats lower four of a kind" <|
                \_ ->
                    let
                        higherQuads =
                            [ card King Spades, card King Hearts, card King Diamonds, card King Clubs, card Two Spades ]

                        lowerQuads =
                            [ card Queen Spades, card Queen Hearts, card Queen Diamonds, card Queen Clubs, card Ace Spades ]
                    in
                    Hands.compareHands higherQuads lowerQuads
                        |> Expect.equal GT
            , test "same four of a kind - kicker decides" <|
                \_ ->
                    let
                        higherKicker =
                            [ card Ace Spades, card Ace Hearts, card Ace Diamonds, card Ace Clubs, card King Spades ]

                        lowerKicker =
                            [ card Ace Spades, card Ace Hearts, card Ace Diamonds, card Ace Clubs, card Queen Spades ]
                    in
                    Hands.compareHands higherKicker lowerKicker
                        |> Expect.equal GT
            ]
        ]



-- UTILITY FUNCTION TESTS


utilityFunctionTests : Test
utilityFunctionTests =
    describe "Utility Functions"
        [ describe "isFlush"
            [ test "recognizes flush (all same suit)" <|
                \_ ->
                    [ card Ace Spades, card King Spades, card Queen Spades, card Jack Spades, card Nine Spades ]
                        |> Hands.isFlush
                        |> Expect.equal True
            , test "rejects mixed suits" <|
                \_ ->
                    [ card Ace Spades, card King Hearts, card Queen Spades, card Jack Spades, card Nine Spades ]
                        |> Hands.isFlush
                        |> Expect.equal False
            , test "handles empty list" <|
                \_ ->
                    []
                        |> Hands.isFlush
                        |> Expect.equal False
            , test "handles single card" <|
                \_ ->
                    [ card Ace Spades ]
                        |> Hands.isFlush
                        |> Expect.equal True
            ]
        , describe "isStraight"
            [ test "recognizes regular straight" <|
                \_ ->
                    [ Ace, King, Queen, Jack, Ten ]
                        |> Hands.isStraight
                        |> Expect.equal True
            , test "recognizes wheel straight (A-2-3-4-5)" <|
                \_ ->
                    [ Two, Three, Four, Five, Ace ]
                        |> Hands.isStraight
                        |> Expect.equal True
            , test "rejects non-consecutive ranks" <|
                \_ ->
                    [ Ace, King, Queen, Jack, Nine ]
                        |> Hands.isStraight
                        |> Expect.equal False
            , test "handles empty list" <|
                \_ ->
                    []
                        |> Hands.isStraight
                        |> Expect.equal False
            , test "rejects duplicate ranks" <|
                \_ ->
                    [ Ace, Ace, King, Queen, Jack ]
                        |> Hands.isStraight
                        |> Expect.equal False
            ]
        , describe "groupByRank"
            [ test "groups cards by rank correctly" <|
                \_ ->
                    let
                        testCards =
                            [ card Ace Spades, card Ace Hearts, card King Spades, card Queen Hearts ]

                        groups =
                            Hands.groupByRank testCards
                    in
                    Expect.all
                        [ \_ -> Dict.get 14 groups |> Maybe.map List.length |> Expect.equal (Just 2)
                        , \_ -> Dict.get 13 groups |> Maybe.map List.length |> Expect.equal (Just 1)
                        , \_ -> Dict.get 12 groups |> Maybe.map List.length |> Expect.equal (Just 1)
                        ]
                        ()
            ]
        , describe "sortByRank"
            [ test "sorts cards by rank (highest first)" <|
                \_ ->
                    let
                        unsortedCards =
                            [ card Two Spades, card Ace Hearts, card King Spades ]

                        expectedOrder =
                            [ card Ace Hearts, card King Spades, card Two Spades ]
                    in
                    Hands.sortByRank unsortedCards
                        |> Expect.equal expectedOrder
            ]
        ]



-- BEST FIVE CARD HAND TESTS


bestFiveCardHandTests : Test
bestFiveCardHandTests =
    describe "Best Five Card Hand"
        [ test "returns all cards when 5 or fewer" <|
            \_ ->
                let
                    fiveCards =
                        [ card Ace Spades, card King Hearts, card Queen Diamonds, card Jack Clubs, card Ten Spades ]
                in
                Hands.bestFiveCardHand fiveCards
                    |> List.length
                    |> Expect.equal 5
        , test "selects best 5 from 7 cards" <|
            \_ ->
                let
                    sevenCards =
                        [ card Ace Spades
                        , card Ace Hearts
                        , card Ace Diamonds
                        , card Ace Clubs
                        , card King Spades
                        , card Two Hearts
                        , card Three Diamonds
                        ]

                    bestFive =
                        Hands.bestFiveCardHand sevenCards

                    ranking =
                        Hands.handRanking bestFive
                in
                ranking
                    |> isFourOfAKind
                    |> Expect.equal True
        , test "prefers higher ranking hand from multiple possibilities" <|
            \_ ->
                let
                    -- Cards that can make both straight and flush, should pick flush
                    sevenCards =
                        [ card Nine Spades
                        , card Eight Spades
                        , card Seven Spades
                        , card Six Spades
                        , card Five Spades
                        , card Four Hearts
                        , card Three Hearts
                        ]

                    bestFive =
                        Hands.bestFiveCardHand sevenCards

                    ranking =
                        Hands.handRanking bestFive
                in
                ranking
                    |> isStraightFlush
                    |> Expect.equal True
        , test "correctly identifies best 5-card hand from 7 cards (two pair scenario)" <|
            \_ ->
                let
                    sevenCards =
                        [ card Ace Spades
                        , card Ace Hearts
                        , card Eight Clubs
                        , card Seven Spades
                        , card King Spades
                        , card King Hearts
                        , card Queen Diamonds
                        ]

                    bestFive =
                        Hands.bestFiveCardHand sevenCards

                    handRanking =
                        Hands.handRanking bestFive
                in
                case handRanking of
                    TwoPair highPair lowPair kicker ->
                        -- Should find the two pair of Aces and Kings with Queen kicker
                        Expect.all
                            [ \_ -> highPair |> Expect.equal Ace
                            , \_ -> lowPair |> Expect.equal King
                            , \_ -> kicker |> Expect.equal Queen
                            ]
                            ()

                    _ ->
                        Expect.fail ("Expected TwoPair but got: " ++ Hands.handRankingToString handRanking)
        ]



-- STRING CONVERSION TESTS


stringConversionTests : Test
stringConversionTests =
    describe "String Conversion"
        [ test "converts hand rankings to readable strings" <|
            \_ ->
                Expect.all
                    [ \_ -> Hands.handRankingToString (StraightFlush []) |> Expect.equal "Straight Flush"
                    , \_ -> Hands.handRankingToString (FourOfAKind Ace King) |> Expect.equal "Four of a Kind (As)"
                    , \_ -> Hands.handRankingToString (FullHouse King Three) |> Expect.equal "Full House (Ks over 3s)"
                    , \_ -> Hands.handRankingToString (Flush []) |> Expect.equal "Flush"
                    , \_ -> Hands.handRankingToString (Straight []) |> Expect.equal "Straight"
                    , \_ -> Hands.handRankingToString (ThreeOfAKind Ace []) |> Expect.equal "Three of a Kind (As)"
                    , \_ -> Hands.handRankingToString (TwoPair Ace King Queen) |> Expect.equal "Two Pair (As and Ks)"
                    , \_ -> Hands.handRankingToString (OnePair Ace []) |> Expect.equal "One Pair (As)"
                    , \_ -> Hands.handRankingToString (HighCard [ Ace ]) |> Expect.equal "High Card (A high)"
                    ]
                    ()
        ]



-- NUMERIC CONVERSION TESTS


numericConversionTests : Test
numericConversionTests =
    describe "Numeric Conversion"
        [ test "hand rankings have correct numerical order" <|
            \_ ->
                let
                    rankings =
                        [ HighCard [ Ace ]
                        , OnePair Ace []
                        , TwoPair Ace King Queen
                        , ThreeOfAKind Ace []
                        , Straight []
                        , Flush []
                        , FullHouse King Three
                        , FourOfAKind Ace King
                        , StraightFlush []
                        ]

                    intValues =
                        List.map Hands.handRankingToInt rankings
                in
                intValues
                    |> Expect.equal (List.sort intValues)
        , test "higher ranks within same hand type have higher values" <|
            \_ ->
                let
                    aceQuads =
                        Hands.handRankingToInt (FourOfAKind Ace Two)

                    kingQuads =
                        Hands.handRankingToInt (FourOfAKind King Two)
                in
                aceQuads
                    |> Expect.greaterThan kingQuads
        ]



-- EDGE CASE TESTS


edgeCaseTests : Test
edgeCaseTests =
    describe "Edge Cases"
        [ test "handles empty hand gracefully" <|
            \_ ->
                []
                    |> Hands.bestFiveCardHand
                    |> List.length
                    |> Expect.equal 0
        , test "handles single card" <|
            \_ ->
                [ card Ace Spades ]
                    |> Hands.bestFiveCardHand
                    |> List.length
                    |> Expect.equal 1
        , test "wheel straight beats nothing but loses to six-high straight" <|
            \_ ->
                let
                    wheelRanking =
                        Hands.handRanking wheelStraight

                    sixHighStraight =
                        [ card Six Spades, card Five Hearts, card Four Diamonds, card Three Clubs, card Two Spades ]

                    sixHighRanking =
                        Hands.handRanking sixHighStraight
                in
                Hands.handRankingToInt wheelRanking
                    |> Expect.lessThan (Hands.handRankingToInt sixHighRanking)
        ]



-- HELPER FUNCTIONS FOR PATTERN MATCHING


isStraightFlush : HandRanking -> Bool
isStraightFlush ranking =
    case ranking of
        StraightFlush _ ->
            True

        _ ->
            False


isFourOfAKind : HandRanking -> Bool
isFourOfAKind ranking =
    case ranking of
        FourOfAKind _ _ ->
            True

        _ ->
            False


isFullHouse : HandRanking -> Bool
isFullHouse ranking =
    case ranking of
        FullHouse _ _ ->
            True

        _ ->
            False


isFlush : HandRanking -> Bool
isFlush ranking =
    case ranking of
        Flush _ ->
            True

        _ ->
            False


isStraight : HandRanking -> Bool
isStraight ranking =
    case ranking of
        Straight _ ->
            True

        _ ->
            False


isThreeOfAKind : HandRanking -> Bool
isThreeOfAKind ranking =
    case ranking of
        ThreeOfAKind _ _ ->
            True

        _ ->
            False


isTwoPair : HandRanking -> Bool
isTwoPair ranking =
    case ranking of
        TwoPair _ _ _ ->
            True

        _ ->
            False


isOnePair : HandRanking -> Bool
isOnePair ranking =
    case ranking of
        OnePair _ _ ->
            True

        _ ->
            False


isHighCard : HandRanking -> Bool
isHighCard ranking =
    case ranking of
        HighCard _ ->
            True

        _ ->
            False

module Poker.CardsTest exposing (suite)

import Expect
import Poker.Cards as Cards exposing (Card, Rank(..), Suit(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Poker.Cards"
        [ cardConstructionTests
        , stringConversionTests
        , numericConversionTests
        , collectionTests
        , propertyTests
        , comparisonTests
        ]


cardConstructionTests : Test
cardConstructionTests =
    describe "Card Construction"
        [ test "creates card with rank and suit" <|
            \_ ->
                let
                    card =
                        Cards.card Ace Spades
                in
                Expect.all
                    [ \c -> Cards.rank c |> Expect.equal Ace
                    , \c -> Cards.suit c |> Expect.equal Spades
                    ]
                    card
        , test "accessors return correct values" <|
            \_ ->
                let
                    card =
                        Cards.card King Hearts
                in
                Expect.all
                    [ \_ -> Cards.rank card |> Expect.equal King
                    , \_ -> Cards.suit card |> Expect.equal Hearts
                    ]
                    ()
        ]


stringConversionTests : Test
stringConversionTests =
    describe "String Conversion"
        [ describe "toString"
            [ test "converts Ace of Spades correctly" <|
                \_ ->
                    Cards.card Ace Spades
                        |> Cards.toString
                        |> Expect.equal "As"
            , test "converts Two of Hearts correctly" <|
                \_ ->
                    Cards.card Two Hearts
                        |> Cards.toString
                        |> Expect.equal "2h"
            , test "converts Ten of Diamonds correctly" <|
                \_ ->
                    Cards.card Ten Diamonds
                        |> Cards.toString
                        |> Expect.equal "Td"
            , test "converts Jack of Clubs correctly" <|
                \_ ->
                    Cards.card Jack Clubs
                        |> Cards.toString
                        |> Expect.equal "Jc"
            ]
        , describe "fromString"
            [ test "parses valid card strings" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.fromString "As" |> Expect.equal (Just (Cards.card Ace Spades))
                        , \_ -> Cards.fromString "2h" |> Expect.equal (Just (Cards.card Two Hearts))
                        , \_ -> Cards.fromString "Td" |> Expect.equal (Just (Cards.card Ten Diamonds))
                        , \_ -> Cards.fromString "Jc" |> Expect.equal (Just (Cards.card Jack Clubs))
                        ]
                        ()
            , test "handles case insensitive input" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.fromString "as" |> Expect.equal (Just (Cards.card Ace Spades))
                        , \_ -> Cards.fromString "KH" |> Expect.equal (Just (Cards.card King Hearts))
                        , \_ -> Cards.fromString "td" |> Expect.equal (Just (Cards.card Ten Diamonds))
                        ]
                        ()
            , test "returns Nothing for invalid strings" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.fromString "invalid" |> Expect.equal Nothing
                        , \_ -> Cards.fromString "1s" |> Expect.equal Nothing
                        , \_ -> Cards.fromString "Ax" |> Expect.equal Nothing
                        , \_ -> Cards.fromString "" |> Expect.equal Nothing
                        , \_ -> Cards.fromString "A" |> Expect.equal Nothing
                        ]
                        ()
            , test "roundtrip conversion works" <|
                \_ ->
                    let
                        originalCard =
                            Cards.card Queen Diamonds
                    in
                    originalCard
                        |> Cards.toString
                        |> Cards.fromString
                        |> Expect.equal (Just originalCard)
            ]
        , describe "suitToString"
            [ test "converts suits to single characters" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.suitToString Hearts |> Expect.equal "h"
                        , \_ -> Cards.suitToString Diamonds |> Expect.equal "d"
                        , \_ -> Cards.suitToString Clubs |> Expect.equal "c"
                        , \_ -> Cards.suitToString Spades |> Expect.equal "s"
                        ]
                        ()
            ]
        , describe "suitToSymbol"
            [ test "converts suits to Unicode symbols" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.suitToSymbol Hearts |> Expect.equal "♥"
                        , \_ -> Cards.suitToSymbol Diamonds |> Expect.equal "♦"
                        , \_ -> Cards.suitToSymbol Clubs |> Expect.equal "♣"
                        , \_ -> Cards.suitToSymbol Spades |> Expect.equal "♠"
                        ]
                        ()
            ]
        , describe "rankToString"
            [ test "converts number ranks correctly" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.rankToString Two |> Expect.equal "2"
                        , \_ -> Cards.rankToString Three |> Expect.equal "3"
                        , \_ -> Cards.rankToString Four |> Expect.equal "4"
                        , \_ -> Cards.rankToString Five |> Expect.equal "5"
                        , \_ -> Cards.rankToString Six |> Expect.equal "6"
                        , \_ -> Cards.rankToString Seven |> Expect.equal "7"
                        , \_ -> Cards.rankToString Eight |> Expect.equal "8"
                        , \_ -> Cards.rankToString Nine |> Expect.equal "9"
                        ]
                        ()
            , test "converts face cards correctly" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.rankToString Ten |> Expect.equal "T"
                        , \_ -> Cards.rankToString Jack |> Expect.equal "J"
                        , \_ -> Cards.rankToString Queen |> Expect.equal "Q"
                        , \_ -> Cards.rankToString King |> Expect.equal "K"
                        , \_ -> Cards.rankToString Ace |> Expect.equal "A"
                        ]
                        ()
            ]
        ]


numericConversionTests : Test
numericConversionTests =
    describe "Numeric Conversion"
        [ describe "rankToInt"
            [ test "converts ranks to correct integers" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.rankToInt Two |> Expect.equal 2
                        , \_ -> Cards.rankToInt Three |> Expect.equal 3
                        , \_ -> Cards.rankToInt Four |> Expect.equal 4
                        , \_ -> Cards.rankToInt Five |> Expect.equal 5
                        , \_ -> Cards.rankToInt Six |> Expect.equal 6
                        , \_ -> Cards.rankToInt Seven |> Expect.equal 7
                        , \_ -> Cards.rankToInt Eight |> Expect.equal 8
                        , \_ -> Cards.rankToInt Nine |> Expect.equal 9
                        , \_ -> Cards.rankToInt Ten |> Expect.equal 10
                        , \_ -> Cards.rankToInt Jack |> Expect.equal 11
                        , \_ -> Cards.rankToInt Queen |> Expect.equal 12
                        , \_ -> Cards.rankToInt King |> Expect.equal 13
                        , \_ -> Cards.rankToInt Ace |> Expect.equal 14
                        ]
                        ()
            , test "Ace has highest value" <|
                \_ ->
                    Cards.rankToInt Ace
                        |> Expect.greaterThan (Cards.rankToInt King)
            , test "maintains order for all ranks" <|
                \_ ->
                    let
                        ranks =
                            [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]

                        intValues =
                            List.map Cards.rankToInt ranks
                    in
                    intValues
                        |> Expect.equal (List.sort intValues)
            ]
        ]


collectionTests : Test
collectionTests =
    describe "Collections"
        [ describe "allSuits"
            [ test "contains exactly 4 suits" <|
                \_ ->
                    Cards.allSuits
                        |> List.length
                        |> Expect.equal 4
            , test "contains all expected suits" <|
                \_ ->
                    Cards.allSuits
                        |> Expect.equalLists [ Hearts, Diamonds, Clubs, Spades ]
            ]
        , describe "allRanks"
            [ test "contains exactly 13 ranks" <|
                \_ ->
                    Cards.allRanks
                        |> List.length
                        |> Expect.equal 13
            , test "contains all expected ranks in order" <|
                \_ ->
                    Cards.allRanks
                        |> Expect.equalLists
                            [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]
            ]
        , describe "allCards"
            [ test "contains exactly 52 cards" <|
                \_ ->
                    Cards.allCards
                        |> List.length
                        |> Expect.equal 52
            , test "contains no duplicate cards" <|
                \_ ->
                    let
                        cardStrings =
                            List.map Cards.toString Cards.allCards

                        uniqueCardStrings =
                            List.foldl
                                (\card acc ->
                                    if List.member card acc then
                                        acc

                                    else
                                        card :: acc
                                )
                                []
                                cardStrings
                    in
                    List.length uniqueCardStrings
                        |> Expect.equal 52
            , test "contains expected number of each suit" <|
                \_ ->
                    let
                        suitCounts =
                            List.foldl
                                (\card acc ->
                                    let
                                        cardSuit =
                                            Cards.suit card
                                    in
                                    case cardSuit of
                                        Hearts ->
                                            { acc | hearts = acc.hearts + 1 }

                                        Diamonds ->
                                            { acc | diamonds = acc.diamonds + 1 }

                                        Clubs ->
                                            { acc | clubs = acc.clubs + 1 }

                                        Spades ->
                                            { acc | spades = acc.spades + 1 }
                                )
                                { hearts = 0, diamonds = 0, clubs = 0, spades = 0 }
                                Cards.allCards
                    in
                    Expect.all
                        [ \counts -> counts.hearts |> Expect.equal 13
                        , \counts -> counts.diamonds |> Expect.equal 13
                        , \counts -> counts.clubs |> Expect.equal 13
                        , \counts -> counts.spades |> Expect.equal 13
                        ]
                        suitCounts
            , test "contains expected number of each rank" <|
                \_ ->
                    let
                        rankCounts =
                            List.map
                                (\rank ->
                                    List.filter (\card -> Cards.rank card == rank) Cards.allCards
                                        |> List.length
                                )
                                Cards.allRanks
                    in
                    rankCounts
                        |> List.all (\count -> count == 4)
                        |> Expect.equal True
            ]
        ]


propertyTests : Test
propertyTests =
    describe "Card Properties"
        [ describe "isRed"
            [ test "identifies red cards correctly" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.card Ace Hearts |> Cards.isRed |> Expect.equal True
                        , \_ -> Cards.card King Diamonds |> Cards.isRed |> Expect.equal True
                        , \_ -> Cards.card Queen Hearts |> Cards.isRed |> Expect.equal True
                        , \_ -> Cards.card Two Diamonds |> Cards.isRed |> Expect.equal True
                        ]
                        ()
            , test "identifies black cards as not red" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.card Ace Spades |> Cards.isRed |> Expect.equal False
                        , \_ -> Cards.card King Clubs |> Cards.isRed |> Expect.equal False
                        , \_ -> Cards.card Queen Spades |> Cards.isRed |> Expect.equal False
                        , \_ -> Cards.card Two Clubs |> Cards.isRed |> Expect.equal False
                        ]
                        ()
            ]
        , describe "isBlack"
            [ test "identifies black cards correctly" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.card Ace Spades |> Cards.isBlack |> Expect.equal True
                        , \_ -> Cards.card King Clubs |> Cards.isBlack |> Expect.equal True
                        , \_ -> Cards.card Queen Spades |> Cards.isBlack |> Expect.equal True
                        , \_ -> Cards.card Two Clubs |> Cards.isBlack |> Expect.equal True
                        ]
                        ()
            , test "identifies red cards as not black" <|
                \_ ->
                    Expect.all
                        [ \_ -> Cards.card Ace Hearts |> Cards.isBlack |> Expect.equal False
                        , \_ -> Cards.card King Diamonds |> Cards.isBlack |> Expect.equal False
                        , \_ -> Cards.card Queen Hearts |> Cards.isBlack |> Expect.equal False
                        , \_ -> Cards.card Two Diamonds |> Cards.isBlack |> Expect.equal False
                        ]
                        ()
            ]
        , test "every card is either red or black, not both" <|
            \_ ->
                Cards.allCards
                    |> List.all (\card -> Cards.isRed card /= Cards.isBlack card)
                    |> Expect.equal True
        ]


comparisonTests : Test
comparisonTests =
    describe "Rank Comparison"
        [ test "compareRanks orders ranks correctly" <|
            \_ ->
                Expect.all
                    [ \_ -> Cards.compareRanks Two Three |> Expect.equal LT
                    , \_ -> Cards.compareRanks King Queen |> Expect.equal GT
                    , \_ -> Cards.compareRanks Ace Ace |> Expect.equal EQ
                    , \_ -> Cards.compareRanks Ace King |> Expect.equal GT
                    , \_ -> Cards.compareRanks Two Ace |> Expect.equal LT
                    ]
                    ()
        , test "sorts ranks in ascending order" <|
            \_ ->
                let
                    shuffledRanks =
                        [ Ace, Two, King, Three, Queen ]

                    expectedOrder =
                        [ Two, Three, Queen, King, Ace ]
                in
                shuffledRanks
                    |> List.sortWith Cards.compareRanks
                    |> Expect.equalLists expectedOrder
        , test "all ranks are comparable" <|
            \_ ->
                let
                    allComparisons =
                        List.concatMap
                            (\rank1 ->
                                List.map
                                    (\rank2 -> Cards.compareRanks rank1 rank2)
                                    Cards.allRanks
                            )
                            Cards.allRanks
                in
                -- Just ensure no crashes occur during comparison
                List.length allComparisons
                    |> Expect.equal (13 * 13)
        ]

module Poker.Cards exposing
    ( Card, Suit(..), Rank(..)
    , card
    , rank, suit
    , toString, fromString, suitToString, suitToSymbol, rankToString
    , rankToInt
    , allCards, allSuits, allRanks
    , isRed, isBlack
    , compareRanks
    )

{-| This module defines the basic card types and utilities for poker calculations.


# Types

@docs Card, Suit, Rank


# Constructors

@docs card


# Accessors

@docs rank, suit


# String Conversion

@docs toString, fromString, suitToString, suitToSymbol, rankToString


# Numeric Conversion

@docs rankToInt


# Collections

@docs allCards, allSuits, allRanks


# Properties

@docs isRed, isBlack


# Comparison

@docs compareRanks

-}

-- TYPES


{-| Represents a playing card suit
-}
type Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades


{-| Represents a playing card rank
-}
type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


{-| Represents a playing card with a rank and suit
-}
type Card
    = Card Rank Suit



-- CONSTRUCTORS


{-| Create a card from a rank and suit
-}
card : Rank -> Suit -> Card
card r s =
    Card r s



-- ACCESSORS


{-| Get the rank from a card
-}
rank : Card -> Rank
rank (Card r _) =
    r


{-| Get the suit from a card
-}
suit : Card -> Suit
suit (Card _ s) =
    s



-- STRING CONVERSION


{-| Convert a card to a string representation (e.g., "As", "Kh", "2c")
-}
toString : Card -> String
toString (Card r s) =
    rankToString r ++ suitToString s


{-| Parse a card from string representation
-}
fromString : String -> Maybe Card
fromString str =
    case String.toList str of
        [ rankChar, suitChar ] ->
            Maybe.map2 Card
                (parseRank rankChar)
                (parseSuit suitChar)

        _ ->
            Nothing


{-| Convert suit to single character string
-}
suitToString : Suit -> String
suitToString s =
    case s of
        Hearts ->
            "h"

        Diamonds ->
            "d"

        Clubs ->
            "c"

        Spades ->
            "s"


{-| Convert suit to Unicode symbol
-}
suitToSymbol : Suit -> String
suitToSymbol s =
    case s of
        Hearts ->
            "♥"

        Diamonds ->
            "♦"

        Clubs ->
            "♣"

        Spades ->
            "♠"


{-| Convert rank to string representation
-}
rankToString : Rank -> String
rankToString r =
    case r of
        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

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



-- NUMERIC CONVERSION


{-| Convert rank to integer value (2-14, where Ace = 14)
-}
rankToInt : Rank -> Int
rankToInt r =
    case r of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ten ->
            10

        Jack ->
            11

        Queen ->
            12

        King ->
            13

        Ace ->
            14



-- COLLECTIONS


{-| All possible cards in a standard deck
-}
allCards : List Card
allCards =
    List.concatMap (\s -> List.map (\r -> Card r s) allRanks) allSuits


{-| All suits in a standard deck
-}
allSuits : List Suit
allSuits =
    [ Hearts, Diamonds, Clubs, Spades ]


{-| All ranks in a standard deck
-}
allRanks : List Rank
allRanks =
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace ]



-- PROPERTIES


{-| Check if a card is red (Hearts or Diamonds)
-}
isRed : Card -> Bool
isRed (Card _ s) =
    case s of
        Hearts ->
            True

        Diamonds ->
            True

        _ ->
            False


{-| Check if a card is black (Clubs or Spades)
-}
isBlack : Card -> Bool
isBlack c =
    not (isRed c)



-- COMPARISON


{-| Compare two ranks by their poker value
-}
compareRanks : Rank -> Rank -> Order
compareRanks r1 r2 =
    compare (rankToInt r1) (rankToInt r2)



-- INTERNAL HELPERS


parseRank : Char -> Maybe Rank
parseRank c =
    case c of
        '2' ->
            Just Two

        '3' ->
            Just Three

        '4' ->
            Just Four

        '5' ->
            Just Five

        '6' ->
            Just Six

        '7' ->
            Just Seven

        '8' ->
            Just Eight

        '9' ->
            Just Nine

        'T' ->
            Just Ten

        't' ->
            Just Ten

        'J' ->
            Just Jack

        'j' ->
            Just Jack

        'Q' ->
            Just Queen

        'q' ->
            Just Queen

        'K' ->
            Just King

        'k' ->
            Just King

        'A' ->
            Just Ace

        'a' ->
            Just Ace

        _ ->
            Nothing


parseSuit : Char -> Maybe Suit
parseSuit c =
    case c of
        'h' ->
            Just Hearts

        'H' ->
            Just Hearts

        'd' ->
            Just Diamonds

        'D' ->
            Just Diamonds

        'c' ->
            Just Clubs

        'C' ->
            Just Clubs

        's' ->
            Just Spades

        'S' ->
            Just Spades

        _ ->
            Nothing

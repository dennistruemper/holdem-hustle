module Components.CardSelector exposing
    ( Config, defaultConfig
    , view
    )

{-| A card selector component for choosing poker cards.


# Configuration

@docs Config, defaultConfig


# View

@docs view

-}

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList, disabled, style, title)
import Html.Events exposing (onClick)
import Poker.Cards as Cards exposing (Card, Rank(..), Suit(..))



-- TYPES


{-| Configuration for the card selector
-}
type alias Config msg =
    { onCardSelect : Card -> msg
    , selectedCards : List Card
    , maxCards : Int
    , showSuitSymbols : Bool
    , disabled : Bool
    }


{-| Default configuration for the card selector
-}
defaultConfig : (Card -> msg) -> Config msg
defaultConfig onCardSelect =
    { onCardSelect = onCardSelect
    , selectedCards = []
    , maxCards = 2
    , showSuitSymbols = True
    , disabled = False
    }



-- VIEW


{-| Render the card selector component
-}
view : Config msg -> Html msg
view config =
    div [ class "card-selector" ]
        [ div [ class "card-selector__header" ]
            [ text ("Select cards (" ++ String.fromInt (List.length config.selectedCards) ++ "/" ++ String.fromInt config.maxCards ++ ")")
            ]
        , div [ class "card-selector__grid" ]
            (List.map (viewCard config) Cards.allCards)
        ]


{-| Render a single card button
-}
viewCard : Config msg -> Card -> Html msg
viewCard config card =
    let
        isSelected =
            List.member card config.selectedCards

        isDisabled =
            config.disabled || (not isSelected && List.length config.selectedCards >= config.maxCards)

        cardRank =
            Cards.rank card

        cardSuit =
            Cards.suit card

        cardText =
            if config.showSuitSymbols then
                Cards.rankToString cardRank ++ Cards.suitToSymbol cardSuit

            else
                Cards.toString card

        cardColor =
            if Cards.isRed card then
                "red"

            else
                "black"
    in
    button
        [ class "card-selector__card"
        , classList
            [ ( "card-selector__card--selected", isSelected )
            , ( "card-selector__card--disabled", isDisabled )
            , ( "card-selector__card--red", Cards.isRed card )
            , ( "card-selector__card--black", Cards.isBlack card )
            ]
        , disabled isDisabled
        , onClick (config.onCardSelect card)
        , title (Cards.toString card)
        , style "color" cardColor
        ]
        [ span [ class "card-selector__card-text" ]
            [ text cardText ]
        ]

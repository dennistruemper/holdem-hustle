module Components.PlayerControls exposing
    ( Config, defaultConfig
    , view
    )

{-| Player count control component for managing active players and folded players.


# Configuration

@docs Config, defaultConfig


# View

@docs view

-}

import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes exposing (class, classList, disabled, max, min, step, type_, value)
import Html.Events exposing (onClick, onInput)



-- TYPES


{-| Configuration for the player controls
-}
type alias Config msg =
    { totalPlayers : Int
    , activePlayers : Int
    , onTotalPlayersChange : Int -> msg
    , onActivePlayersChange : Int -> msg
    , onPlayerFold : msg
    , onPlayerJoin : msg
    , disabled : Bool
    , minPlayers : Int
    , maxPlayers : Int
    }


{-| Default configuration for player controls
-}
defaultConfig : (Int -> msg) -> (Int -> msg) -> msg -> msg -> Config msg
defaultConfig onTotalChange onActiveChange onFold onJoin =
    { totalPlayers = 2
    , activePlayers = 2
    , onTotalPlayersChange = onTotalChange
    , onActivePlayersChange = onActiveChange
    , onPlayerFold = onFold
    , onPlayerJoin = onJoin
    , disabled = False
    , minPlayers = 2
    , maxPlayers = 10
    }



-- VIEW


{-| Render the player controls component
-}
view : Config msg -> Html msg
view config =
    div [ class "player-controls" ]
        [ viewTotalPlayersControl config
        , viewActivePlayersControl config
        , viewQuickActions config
        ]


{-| Render total players control
-}
viewTotalPlayersControl : Config msg -> Html msg
viewTotalPlayersControl config =
    div [ class "player-controls__section" ]
        [ label [ class "player-controls__label" ]
            [ text "Total Players"
            ]
        , div [ class "player-controls__input-group" ]
            [ input
                [ type_ "range"
                , class "player-controls__slider"
                , min (String.fromInt config.minPlayers)
                , max (String.fromInt config.maxPlayers)
                , value (String.fromInt config.totalPlayers)
                , onInput (String.toInt >> Maybe.withDefault config.totalPlayers >> config.onTotalPlayersChange)
                , disabled config.disabled
                ]
                []
            , input
                [ type_ "number"
                , class "player-controls__number-input"
                , min (String.fromInt config.minPlayers)
                , max (String.fromInt config.maxPlayers)
                , value (String.fromInt config.totalPlayers)
                , onInput (String.toInt >> Maybe.withDefault config.totalPlayers >> config.onTotalPlayersChange)
                , disabled config.disabled
                ]
                []
            ]
        , div [ class "player-controls__help-text" ]
            [ text ("Including yourself: " ++ String.fromInt config.totalPlayers ++ " players")
            ]
        ]


{-| Render active players control
-}
viewActivePlayersControl : Config msg -> Html msg
viewActivePlayersControl config =
    let
        foldedPlayers =
            config.totalPlayers - config.activePlayers
    in
    div [ class "player-controls__section" ]
        [ label [ class "player-controls__label" ]
            [ text "Active Players"
            ]
        , div [ class "player-controls__input-group" ]
            [ input
                [ type_ "range"
                , class "player-controls__slider"
                , min "1"
                , max (String.fromInt config.totalPlayers)
                , value (String.fromInt config.activePlayers)
                , onInput (String.toInt >> Maybe.withDefault config.activePlayers >> config.onActivePlayersChange)
                , disabled config.disabled
                ]
                []
            , input
                [ type_ "number"
                , class "player-controls__number-input"
                , min "1"
                , max (String.fromInt config.totalPlayers)
                , value (String.fromInt config.activePlayers)
                , onInput (String.toInt >> Maybe.withDefault config.activePlayers >> config.onActivePlayersChange)
                , disabled config.disabled
                ]
                []
            ]
        , div [ class "player-controls__status" ]
            [ span [ class "player-controls__active-count" ]
                [ text (String.fromInt config.activePlayers ++ " active") ]
            , span [ class "player-controls__folded-count" ]
                [ text (String.fromInt foldedPlayers ++ " folded") ]
            ]
        ]


{-| Render quick action buttons
-}
viewQuickActions : Config msg -> Html msg
viewQuickActions config =
    div [ class "player-controls__section" ]
        [ label [ class "player-controls__label" ]
            [ text "Quick Actions"
            ]
        , div [ class "player-controls__actions" ]
            [ button
                [ class "player-controls__action-button"
                , classList [ ( "player-controls__action-button--disabled", config.activePlayers <= 1 ) ]
                , onClick config.onPlayerFold
                , disabled (config.disabled || config.activePlayers <= 1)
                ]
                [ text "Player Folds" ]
            , button
                [ class "player-controls__action-button"
                , classList [ ( "player-controls__action-button--disabled", config.activePlayers >= config.totalPlayers ) ]
                , onClick config.onPlayerJoin
                , disabled (config.disabled || config.activePlayers >= config.totalPlayers)
                ]
                [ text "Player Joins" ]
            ]
        ]

module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = NavigateToCalculator
    | NavigateToGame


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NavigateToCalculator ->
            ( model
            , Effect.pushRoutePath Route.Path.Calculator
            )

        NavigateToGame ->
            ( model
            , Effect.pushRoutePath Route.Path.Game
            )



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



-- VIEW


view : Model -> View Msg
view model =
    { title = "PokerChance - Select Mode"
    , body =
        [ div [ class "mode-selection" ]
            [ div [ class "mode-selection-container" ]
                [ h1 [ class "mode-selection-title" ] [ text "🃏 PokerChance" ]
                , p [ class "mode-selection-subtitle" ]
                    [ text "Choose your mode:" ]
                , div [ class "mode-buttons" ]
                    [ button
                        [ class "mode-button mode-button--game"
                        , onClick NavigateToGame
                        ]
                        [ div [ class "mode-button-icon" ] [ text "🎮" ]
                        , div [ class "mode-button-title" ] [ text "Game" ]
                        , div [ class "mode-button-description" ]
                            [ text "Test your poker probability skills" ]
                        ]
                    , button
                        [ class "mode-button mode-button--calculator"
                        , onClick NavigateToCalculator
                        ]
                        [ div [ class "mode-button-icon" ] [ text "📊" ]
                        , div [ class "mode-button-title" ] [ text "Calculator" ]
                        , div [ class "mode-button-description" ]
                            [ text "Calculate live poker odds for your hand" ]
                        ]
                    ]
                ]
            ]
        ]
    }

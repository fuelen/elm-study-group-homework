module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, progress, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Time


type alias Model =
    { elapsedTime : Int
    , sliderValue : Int
    }


initialModel flags =
    ( { elapsedTime = 1
      , sliderValue = 0
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | Tick Time.Posix
    | Reset
    | UpdateSlider String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick time ->
            ( { model | elapsedTime = model.elapsedTime + 1 }, Cmd.none )

        UpdateSlider valueStr ->
            case String.toInt valueStr of
                Just value ->
                    ( { model
                        | elapsedTime = model.elapsedTime + (value - model.sliderValue)
                        , sliderValue = value
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        Reset ->
            ( { model | elapsedTime = 0 }, Cmd.none )


maxProgressValue =
    "20"


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ progress
                [ Html.Attributes.max maxProgressValue
                , value (model.elapsedTime |> String.fromInt)
                ]
                []
            ]
        , div []
            [ input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.min "0"
                , Html.Attributes.max "20"
                , onInput UpdateSlider
                , value (String.fromInt model.sliderValue)
                ]
                []
            ]
        , div []
            [ button [ onClick Reset ] [ text "Rest" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 (\x -> Tick x)


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

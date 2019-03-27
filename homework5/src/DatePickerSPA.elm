module DatePickerSPA exposing (main)

import Browser
import Date exposing (Date)
import DatePicker
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { datePicker : DatePicker.DatePicker, date : Maybe Date }


initialModel flags =
    let
        ( datePicker, datePickerCmd ) =
            DatePicker.init
    in
    ( { datePicker = datePicker, date = Nothing }
    , Cmd.map SetDatePicker datePickerCmd
    )


type Msg
    = NoOp
    | SetDatePicker DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetDatePicker subMsg ->
            let
                ( newDatePicker, dateEvent ) =
                    DatePicker.update DatePicker.defaultSettings subMsg model.datePicker

                date =
                    case dateEvent of
                        DatePicker.Picked newDate ->
                            Just newDate

                        _ ->
                            model.date
            in
            ( { model
                | date = date
                , datePicker = newDatePicker
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ DatePicker.view
            model.date
            DatePicker.defaultSettings
            model.datePicker
            |> Html.map SetDatePicker
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

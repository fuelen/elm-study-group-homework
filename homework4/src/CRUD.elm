module CRUD exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, input, label, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type alias UserID =
    String


type alias User =
    { name : String
    , surname : String
    , id : UserID
    }


type alias Model =
    { users : Dict UserID User
    , inputName : String
    , inputSurname : String
    , selectedUserID : Maybe UserID
    , filteredUsers : Maybe (List User)
    }


initialModel flags =
    ( { users =
            [ { id = "1", name = "test", surname = "test1123" }
            , { id = "2", name = "qwe", surname = "mnmnhhh" }
            , { id = "3", name = "qwk", surname = "qpweo" }
            ]
                |> List.map (\user -> ( user.id, user ))
                |> Dict.fromList
      , selectedUserID = Nothing
      , inputName = ""
      , inputSurname = ""
      , filteredUsers = Nothing
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | FilterPrefix String
    | SelectUser String
    | InputName String
    | InputSurname String
    | CreateUser
    | UpdateUser UserID
    | DeleteUser UserID


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Debug.log "state"
        (case msg of
            NoOp ->
                ( model, Cmd.none )

            FilterPrefix prefixInput ->
                let
                    filteredUsers =
                        if prefixInput == "" then
                            Nothing

                        else
                            model.users
                                |> Dict.values
                                |> List.filter (\user -> String.startsWith prefixInput user.surname)
                                |> Just
                in
                ( { model | filteredUsers = filteredUsers }, Cmd.none )

            SelectUser userId ->
                let
                    selectedUser =
                        Dict.get userId model.users

                    newInputSurname =
                        selectedUser
                            |> Maybe.map .surname
                            |> Maybe.withDefault model.inputSurname

                    newInputName =
                        selectedUser
                            |> Maybe.map .name
                            |> Maybe.withDefault model.inputName
                in
                ( { model
                    | selectedUserID = Just userId
                    , inputName = newInputName
                    , inputSurname = newInputSurname
                  }
                , Cmd.none
                )

            InputSurname surname ->
                ( { model | inputSurname = surname }, Cmd.none )

            InputName name ->
                ( { model | inputName = name }, Cmd.none )

            CreateUser ->
                let
                    user =
                        { id = "TODO: generate id" ++ model.inputName ++ model.inputSurname
                        , name = model.inputName
                        , surname = model.inputSurname
                        }
                in
                ( { model
                    | users = Dict.insert user.id user model.users
                    , inputName = ""
                    , inputSurname = ""
                    , selectedUserID = Just user.id
                  }
                , Cmd.none
                )

            UpdateUser selectedUserID ->
                let
                    updateUser user =
                        { user
                            | name = model.inputName
                            , surname = model.inputSurname
                        }
                in
                ( { model
                    | users = Dict.update selectedUserID (Maybe.map updateUser) model.users
                  }
                , Cmd.none
                )

            DeleteUser selectedUserID ->
                ( { model
                    | users = model.users |> Dict.remove selectedUserID
                    , selectedUserID = Nothing
                  }
                , Cmd.none
                )
        )


formatName : User -> String
formatName user =
    user.surname ++ ", " ++ user.name


view : Model -> Html Msg
view model =
    let
        onClickOrDisabled : (a -> Msg) -> Maybe a -> Html.Attribute Msg
        onClickOrDisabled msg =
            Maybe.map (msg >> onClick) >> Maybe.withDefault (disabled True)

        userOption : Maybe UserID -> User -> Html Msg
        userOption selectedID user =
            option
                [ value user.id
                , selected
                    (selectedID
                        |> Maybe.map ((==) user.id)
                        |> Maybe.withDefault False
                    )
                ]
                [ text (formatName user) ]
    in
    div []
        [ div []
            [ label []
                [ text "Filter prefix: "
                , input [ type_ "text", onInput FilterPrefix ] []
                ]
            ]
        , br [] []
        , div []
            [ select
                [ size 5, onInput SelectUser ]
                (model.filteredUsers
                    |> Maybe.withDefault (model.users |> Dict.values)
                    |> List.map (userOption model.selectedUserID)
                )
            ]
        , div []
            [ div []
                [ label []
                    [ text "Name: "
                    , input [ type_ "text", onInput InputName, value model.inputName ] []
                    ]
                ]
            , div []
                [ label []
                    [ text "Surname: "
                    , input [ type_ "text", onInput InputSurname, value model.inputSurname ] []
                    ]
                ]
            ]
        , div []
            [ button [ onClick CreateUser ] [ text "Create" ]
            , button
                [ onClickOrDisabled UpdateUser model.selectedUserID ]
                [ text "Update" ]
            , button
                [ onClickOrDisabled DeleteUser model.selectedUserID ]
                [ text "Delete" ]
            ]
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

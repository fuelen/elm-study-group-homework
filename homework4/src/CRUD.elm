module CRUD exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, input, label, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


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


jsonEncUserParams user =
    Json.Encode.object
        [ ( "name", Json.Encode.string user.name )
        , ( "surname", Json.Encode.string user.surname )
        ]


jsonDecUser =
    Json.Decode.succeed
        (\id name surname ->
            { id = String.fromInt id
            , name = name
            , surname = surname
            }
        )
        |> Json.Decode.Pipeline.required "id" Json.Decode.int
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.required "surname" Json.Decode.string


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
    , Http.get
        { url = "http://localhost:8000/api/users/list.json"
        , expect = Http.expectJson GotUsers (Json.Decode.list jsonDecUser)
        }
    )


type Msg
    = NoOp
    | FilterPrefix String
    | SelectUser String
    | InputName String
    | InputSurname String
    | CreateUser
    | CreatedUser (Result Http.Error User)
    | UpdateUser UserID
    | UpdatedUser (Result Http.Error User)
    | DeleteUser UserID
    | DeletedUser (Result Http.Error UserID)
    | GotUsers (Result Http.Error (List User))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        GotUsers result ->
            let
                usersToDict =
                    List.map (\user -> ( user.id, user )) >> Dict.fromList
            in
            case result of
                Ok users ->
                    ( { model | users = usersToDict users }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

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
                userParams =
                    { name = model.inputName
                    , surname = model.inputSurname
                    }
            in
            ( model
            , Http.post
                { url = "http://localhost:8000/api/users/create.json"
                , body = Http.jsonBody (jsonEncUserParams userParams)
                , expect = Http.expectJson CreatedUser jsonDecUser
                }
            )

        CreatedUser result ->
            case result of
                Ok user ->
                    ( { model
                        | users = Dict.insert user.id user model.users
                        , inputName = ""
                        , inputSurname = ""
                        , selectedUserID = Just user.id
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )

        UpdateUser selectedUserID ->
            let
                userParams =
                    { name = model.inputName
                    , surname = model.inputSurname
                    }
            in
            ( model
            , Http.request
                { method = "PUT"
                , headers = []
                , url = "http://localhost:8000/api/users/" ++ selectedUserID ++ "/update.json"
                , body = Http.jsonBody (jsonEncUserParams userParams)
                , expect = Http.expectJson UpdatedUser jsonDecUser
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        UpdatedUser result ->
            case result of
                Ok user ->
                    ( { model | users = Dict.update user.id (always (Just user)) model.users }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )

        DeleteUser selectedUserID ->
            ( model
            , Http.request
                { method = "DELETE"
                , headers = []
                , url = "http://localhost:8000/api/users/" ++ selectedUserID ++ "/delete.json"
                , body = Http.emptyBody
                , expect =
                    Http.expectJson DeletedUser
                        (Json.Decode.succeed
                            identity
                            |> Json.Decode.Pipeline.hardcoded selectedUserID
                        )
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        DeletedUser result ->
            case result of
                Ok selectedUserID ->
                    ( { model
                        | users = model.users |> Dict.remove selectedUserID
                        , selectedUserID = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( model, Cmd.none )


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

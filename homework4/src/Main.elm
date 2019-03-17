module Main exposing (AccountInfo, Country, Mottos, User, decodeDate, decodeMottos, decodeUser, jsonDecAccountInfo, jsonPair, mottos)

import Iso8601
import Json.Decode as J
import Json.Decode.Pipeline as Pipeline
import Time


type alias User =
    { name : String
    , cats : Maybe Int
    }


decodeUser : J.Decoder User
decodeUser =
    J.map2 User (J.field "name" J.string) (J.field "cats" (J.nullable J.int))



-- > J.decodeString decodeUser "{\"name\": \"Kostia\", \"cats\": 2}"
-- Ok { cats = Just 2, name = "Kostia" }
--     : Result J.Error User
-- > J.decodeString decodeUser "{\"name\": \"Droopy The Dog\", \"cats\": null}"
-- Ok { cats = Nothing, name = "Droopy The Dog" }
--     : Result J.Error User


mottos : String
mottos =
    """
      {"Germany": {"motto": "Einigkeit und Recht und Freiheit", "currency": "EUR"},
       "England": {"motto": "God Save the Queen", "currency": "GBP"},
       "France": {"motto": "Liberté, Égalité, Fraternité", "currency": "EUR"}}
      """


type alias Mottos =
    { countries : List Country }


type alias Country =
    { name : String
    , motto : String
    , currency : String
    }


decodeMottos : J.Decoder Mottos
decodeMottos =
    J.map
        (List.map (\( name, details ) -> details name) >> Mottos)
        (J.keyValuePairs
            (J.map2
                (\motto currency name -> Country name motto currency)
                (J.field "motto" J.string)
                (J.field "currency" J.string)
            )
        )



-- decodeDate : J.Decoder Time.Posix


decodeDate =
    J.string
        |> J.andThen
            (\date ->
                case Iso8601.toTime date of
                    Ok res ->
                        J.succeed res

                    Err error ->
                        J.fail "Invalid date format"
            )



-- > J.decodeString decodeDate "\"2018-10-01T12:48:00.000Z\""
-- Ok (Posix 1538398080000)
--   : Result J.Error Time.Posix


type alias AccountInfo =
    { id : Int
    , email : String
    , fullName : Maybe String
    , phoneNumber : Maybe String
    , infoComplete : Bool
    }


jsonDecAccountInfo : J.Decoder AccountInfo
jsonDecAccountInfo =
    J.succeed AccountInfo
        |> Pipeline.required "id" J.int
        |> Pipeline.required "email" J.string
        |> Pipeline.required "full_name" (J.nullable J.string)
        |> Pipeline.required "phone_number" (J.nullable J.string)
        |> Pipeline.required "info_complete" J.bool



-- jsonEncAccountInfo : AccountInfo -> Value
-- jsonEncAccountInfo val =
--     Debug.todo ""


jsonPair : J.Decoder a -> J.Decoder b -> J.Decoder ( a, b )
jsonPair a b =
    J.map2 Tuple.pair
        (J.index 0 a)
        (J.index 1 b)



-- > J.decodeString (jsonPair J.string J.string) "[\"Louee\", \"Baton\"]"
-- Ok ("Louee","Baton")
--     : Result J.Error ( String, String )
-- > J.decodeString (jsonPair J.string J.string) "[\"Louee\"]"
-- Err (Failure ("Expecting a list of two elements") <internals>)
--     : Result J.Error ( String, String )

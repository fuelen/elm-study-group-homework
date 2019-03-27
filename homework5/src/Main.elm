module Main exposing (MyJsonDecoder(..), Path(..), Val(..), decodePath, decodeValue, myJsonBoolean, myJsonField, myJsonInt, myJsonString, stringifyVal)

import Dict
import Json.Decode as Decode


type Val
    = Str String
    | Boolean Bool
    | Num Int
    | Obj (List ( String, Val ))
    | Nil
    | Lst (List Val)


stringifyVal : Val -> String
stringifyVal v =
    case v of
        Nil ->
            "(Nil)"

        Str s ->
            "(Str \"" ++ s ++ "\")"

        Num n ->
            "(Num " ++ String.fromInt n ++ ")"

        Boolean True ->
            "(Boolean True)"

        Boolean False ->
            "(Boolean False)"

        Lst l ->
            "(Lst [" ++ (l |> List.map stringifyVal |> String.join ", ") ++ "])"

        Obj l ->
            "Obj ["
                ++ (l
                        |> List.map (\( key, value ) -> "(\"" ++ key ++ "\"," ++ stringifyVal value ++ ")")
                        |> String.join ", "
                   )
                ++ "]"


type MyJsonDecoder a
    = MyJsonDecoder (Val -> Result String a)


decodeValue : MyJsonDecoder a -> Val -> Result String a
decodeValue (MyJsonDecoder f) v =
    f v


myJsonField field (MyJsonDecoder decoder) =
    MyJsonDecoder
        (\v ->
            case v of
                Obj l ->
                    -- We can use List.find from extra package
                    case l |> Dict.fromList |> Dict.get field of
                        Just r ->
                            decoder r

                        Nothing ->
                            Err ("Could not find field \"" ++ field ++ "\" in " ++ stringifyVal v)

                _ ->
                    Err ("Expected Obj, got" ++ stringifyVal v)
        )


myJsonString : MyJsonDecoder String
myJsonString =
    MyJsonDecoder
        (\v ->
            case v of
                Str s ->
                    Ok s

                _ ->
                    Err ("Expected Str, got: " ++ stringifyVal v)
        )


myJsonBoolean : MyJsonDecoder Bool
myJsonBoolean =
    MyJsonDecoder
        (\v ->
            case v of
                Boolean t ->
                    Ok t

                _ ->
                    Err ("Expected Boolean, got: " ++ stringifyVal v)
        )


myJsonInt : MyJsonDecoder Int
myJsonInt =
    MyJsonDecoder
        (\v ->
            case v of
                Num n ->
                    Ok n

                _ ->
                    Err ("Expected Num, got: " ++ stringifyVal v)
        )


type Path
    = PathFile String
    | PathUrl { host : String, port_ : Int }


decodePath : Decode.Decoder Path
decodePath =
    Decode.field "path_type" Decode.string
        |> Decode.andThen
            (\pathType ->
                case pathType of
                    "file" ->
                        Decode.field "value" (Decode.string |> Decode.map PathFile)

                    "url" ->
                        Decode.field "value"
                            (Decode.map2 (\host port_ -> PathUrl { host = host, port_ = port_ })
                                (Decode.field "host" Decode.string)
                                (Decode.field "port" Decode.int)
                            )

                    _ ->
                        Decode.fail "Unexpected path_type"
            )

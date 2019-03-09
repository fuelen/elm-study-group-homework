module Main exposing (either, find, keepOks, mapOk, maybeToList, parseDate, updateList, updateListKv)

import Date exposing (Date)


maybeToList : Maybe a -> List a
maybeToList x =
    case x of
        Nothing ->
            []

        Just value ->
            [ value ]



-- > maybeToList (Just 3)
-- [3]
-- > maybeToList Nothing
-- []


updateList : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
updateList shouldChange f =
    List.filterMap
        (\x ->
            if shouldChange x then
                f x

            else
                Just x
        )



-- > updateList (\x -> x == 3) (\v -> Just (v + 1)) [1,3,5]
-- [1,4,5] : List number
-- > updateList (\x -> x == 3) (\v -> Nothing) [1,3,5]
-- [1,5] : List number


find : (a -> Bool) -> List a -> Maybe a
find f =
    List.foldl
        (\x _ ->
            if f x then
                Just x

            else
                Nothing
        )
        Nothing



-- > find (\x -> x == 2) [1,3,5,2]
-- Just 2 : Maybe number
-- > find (\x -> x == 2) [1,3,5]
-- Nothing : Maybe number


updateListKv :
    List ( k, v )
    -> k
    -> (v -> Maybe v)
    -> List ( k, v )
updateListKv list k updateValue =
    updateList (Tuple.first >> (==) k)
        (\( key, value ) ->
            value
                |> updateValue
                |> Maybe.map (Tuple.pair key)
        )
        list



-- > updateListKv [("foo", 1), ("bar", 2)] "foo" (\x -> Just (x + 1))
-- [("foo", 2), ("bar", 2)]
-- > updateListKv [("foo", 1), ("bar", 2)] "foo" (\x -> Nothing)
-- [("bar", 2)]


keepOks : List (Result a b) -> List b
keepOks =
    List.concatMap (Result.toMaybe >> maybeToList)



-- > keepOks [Ok 1, Err "bad", Ok 2]
-- [1,2] : List number


mapOk : (b -> c) -> Result a b -> Result a c
mapOk =
    Result.map



-- > mapOk (\x -> x + 1) (Ok 2)
-- Ok 3 : Result a number
-- > mapOk (\x -> x + 1) (Err "str")
-- Err "str" : Result String number


either : (a -> c) -> (b -> c) -> Result a b -> c
either fa fb res =
    case res of
        Ok value ->
            fb value

        Err value ->
            fa value



-- > either (\x -> x + 1) (\x -> x - 1) (Ok 1)
-- 0 : number
-- > either (\x -> x + 1) (\x -> x - 1) (Err 1)
-- 2 : number
--


parseDate : Maybe String -> Maybe Date
parseDate =
    Maybe.andThen (Date.fromIsoString >> Result.toMaybe)

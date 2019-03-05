module Main exposing (Address, Profile, User, bird, bird2, bird3, buildStatsUrl, catMaybes, convert, convert02, convert03, mapMaybes, setPhone)

import Monocle.Lens exposing (..)
import Url.Builder


convert :
    List { name : String, email : String, phone_number : String }
    -> List { name : String, email : String }
convert =
    List.map (\{ name, email } -> { name = name, email = email })


convert02 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert02 =
    List.foldl
        (\{ name, email } acc ->
            case ( name, email ) of
                ( Just nameValue, Just emailValue ) ->
                    { name = nameValue, email = emailValue } :: acc

                ( _, _ ) ->
                    acc
        )
        []


convert03 :
    List { name : Maybe String, email : Maybe String }
    -> List { name : String, email : String }
convert03 =
    List.foldl
        (\{ name, email } acc ->
            case ( name, email ) of
                ( Just nameValue, Nothing ) ->
                    { name = nameValue, email = "<unspecified>" } :: acc

                ( _, _ ) ->
                    acc
        )
        []



-- > convert03 [{name=Just "John", email=Nothing}]
-- [{name="John", email="<unspecified>"}]


bird : Int
bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))



-- using <|


bird2 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum <| List.filter notThree <| List.map incr <| [ 1, 2, 3 ]



-- using |>


bird3 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    [ 1, 2, 3 ]
        |> List.map incr
        |> List.filter notThree
        |> List.sum


type alias User =
    { profile : Profile }


type alias Profile =
    { address : Address }


type alias Address =
    { phone : String }


setPhone : String -> User -> User
setPhone phone user =
    let
        addressPhoneLens =
            Lens .phone (\ph a -> { a | phone = ph })

        profileAddressLens =
            Lens .address (\a p -> { p | address = a })

        userProfileLens =
            Lens .profile (\p u -> { u | profile = p })

        userPhoneLens =
            compose (compose userProfileLens profileAddressLens)
                addressPhoneLens
    in
    modify userPhoneLens (always phone) user



-- > setPhone "+123456" { profile = { address = { phone = "+654321" } } }
-- { profile = { address = { phone = "+123456" } } }


mapMaybes : (a -> Maybe b) -> List a -> List b
mapMaybes =
    Debug.todo ""



-- > mapMaybes (\x -> if x == Just 3 then x else Just 4) [Just 1, Nothing, Just 3]
-- [4,4,3] : List number


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.foldl
        (\maybe acc ->
            case maybe of
                Just value ->
                    value :: acc

                Nothing ->
                    acc
        )
        []
        << List.reverse



-- > catMaybes [Just 1, Nothing, Just 3]
-- [1,3] : List number


buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
buildStatsUrl itemId ps =
    let
        queryParams =
            [ ( "num_elems", Maybe.map String.fromInt ps.numElems )
            , ( "start_date", ps.startDate )
            ]
                |> List.foldl
                    (\( key, maybe ) acc ->
                        case maybe of
                            Just value ->
                                Url.Builder.string key value :: acc

                            Nothing ->
                                acc
                    )
                    []
    in
    Url.Builder.absolute [ "api", "item", String.fromInt itemId, "stats.json" ]
        queryParams



-- > buildStatsUrl 12 {startDate=Nothing, numElems=Nothing}
-- https://myapi.com/api/item/12/stats.json
-- > buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Nothing}
-- https://myapi.com/api/item/12/stats.json?start_date=2019-01-01
-- > buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Just 10}
-- https://myapi.com/api/item/12/stats.json?start_date=2019-01-01&num_items=10

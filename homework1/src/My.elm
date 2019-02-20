module My exposing (last, oneButLast, elementAt, length, reverse, isPalindrome, compress, dropEvery, clap)

last list =
  case list of
    [] -> Nothing
    hd :: [] -> Just hd
    hd :: rest -> last rest


oneButLast list =
  case list of
    [] -> Nothing
    hd :: [] -> Nothing
    hd :: _ :: [] -> Just hd
    hd :: rest -> oneButLast rest

elementAt list position =
  if position < 0 then
    Nothing
  else
    let
        doSearch myList currentPosition =
          case myList of
            [] -> Nothing
            hd :: rest ->
              if currentPosition == position then
                Just hd
              else
                doSearch rest (currentPosition + 1)

    in
        doSearch list 0

length = List.foldl (\_ acc -> acc + 1) 0

reverse = List.foldl (::) []

isPalindrome list = list == reverse list

compress string =
  string
  |> String.toList
  |> dedup
  |> List.map String.fromChar
  |> String.join ""

dedup list =
  let
      doDedup myList previousValue acc =
        case myList of
          [] -> acc
          currentValue :: otherValues ->
            if currentValue == previousValue then
              doDedup otherValues currentValue acc
            else
              doDedup otherValues currentValue (currentValue :: acc)
  in
      case list of
        [] -> []
        hd :: rest -> doDedup rest hd [hd] |> reverse

dropEvery string position =
  string
  |> String.toList
  |> List.indexedMap Tuple.pair
  |> List.filter(\(currentPosition, _) -> remainderBy position (currentPosition + 1) /= 0)
  |> List.map Tuple.second
  |> List.map String.fromChar
  |> String.join ""

clap string =
  string
  |> String.words
  |> List.intersperse("👏")
  |> String.join ""

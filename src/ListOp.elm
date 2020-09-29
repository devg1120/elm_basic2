module ListOp exposing (..)

type MyList a
    = Empty
    | Node a (MyList a)

sum : MyList Int -> Int
sum myList =
    case myList of
        Empty ->
            0

        Node intValue remainingNodes ->
            intValue + sum remainingNodes

inverter a =
    case a of
        0 ->
            1

        1 ->
            0

        _ ->
            -1

{--
--eml repl
--  import ListTest exposing (..)

myList = Node 1 (Node 2 (Node 3 (Node 4 Empty)))
sum myList

--}

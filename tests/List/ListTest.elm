module List.ListTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import ListOp

suite : Test
suite =
    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


suite2 =
    test "ok two plus two equals four" <|
        \_ -> (2 + 2) |> Expect.equal 4

listop =
    test "list oplation" <|
        \_ ->
            let
                myList = ListOp.Node 1 (ListOp.Node 2 (ListOp.Node 3 (ListOp.Node 4 ListOp.Empty)))
                _ = Debug.log  "myList" myList   -- # DEBUG
            in
            ListOp.sum myList
                |> Expect.equal 10
                  

inverterTests =
    describe "Inverter"
        [ test "output is 0 when the input is 1" <|
            \_ ->
                ListOp.inverter 0
                    |> Debug.log "Inverter"      -- # DEBUG
                    |> Expect.equal 1
        , test "output is 1 when the input is 0" <|
            \_ ->
                ListOp.inverter 1
                    |> Expect.equal 0
        ]

-----------------------------------------------------------
custom : Test
custom  =
    describe "All our numbers are magical"
        [ test "8 * 3 equals 24"
            (\_ -> expectMagicalNumber (8 * 3))
        , test "12 * 2 equals 24"
            (\_ -> expectMagicalNumber (12 * 2))
        ]


expectMagicalNumber : Int -> Expectation
expectMagicalNumber number =
    if number == 24 then
        Expect.pass

    else
        Expect.fail (String.fromInt number ++ " is not 24..")

-----------------------------------------------------------
suite4 : Test
suite4 =
    describe "The String module"
        [ describe "String.reverse" -- Nest as many descriptions as you like.
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)

            -- Expect.equal is designed to be used in pipeline style, like this.
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"

            -- fuzz runs the test 100 times with randomly-generated inputs!
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]
-----------------------------------------------------------


{--
suite3 : Test
suite3 =
    todo "Implement our first test. See https://package.elm-lang.org/packages/elm-explorations/test/latest for how to do this!"


suite4 =
    test "two plus two equals four" <|
        \_ -> (2 + 2) |> Expect.equal 4


guardianNames =
    test "only 2 guardians have names with less than 6 characters" <|
        \_ ->
            let
                guardians =
                    [ "Star-lord", "Groot", "Gamora", "Drax", "Rocket" ]
            in
            guardians
                |> List.map String.length
                |> List.filter (\x -> x < 6)
                |> List.length
                |> Expect.equal 2

additionTests =
    describe "Addition"
        [ test "two plus two equals four" <|
            \_ -> (2 + 2) |> Expect.equal 4
        , test "three plus four equals seven" <|
            \_ -> (3 + 4) |> Expect.equal 7
        ]

comparisonTests =
    describe "Comparison"
        [ test "2 is not equal to 3" <|
            \_ -> 2 |> Expect.notEqual 3
        , test "4 is less than 5" <|
            \_ -> 4 |> Expect.lessThan 5
        , test "6 is less than or equal to 7" <|
            \_ -> 6 |> Expect.atMost 7
        , test "9 is greater than 8" <|
            \_ -> 9 |> Expect.greaterThan 8 -- 8
        , test "11 is greater than or equal to 10" <|
            \_ -> 11 |> Expect.atLeast 10
        , test "a list with zero elements is empty" <|
            \_ ->
                List.isEmpty []
                    |> Expect.true "expected the list to be empty"
        , test "a list with some elements is not empty" <|
            \_ ->
                List.isEmpty [ "Jyn", "Cassian", "K-2SO" ]
                    |> Expect.false "expected the list not to be empty"
        ]
--}

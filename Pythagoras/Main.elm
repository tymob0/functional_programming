module Main exposing (main)

import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Char exposing (toLower)
import List exposing (filter, length, map)
import Char exposing (fromCode, toCode, isUpper, isLower) 
import Basics exposing (modBy)
import Pythagoras exposing (isTripleTuple, pythTriple)
import String exposing (fromChar, fromList, toList)
import Caesar exposing (encodeChar, decodeChar)

--
-- Caesar (Part 2)
--

sanitize: Char -> Bool
sanitize char =
            ((toCode char >= toCode 'a'  ) && (toCode char <= toCode 'z')) ||
            ((toCode char >= toCode 'A'  ) && (toCode char <= toCode 'Z'))

normalize: String -> String
normalize string =
    fromList (filter sanitize (toList string))

encode: Int -> String -> String
encode offset string =
    case (toList (normalize string)) of
        [] ->
            ""
        x :: xs ->
            (fromChar (encodeChar offset x)) ++ (encode offset (fromList xs))

decode: Int -> String -> String
decode offset string =
    case (toList (normalize string)) of
        [] ->
            ""
        x :: xs ->
            (fromChar (decodeChar offset x)) ++ (decode offset (fromList xs))

--
-- Pythagoras (Part 2)
--


pythTriplesMap: List (Int, Int) -> List (Int, Int, Int)
pythTriplesMap list =
        map pythTriple list


pythTriplesRec: List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec list =
    case list of
        [] ->
            []
        x :: xs ->
            (pythTriple x) :: pythTriplesRec xs

arePythTriplesFilter: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter list =
    filter isTripleTuple list


arePythTriplesRec: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec list =
    case list of
            [] ->
                []
            x :: xs ->
                if isTripleTuple x then
                    x :: (arePythTriplesRec xs)
                else 
                    (arePythTriplesRec xs)



-- REPRESENTATION
caesar2 : List ( String, List ExerciseRunner.Example )
caesar2 =
    [ ( "Caesar (part 2)"
        , [ ExerciseRunner.functionExample1 "normalize"
            normalize
            [ ( ("Hello, Fontys!"), "HelloFontys" )]
        ]
        ),
        ( ""
            , [ ExerciseRunner.functionExample2 "encode"
                encode
                [ ( (7, "Hello, Fontys!"), "OlssvMvuafz" )]
            ]
        ),
        ( ""
            , [ ExerciseRunner.functionExample2 "decode"
                decode
                [ ( (7, "OlssvMvuafz"), "HelloFontys" )]
            ]
        )
    -- , ( "HTML", [] )
    ]

pythagoras2 : List ( String, List ExerciseRunner.Example )
pythagoras2 =
    [ ( "Pythagoras (part 2)"
        , [ ExerciseRunner.functionExample1 "pythTriplesMap"
            pythTriplesMap
            [ ( ([(5,4),(2,1),(35,7)]), [(9,40,41),(3,4,5),(1176,490,1274)] )]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample1 "pythTriplesRec"
            pythTriplesRec
            [ ( ([(5,4),(2,1),(39,7)]), [(9,40,41),(3,4,5),(1472,546,1570)] )]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample1 "arePythTriplesFilter"
            arePythTriplesFilter
            [ ( ([(1,2,3), (9,40,41), (3,4,5), (100,2,500)]),
             [(9,40,41),(3,4,5)] )]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample1 "arePythTriplesRec"
            arePythTriplesRec
            [ ( ([(1,2,3), (9,40,41), (3,4,5), (100,2,500)]),
             [(9,40,41),(3,4,5)] )]
        ]
       )
    -- , ( "HTML", [] )
    ]

main : Html Never
main =
    Html.div
        [ Html.Attributes.style "padding" "20px" ]
        [ ExerciseRunner.fontStyles
        , caesar2
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
        , pythagoras2
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
        ]

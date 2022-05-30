module Main exposing (main)

import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Char exposing (toLower)
import List exposing (length)
import Char exposing (fromCode, toCode, isUpper, isLower)
import Basics exposing (modBy)
import Tuple exposing (first, second)

--
-- Caesar
--

-- Encryption formula with modulo operator.
shift_char: Int -> Char -> Char -> Char
shift_char offset char bound = 
    fromCode (modBy 26 (toCode char - toCode bound + offset) + toCode bound)

-- Depending on the lower/upper case pass different 'bound' symbol.
encode: Int -> Char -> Char
encode offset char =
    if isLower char then
        shift_char offset char 'a'
    else if isUpper char then
        shift_char offset char 'A'
    else
        char

-- Reuse encode function with negative offset.
decode: Int -> Char -> Char
decode offset char =
    encode (0 - offset) char



--
-- Pythagoras
--

-- Compute square.
sqr: Int -> Int
sqr val = val * val

-- Verify if values suit Pythagoras equality.
isTriple: Int -> Int -> Int -> Bool
isTriple adj op hyp = 
    adj > 0 && op > 0 && hyp > 0 && ((sqr adj + sqr op) == sqr hyp)

-- Find one side.
validXY: Int -> Int -> Bool
validXY x y = x>0 && y>0 && x>y

-- Find one side.
leg1: Int -> Int -> Int
leg1 x y =
      if validXY x y then x^2 - y^2
      else 0
-- Find another side.
leg2: Int -> Int -> Int
leg2 x y =
      if validXY x y then 2 * y * x
      else 0
-- Find hypotenuse.
hypotenuse: Int -> Int -> Int
hypotenuse x y =
      if validXY x y then x^2 + y^2
      else 0
-- Form Pythagoras triple from two values.
pythTriple: (Int, Int) -> (Int, Int, Int)
pythTriple (first, second) =
        (leg1 first second, leg2 first second, hypotenuse first second)
-- Verify if values suit Pythagoras equality by taking a tuple as a parameter.
isTripleTuple: (Int, Int, Int) -> Bool
isTripleTuple (adj,op,hyp) =
                        isTriple adj op hyp

-- REPRESENTATION
caesar1 : List ( String, List ExerciseRunner.Example )
caesar1 =
    [ ( "Caesar (part 1)"
        , [ ExerciseRunner.functionExample2 "encode"
            encode
            [ ( (5, 'x'), 'c' )
            , ( (7, 'T'), 'A' )
            ]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample2 "decode"
            decode
            [ ( (5, 'c'), 'x' )
            , ( (7, 'A'), 'T' )
            ]
        ]
        )
    -- , ( "HTML", [] )
    ]

pythagoras1 : List ( String, List ExerciseRunner.Example )
pythagoras1 =
    [ ( "Pythagoras (part 1)"
        , [ ExerciseRunner.functionExample3 "isTriple"
            isTriple
            [ ( (3, 4, 5), True )
            , ( (3, 4, 6), False )
            ]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample1 "pythTriple "
            pythTriple
            [ ( (5,4), (9,40,41) )]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample1 "isTripleTuple"
            isTripleTuple
            [ ( (9,40,41), True )]
        ]
        ),
        ( ""
        , [ ExerciseRunner.functionExample1 "isTripleTuple with pythTriple "
            isTripleTuple
            [( (pythTriple (5,4)) , True )]
            ]
        )
    ]


main : Html Never
main =
    Html.div
        [ Html.Attributes.style "padding" "20px" ]
        [ ExerciseRunner.fontStyles
        , caesar1
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
                    , pythagoras1
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
        ]

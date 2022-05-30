module Merge exposing (main)

import ExerciseRunner
import Html exposing (Html)
import Html.Attributes
import Char exposing (toLower)
import List exposing (drop, length)
import String exposing (fromFloat, fromInt)
import Basics exposing (modBy,abs)
import Tuple exposing (first, second)

--
-- Merge sort
--


--split: Int->List Int -> (List Int, List Int)
--split i list =
--    case list of
--        [] -> ([],[])
--        (x::xs) ->
--            if ((modBy 2 i) == 0) then
--                if length list >= i then
--                   (x::first (split i xs),drop (i - 1) xs)
--                else
--                    ([], [])
--            else
--                if length list > i then
--                   (x::first (split i xs),drop (i - 1) xs)
--                else
--                    ([], [])


split : List a -> List a -> List a -> ( List a, List a )
split list left right =
  case list of
    [] ->
      (left, right)
    x::[] ->
        split [] (x :: left) right
    x :: y :: xs ->
        split xs (x :: left) (y::right)



merge: List Int -> List Int -> List Int
merge left right =
    case (left,right) of
        ([],[]) -> []
        (_,[]) -> left
        ([],_) -> right
        (x::xs,y::ys) ->
            if x < y then
                x :: merge xs right
            else
                y :: merge left ys


msort: List Int -> List Int
msort input =
    case input of
        [] -> input
        [_] -> input
        _ ->
              merge (msort (first (split input [] [] )))
                    (msort (second (split input [] [])))



-- REPRESENTATION
math1 : List ( String, List ExerciseRunner.Example )
math1 =
    [
    -- , ( "HTML", [] )
    ( "MergeSort"
                , [ ExerciseRunner.functionExample2 "Merge"
                    merge
                    [(([4,5,9,18],[1,2,3]),  ([1,2,3,4,5,9,18]))]
                ]
        ),
     ( ""
         , [ ExerciseRunner.functionExample1 "Sort"
             msort
             [ ([9,5,4,18,3,2,1,78,12,32,4,-3,0,21,3,2,1,3], [-3,0,1,1,2,2,3,3,3,4,4,5,9,12,18,21,32,78])]
         ]
     )
    ]


main : Html Never
main =
    Html.div
        [ Html.Attributes.style "padding" "20px" ]
        [ ExerciseRunner.fontStyles
        , math1
            |> List.map (\( title, x ) -> ExerciseRunner.viewExampleSection title x)
            |> Html.div []
        ]






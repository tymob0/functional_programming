module Higher_Order exposing (..)

import Html exposing (Html)
import Basics exposing (toFloat,modBy,logBase)

-- your functions:

aboveVal: comparable -> comparable -> Bool
aboveVal target input =
    (input > target)

contains1: List Int -> Bool 
contains1 list =
    case list of
        [] -> False
        (x :: xs) -> x == 1

double: Int -> Int
double x =
    x * 2

exp: Float -> Float -> Float
exp base power =
    (logBase base ((base^power))) + 1

repeatUntil: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil predicate operation input =
    if (predicate input) then
        input
    else
        repeatUntil predicate operation (operation input)

myCollatz: List Int -> List Int
myCollatz list =
    case list of 
        [] -> []
        (x :: xs) ->
            if (modBy 2 x == 0) then
                x//2 :: x :: xs
            else
                1 + (3*x) :: x :: xs


-- collecting results for printing:

-- arbitrary list:
my_results: List String
my_results =
    [
        "repeatUntil (aboveVal 100) double 7 = " ++ pr (repeatUntil (aboveVal 100) double 7)++"\n\n",
        "log_3 100 = " ++ pr (repeatUntil (aboveVal (logBase 3 100)) (exp 3) 0) ++ "\n\n",
        "repeatUntil (aboveVal 100) ((+) 1) 42 = "++ pr (repeatUntil (aboveVal 100) ((+) 1) 42)++"\n\n",
        "repeatUntil for Collatz starting at 19 = "++ pr (repeatUntil contains1 myCollatz [19])++"\n\n"
    ] 
    
-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 250

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)
    

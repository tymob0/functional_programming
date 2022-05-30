module Modelling exposing (..)

-- steps:
--      install Elm: https://guide.elm-lang.org/install/elm.html
--      start a terminal
--      run "elm init"
--      copy this file to the new ./src directory
--      rename this file to your own name, and rename the module (see top line above) accordingly


import Html exposing (Html)
import Char exposing (toLower)
import List exposing (length)
import String exposing (fromInt,fromFloat)
import Basics exposing (modBy,abs)
import Tuple exposing (first, second)

--




-- your functions:

type Function
    = Poly Function Float
    | Mult Function Function
    | Div Function Function
    | Plus Function Function
    | Minus Function Function
    | Const Float
    | X

print: Function -> String
print func = 
    case func of
        Poly base power -> "("++(print base)++"^"++fromFloat power++")"
        Mult left right -> "("++(print left) ++ "*" ++ (print right)++")"
        Div top bottom -> "("++(print top)++")" ++ "/" ++ "("++(print bottom)++")"
        Plus left right -> "("++(print left) ++ "+" ++ (print right)++")"
        Minus left right-> "("++(print left) ++ "-" ++ (print right)++")"
        Const value -> fromFloat value 
        X -> "x"

eval: Float -> Function -> Float
eval x func =
    case func of
        Poly base power -> (eval x base) ^ power
        Mult left right -> (eval x left) * (eval x right)
        Div top bottom ->  (eval x top) / (eval x bottom)
        Plus left right -> (eval x left) + (eval x right)
        Minus left right-> (eval x left) - (eval x right)
        Const value -> value 
        X -> x

graph: Function -> Float -> Float -> Float -> Float -> String
graph func xmin xmax ymin ymax =
    if xmax == xmin then
        ""
    else
        (drawLine (eval xmin func) ymax ymin)++ "\n" ++ graph func (xmin + 1) xmax ymin ymax

drawLine: Float -> Float -> Float -> String
drawLine value  upper current =
    if current < value && current <= upper then
        "*" ++ (drawLine value upper (current+1))
    else if current <= upper then
        "-" ++ (drawLine value upper (current+1))
    else 
        ""


derivative: Function -> Function
derivative func =
    case func of
        Poly base power -> Mult (Mult (Const power) (Poly base (power - 1))) (derivative base)
        Mult left right -> Plus (Mult (derivative left) right) (Mult left (derivative right))
        Div top bottom -> Div (Minus (Mult bottom (derivative top)) (Mult top (derivative bottom))) (Poly bottom 2)
        Plus left right -> Plus (derivative left) (derivative right)
        Minus left right-> Minus (derivative left) (derivative right)
        Const value -> Const 0
        X -> Const 1


simplify: Function -> Function
simplify func =
    case func of
        Plus (Const left) (Const right) -> Const(left+right)
        Plus left right ->
            if ( simplify left ) == Const 0 then
                simplify right
            else if (simplify right) == Const 0 then
                simplify left
            else
                Plus (simplify left) (simplify right)

        Minus (Const left) (Const right) -> Const(left-right)
        Minus left right ->
            if ( simplify left ) == Const 0 then
                simplify right
            else if (simplify right) == Const 0 then
                simplify left
            else
                Minus (simplify left) (simplify right)

        Mult (Const left) (Const right) -> Const(left*right)
        Mult left right ->
            if ( simplify left ) == Const 0 || (simplify right) == Const 0 then
                Const 0
            else if ( simplify left ) == Const 1 then
                simplify right
            else if (simplify right) == Const 1 then
                simplify left
            else
                Mult (simplify left) (simplify right)

        Div (Const a) (Const b) -> Const(a/b)
        Div top bottom ->
            if (simplify bottom) == Const 1 then
                simplify top
            else
                Div (simplify top) (simplify bottom)

        Poly (Const base) (power) -> Const(base^power)
        Poly base power ->
            if power == 0 then
                Const 1
            else if power == 1 then
                simplify base
            else
                Poly (simplify base) power

        Const value -> Const value
        X -> X







-- collecting results for printing:





-- arbitrary list:
f = Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2)
g = (Plus (Minus (Poly (Minus (Div (X) (Const 5)) (Const 1)) 4) (Poly (Plus (Div (X) (Const -2)) (Const 2)) 2))(Const 6))
my_results: List String
my_results =
    [
        --(Minus (Poly (Minus (Div (X) (Const 5)) (Const 1)) 4) (Plus (Poly (Plus (Div (X) (Const -2)) (Const 2)) (Const 2)) (Const 6)))
        "f(x) = " ++ print f ++"\n\n",
        "f'(x) = "++print (derivative f)++"\n\n",
        "simplified f'(x) = "++print (simplify (derivative f))++"\n\n",
        "\n\n",
        "g(x) = " ++ print g ++"\n\n",
        "g'(x) = "++print (derivative g)++"\n\n",
        "simplified g'(x) = "++print (simplify (derivative g))++"\n\n"
    ] 
    
-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 5000

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
    
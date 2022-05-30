module Graph exposing (..)

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
drawLine value upper current =
    if current < value && current <= upper then
        "◾" ++ (drawLine value upper (current+1))
    else if current <= upper then
        "◽" ++ (drawLine value upper (current+1))
    else 
        ""


-- arbitrary list:
f = Minus p0 p1
p0 = Div p2 p3
p1 = Div p4 (Const 3)
p2 = Poly (Minus (Mult (Const 3) X) (Const 2)) 2
p3 = Poly (Plus X (Const 7)) 3
p4 = Plus (Mult (Const 2) X) (Const 5)

g = (Plus (Minus (Poly (Minus (Div (X) (Const 5)) (Const 1)) 4) (Poly (Plus (Div (X) (Const -2)) (Const 2)) 2))(Const 6))

h = (Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2))

my_results: List String
my_results =
    [
        "h = (Plus (Mult (Plus (Const 3) X) (Minus X (Poly X 5))) (Const 2))",
        "\n\n",
        "(print h)" ++ " Results in: " ++ (print h) ++ "     ✅",
        "\n\n",
        "(eval 2 h)" ++ " Results in: " ++ to_wrap(Debug.toString(eval 2.0 h)) ++ "     ✅",
        "\n\n\n\n",
        "Graph of g(x) = (x/5 - 1) ^ 4 – ((x/-2) + 2) ^ 2 + 6:\n\n",
        (graph g -10 20 -10 10),
        "\n\n\n\n",
        "Graph of f(x) = (((((3*x)-2)^2)/((x+7)^3))-(((2*x)+5)/3)):\n\n",
        (graph f -20 20 -20 20)
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
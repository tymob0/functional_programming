module Pythagoras exposing (isTriple,pythTriple,isTripleTuple)



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

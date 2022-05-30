module Caesar exposing
    ( encodeChar,
    decodeChar
    )


import Char exposing (fromCode, isLower, isUpper, toCode)
import Basics exposing (modBy)

--
-- Caesar
--

-- Encryption formula with modulo operator.

shift_char: Int -> Char -> Char -> Char
shift_char offset char bound =
    fromCode (modBy 26 (toCode char - toCode bound + offset) + toCode bound)

-- Depending on the lower/upper case pass different 'bound' symbol.
encodeChar: Int -> Char -> Char
encodeChar offset char =
    if isLower char then
        shift_char offset char 'a'
    else if isUpper char then
        shift_char offset char 'A'
    else
        char

-- Reuse encode function with negative offset.
decodeChar: Int -> Char -> Char
decodeChar offset char =
    encodeChar (0 - offset) char
module Caesar exposing
    ( encodeChar,
    decodeChar,
    encode,
    decode,
    sanitize,
    normalize
    )


import Char exposing (fromCode, isLower, isUpper, toCode)
import Basics exposing (modBy)
import List exposing (filter, length, map)
import Char exposing (fromCode, toCode, isUpper, isLower) 
import String exposing (fromChar, fromList, toList)

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
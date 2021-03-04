{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Applicative
import Course.Core
import Course.Functor
import Course.List
import Course.Monad
import Course.Optional

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh
          [ const "",
            const "un",
            const "do",
            const "tre",
            const "quattuor",
            const "quin",
            const "sex",
            const "septen",
            const "octo",
            \q -> if "n" `isPrefixOf` q then "novem" else "noven"
          ]
      postillion ::
        List Chars
      postillion =
        listh
          [ "vigintillion",
            "trigintillion",
            "quadragintillion",
            "quinquagintillion",
            "sexagintillion",
            "septuagintillion",
            "octogintillion",
            "nonagintillion",
            "centillion",
            "decicentillion",
            "viginticentillion",
            "trigintacentillion",
            "quadragintacentillion",
            "quinquagintacentillion",
            "sexagintacentillion",
            "septuagintacentillion",
            "octogintacentillion",
            "nonagintacentillion",
            "ducentillion",
            "deciducentillion",
            "vigintiducentillion",
            "trigintaducentillion",
            "quadragintaducentillion",
            "quinquagintaducentillion",
            "sexagintaducentillion",
            "septuagintaducentillion",
            "octogintaducentillion",
            "nonagintaducentillion",
            "trecentillion",
            "decitrecentillion",
            "vigintitrecentillion",
            "trigintatrecentillion",
            "quadragintatrecentillion",
            "quinquagintatrecentillion",
            "sexagintatrecentillion",
            "septuagintatrecentillion",
            "octogintatrecentillion",
            "nonagintatrecentillion",
            "quadringentillion",
            "deciquadringentillion",
            "vigintiquadringentillion",
            "trigintaquadringentillion",
            "quadragintaquadringentillion",
            "quinquagintaquadringentillion",
            "sexagintaquadringentillion",
            "septuagintaquadringentillion",
            "octogintaquadringentillion",
            "nonagintaquadringentillion",
            "quingentillion",
            "deciquingentillion",
            "vigintiquingentillion",
            "trigintaquingentillion",
            "quadragintaquingentillion",
            "quinquagintaquingentillion",
            "sexagintaquingentillion",
            "septuagintaquingentillion",
            "octogintaquingentillion",
            "nonagintaquingentillion",
            "sescentillion",
            "decisescentillion",
            "vigintisescentillion",
            "trigintasescentillion",
            "quadragintasescentillion",
            "quinquagintasescentillion",
            "sexagintasescentillion",
            "septuagintasescentillion",
            "octogintasescentillion",
            "nonagintasescentillion",
            "septingentillion",
            "deciseptingentillion",
            "vigintiseptingentillion",
            "trigintaseptingentillion",
            "quadragintaseptingentillion",
            "quinquagintaseptingentillion",
            "sexagintaseptingentillion",
            "septuagintaseptingentillion",
            "octogintaseptingentillion",
            "nonagintaseptingentillion",
            "octingentillion",
            "decioctingentillion",
            "vigintioctingentillion",
            "trigintaoctingentillion",
            "quadragintaoctingentillion",
            "quinquagintaoctingentillion",
            "sexagintaoctingentillion",
            "septuagintaoctingentillion",
            "octogintaoctingentillion",
            "nonagintaoctingentillion",
            "nongentillion",
            "decinongentillion",
            "vigintinongentillion",
            "trigintanongentillion",
            "quadragintanongentillion",
            "quinquagintanongentillion",
            "sexagintanongentillion",
            "septuagintanongentillion",
            "octogintanongentillion",
            "nonagintanongentillion"
          ]
   in listh
        [ "",
          "thousand",
          "million",
          "billion",
          "trillion",
          "quadrillion",
          "quintillion",
          "sextillion",
          "septillion",
          "octillion",
          "nonillion",
          "decillion",
          "undecillion",
          "duodecillion",
          "tredecillion",
          "quattuordecillion",
          "quindecillion",
          "sexdecillion",
          "septendecillion",
          "octodecillion",
          "novemdecillion"
        ]
        ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit
  = Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit ::
  Digit ->
  Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3
  = D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq)

-- Possibly convert a character to a digit.
fromChar ::
  Char ->
  Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

digitToDigit3 ::
  List Digit ->
  List Digit3
digitToDigit3 (d1 :. Nil) =
  D1 d1 :. Nil
digitToDigit3 (d2 :. (d1 :. Nil)) =
  D2 d1 d2 :. Nil
digitToDigit3 (d3 :. (d2 :. (d1 :. Nil))) =
  D3 d1 d2 d3 :. Nil
digitToDigit3 (d3 :. (d2 :. (d1 :. t))) =
  D3 d1 d2 d3 :. digitToDigit3 t
digitToDigit3 _ =
  Nil

digit3ToChars ::
  List Digit3 ->
  List Chars
digit3ToChars (d :. t) =
  case d of
    D1 d1 -> showDigit d1 :. digit3ToChars t
    D2 d2 d1 -> case d2 of
      Zero -> showDigit d1 :. digit3ToChars t
      One -> case d1 of
        Zero -> "ten" :. digit3ToChars t
        One -> "eleven" :. digit3ToChars t
        Two -> "twelve" :. digit3ToChars t
        Three -> "thirteen" :. digit3ToChars t
        Four -> "fourteen" :. digit3ToChars t
        Five -> "fifteen" :. digit3ToChars t
        Six -> "sixteen" :. digit3ToChars t
        Seven -> "seventeen" :. digit3ToChars t
        Eight -> "eighteen" :. digit3ToChars t
        Nine -> "nineteen" :. digit3ToChars t
      Two -> case d1 of
        Zero -> "twenty" :. digit3ToChars t
        _ -> ("twenty-" ++ showDigit d1) :. digit3ToChars t
      Three -> case d1 of
        Zero -> "thirty" :. digit3ToChars t
        _ -> ("thirty-" ++ showDigit d1) :. digit3ToChars t
      Four -> case d1 of
        Zero -> "forty" :. digit3ToChars t
        _ -> ("forty-" ++ showDigit d1) :. digit3ToChars t
      Five -> case d1 of
        Zero -> "fifty" :. digit3ToChars t
        _ -> ("fifty-" ++ showDigit d1) :. digit3ToChars t
      Six -> case d1 of
        Zero -> "sixty" :. digit3ToChars t
        _ -> ("sixty-" ++ showDigit d1) :. digit3ToChars t
      Seven -> case d1 of
        Zero -> "seventy" :. digit3ToChars t
        _ -> ("seventy-" ++ showDigit d1) :. digit3ToChars t
      Eight -> case d1 of
        Zero -> "eighty" :. digit3ToChars t
        _ -> ("eighty-" ++ showDigit d1) :. digit3ToChars t
      Nine -> case d1 of
        Zero -> "ninety" :. digit3ToChars t
        _ -> ("ninety-" ++ showDigit d1) :. digit3ToChars t
    D3 d3 d2 d1 -> case (d3, d2, d1) of
      (Zero, Zero, Zero) -> "" :. digit3ToChars t
      (Zero, Zero, _) -> flatten (digit3ToChars (D1 d1 :. Nil)) :. digit3ToChars t
      (Zero, _, _) -> flatten (digit3ToChars (D2 d2 d1 :. Nil)) :. digit3ToChars t
      (d3', Zero, Zero) -> (showDigit d3' ++ " hundred") :. digit3ToChars t
      _ -> (showDigit d3 ++ " hundred and " ++ flatten (digit3ToChars (D2 d2 d1 :. Nil))) :. digit3ToChars t
digit3ToChars _ =
  Nil

digit3ToChar ::
  Digit3 ->
  Chars
digit3ToChar d3 =
  flatten (digit3ToChars (d3 :. Nil))

charsToDigits ::
  Chars ->
  List Digit
charsToDigits (c :. cs) =
  case fromChar c of
    Full d -> d :. charsToDigits cs
    _ -> charsToDigits cs
charsToDigits _ =
  Nil

split ::
  Chars ->
  (Chars, Chars)
split =
  break (== '.')

cents ::
  Chars ->
  Optional Digit3
cents cs =
  let takeCents ::
        Chars ->
        Int ->
        List Digit
      takeCents (h :. t) n =
        if n == 0
          then Nil
          else case fromChar h of
            Full d -> d :. takeCents t (n - 1)
            _ -> takeCents t n
      takeCents _ _ =
        Nil
   in case digitToDigit3 (reverse (takeCents cs 2)) of
        (D1 d1 :. _) -> Full (D2 d1 Zero)
        (d3 :. _) -> Full d3
        _ -> Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred
-- and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred
-- and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred
-- and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred
-- and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred
-- and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred
-- and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred
-- and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars ->
  Chars
dollars cs =
  let dc = split cs
   in case (digit3ToChars (digitToDigit3 (reverse (charsToDigits (fst dc)))), cents (snd dc)) of
        (Nil, Empty) -> "zero dollars and zero cents"
        (Nil, Full (D1 One)) -> "zero dollars and one cent"
        (Nil, Full (D2 Zero One)) -> "zero dollars and one cent"
        (Nil, Full c) -> "zero dollars and " ++ digit3ToChar c ++ " cents"
        ("one" :. Nil, Full (D1 One)) -> "one dollar and one cent"
        ("one" :. Nil, Full (D2 Zero One)) -> "one dollar and one cent"
        ("one" :. Nil, Empty) -> "one dollar and zero cents"
        ("one" :. Nil, Full c) -> "one dollar and " ++ digit3ToChar c ++ " cents"
        (h :. Nil, Empty) -> h ++ " dollars and zero cents"
        (h :. Nil, Full c) -> h ++ " dollars and " ++ digit3ToChar c ++ " cents"
        (xs, Empty) ->
          flatten
            ( reverse
                ( zipWith
                    ( \a b ->
                        if isEmpty a
                          then ""
                          else if isEmpty b then a ++ " " else a ++ " " ++ b ++ " "
                    )
                    xs
                    illion
                )
            )
            ++ "dollars and zero cents"
        (xs, Full c) ->
          flatten
            ( reverse
                ( zipWith
                    ( \a b ->
                        if isEmpty a
                          then ""
                          else if isEmpty b then a ++ " " else a ++ " " ++ b ++ " "
                    )
                    xs
                    illion
                )
            )
            ++ "dollars and "
            ++ digit3ToChar c
            ++ " cents"

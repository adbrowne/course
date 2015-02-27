{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Apply
import Course.Bind
import Course.Parser

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Enum, Bounded, Show)

showDigit ::
  Digit
  -> Chars
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
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving (Eq, Show)


-- | Group digits into threes
--
-- >>> toDigit3 $ listh [Zero]
-- [D1 Zero]
-- >>> toDigit3 $ listh [One,Zero]
-- [D2 One Zero]
-- >>> toDigit3 $ listh [One,Zero,Zero]
-- [D3 One Zero Zero]
-- >>> toDigit3 $ listh [One,Zero,Zero,Zero]
-- [D1 One,D3 Zero Zero Zero]
toDigit3 :: List Digit -> List Digit3
toDigit3 xs =
   let
     revxs = reverse xs
     collectDigits :: List Digit -> List Digit3
     collectDigits Nil = Nil
     collectDigits (x :. Nil) = D1 x :. Nil
     collectDigits (x :. y :. Nil) = D2 y x :. Nil
     collectDigits (x :. y :. z :. ys) = (D3 z y x) :. (collectDigits ys)
   in
     reverse $ collectDigits revxs
     
-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
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

parseAmount :: Chars -> (List Digit, List Digit)
parseAmount cs =
  let
    digitFilter (Full x) = x :. Nil
    digitFilter Empty = Nil

    value chars =
      case parse parseWhole chars of
      Result _ (d, c) ->
          (d >>= digitFilter, c >>= digitFilter)
      ErrorResult _ -> (Nil,Nil)
  in value cs
  where
    notPeriod = satisfy (/= '.')
    parseWhole :: Parser (List (Optional Digit), List (Optional Digit))
    parseWhole =
      parseDigit >>= \d ->
      (centParser ||| valueParser Nil) >>= \c ->
      valueParser (d, c)
    parseDigit =
      list $ mapParser fromChar notPeriod
    parseAllDigits =
      list $ mapParser fromChar character
    centParser =
      (is '.') >>> parseAllDigits -- ignore any more periods
    
digitsToWords :: List Digit -> Chars
digitsToWords Nil = "zero"
digitsToWords (x :. Nil) = showDigit x
digitsToWords (x :. xs) = showDigit x ++ " " ++ (digitsToWords xs)

roundCents :: List Digit -> List Digit
roundCents Nil = Nil
roundCents (Zero :. Nil) = Zero :. Nil
roundCents (Zero :. y :. Nil) = y :. Nil
roundCents (x :. Nil) = x :. Zero :. Nil
roundCents (Zero :. Zero :. xs) = Zero :. Nil
roundCents (x :. y :. xs) = x :. y :. Nil

showTens prefix digit suffix = prefix ++ "-" ++ showDigit digit ++ " " ++ suffix

showTeens Zero suffix = "ten " ++ suffix
showTeens One suffix = "eleven " ++ suffix
showTeens Two suffix = "twelve " ++ suffix
showTeens Three suffix = "thirteen " ++ suffix
showTeens Four suffix = "fourteen " ++ suffix
showTeens Five suffix = "fifteen " ++ suffix
showTeens Six suffix = "sixteen " ++ suffix
showTeens Seven suffix = "seventeen " ++ suffix
showTeens Eight suffix = "eighteen " ++ suffix
showTeens Nine suffix = "nineteen " ++ suffix

digitsWithSuffix suffix _ (One :. Nil) = showDigit One ++ " " ++ suffix
digitsWithSuffix _ pluralSuffix (One :. y :. Nil) = showTeens y pluralSuffix
digitsWithSuffix _ pluralSuffix (Two :. y :. Nil) = showTens "twenty" y pluralSuffix
digitsWithSuffix _ pluralSuffix (Three :. y :. Nil) = showTens "thirty" y pluralSuffix
digitsWithSuffix _ pluralSuffix (Four :. y :. Nil) = showTens "forty" y pluralSuffix
digitsWithSuffix _ pluralSuffix (Five :. y :. Nil) = showTens "fifty" y pluralSuffix
digitsWithSuffix _ pluralSuffix (Six :. y :. Nil) = showTens "sixty" y pluralSuffix
digitsWithSuffix _ pluralSuffix (Seven :. y :. Nil) = showTens "seventy" y pluralSuffix
digitsWithSuffix _ pluralSuffix (Eight :. y :. Nil) = showTens "eighty" y pluralSuffix
digitsWithSuffix _ pluralSuffix (Nine :. y :. Nil) = showTens "ninety" y pluralSuffix
digitsWithSuffix _ pluralSuffix (x :. Zero :. Zero :. Nil) = (showDigit x) ++ " hundred " ++ pluralSuffix
digitsWithSuffix _ pluralSuffix (x :. y :. z :. Nil) = (showDigit x) ++ " hundred and " ++ digitsWithSuffix pluralSuffix pluralSuffix (y :. z :. Nil)
digitsWithSuffix _ pluralSuffix (xs) = digitsToWords xs ++ " " ++ pluralSuffix


print10 prefix Zero = prefix
print10 prefix digit = prefix ++ "-" ++ showDigit digit

-- | Print up to three digits
-- 
-- >>> digit3ToChars (D2 Two Two)
-- "twenty-two"
-- >>> digit3ToChars (D3 Two Zero Zero)
-- "two hundred"
digit3ToChars :: Digit3 -> Chars
digit3ToChars (D1 a) = showDigit a
digit3ToChars (D2 One Zero) = "ten"
digit3ToChars (D2 One One) = "eleven"
digit3ToChars (D2 One Two) = "twelve"
digit3ToChars (D2 One Three) = "thirteen"
digit3ToChars (D2 One Four) = "fourteen"
digit3ToChars (D2 One Five) = "fifteen"
digit3ToChars (D2 One Six) = "sixteen"
digit3ToChars (D2 One Seven) = "seventeen"
digit3ToChars (D2 One Eight) = "eighteen"
digit3ToChars (D2 Zero a) = digit3ToChars (D1 a)
digit3ToChars (D2 One Nine) = "nineteen"
digit3ToChars (D2 Two a) = print10 "twenty" a
digit3ToChars (D2 Three a) = print10 "thirty" a
digit3ToChars (D2 Four a) = print10 "forty" a
digit3ToChars (D2 Five a) = print10 "fifty" a
digit3ToChars (D2 Six a) = print10 "sixty" a
digit3ToChars (D2 Seven a) = print10 "seventy" a
digit3ToChars (D2 Eight a) = print10 "eighty" a
digit3ToChars (D2 Nine a) = print10 "ninety" a
digit3ToChars (D3 Zero a b) = digit3ToChars (D2 a b)
digit3ToChars (D3 a Zero Zero) = (showDigit a) ++ " hundred"
digit3ToChars (D3 a b c) = (showDigit a) ++ " hundred and " ++ (digit3ToChars (D2 b c))


-- | Group into illion groups
-- 
-- >>> [("thousand",D1 One),("",D3 Two Three Four)]
-- [("thousand",D1 One),("",D3 Two Three Four)]
-- >>> groupIllions $ listh [One]
-- [("",D1 One)]
groupIllions :: List Digit -> List (Chars, Digit3)
groupIllions = (reverse . zip illion . reverse . toDigit3)

intercalate :: Chars -> List Chars -> Chars
intercalate _ Nil = ""
intercalate _ (x :. Nil) = x
intercalate s (x :. xs) = x ++ s ++ (intercalate s xs)

appendSuffix :: Chars -> Chars -> Chars
appendSuffix Nil xs = xs
appendSuffix suffix xs = xs ++ " " ++ suffix

printIllions :: List (Chars, Digit3) -> Chars
printIllions xs = intercalate " " $ map (\(s,d) -> appendSuffix s (digit3ToChars d)) $ filter nonZero xs
  where 
   nonZero (_,(D3 Zero Zero Zero)) = False
   nonZero (_,(D2 Zero Zero)) = False
   nonZero (_,(D1 Zero)) = False
   nonZero _ = True

digitListToString :: List Digit -> Chars
digitListToString Nil = "zero"
digitListToString (Zero :. Nil) = "zero"
digitListToString xs = printIllions . groupIllions $ xs

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
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars cs =
  let
    (d, c) = parseAmount cs
    cents = roundCents c
  in
   (digitListToString d) ++ " " ++ (suffix d "dollar" "dollars") ++ " and " ++ (digitListToString cents) ++ " " ++ (suffix cents "cent" "cents")
  where 
   suffix (One :. Nil) singular _ = singular
   suffix _ _ plural = plural

{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Text.Mock (styles, version) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import Data.List
import Data.Hashable
import System.Random

-- |Version string
version :: Text
version = "3.7.0"

-- | List of all mock styles as tuples of the name and the transformation function.
styles :: [(Text, Text -> Text)]
styles =
    [ ("random", mockRandom)
    , ("alternate", mockAlternate)
    , ("alternate2", mockAlternate . T.toLower)
    , ("strike", mockStrike)
    , ("double", T.map toDouble)
    , ("dedouble", T.map fromDouble)
    , ("smallcaps", T.map toSmallCap)
    , ("lower", T.toLower)
    , ("upper", T.toUpper)
    , ("cyrillic", T.map toCyrillic)
    , ("fraktur", T.map toFraktur)
    , ("subsuper", mockSubSuper)
    , ("cc", mockCC)
    , ("b", mockB)
    , ("pray", T.unwords . intersperse "🙏" . T.words)
    , ("clap", T.unwords . intersperse "👏" . T.words)
    , ("space", mockSpace 1)
    , ("space2", mockSpace 2)
    , ("space3", mockSpace 3)
    , ("lines", T.intersperse '\n')
    , ("wordlines", T.concat . intersperse "\n" . T.words)
    , ("square", mockSquare)
    ]

-- | Transforms characters of a string into uppercase where the corresponding element of the bool list is true. On encountering a letter that already is uppercase the mask is reversed.
toUpperBy :: [Bool] -> Text -> Text
toUpperBy mask' = fst . T.foldl f ("", mask') where
    f :: (Text, [Bool]) -> Char -> (Text, [Bool])
    f (txt, bit:mask) char
        | isUpper char = (txt `T.snoc` char, map not mask)
        | isSpace char = (txt `T.snoc` char, bit:mask)
    f (txt, True:mask) char = (txt `T.snoc` toUpper char, mask)
    f (txt, False:mask) char = (txt `T.snoc` char, mask)
    f (txt, []) char = (txt `T.snoc` char, [])  -- If the mask is empty, treat is as all false

-- | Transforms every other of the characters of a string into uppercase. The other characters aren't changed.
mockAlternate :: Text -> Text
mockAlternate = toUpperBy (intersperse True $ repeat False)

-- | Tansforms random (that is, pseudo-random per input) characters of a string into uppercase.
mockRandom :: Text -> Text
mockRandom txt = toUpperBy (randoms $ mkStdGen (hash txt)) txt

-- | Letterspaces a String with the given number of blanks between each character.
mockSpace :: Int -> Text -> Text
mockSpace n = T.pack . intercalate (replicate n ' ') . map (:[]) . T.unpack

-- | Transforms characters into their double-struck variant if available.
toDouble :: Char -> Char
toDouble 'C' = chr 8450
toDouble 'H' = chr 8461
toDouble 'N' = chr 8469
toDouble 'P' = chr 8473
toDouble 'Q' = chr 8474
toDouble 'R' = chr 8477
toDouble 'Z' = chr 8484
toDouble c
    | 48 <= ord c && ord c <= 57 =  chr $ ord c - 48 + 120792  -- Number
    | 65 <= ord c && ord c <= 90 =  chr $ ord c - 65 + 120120  -- Uppercase letter
    | 97 <= ord c && ord c <= 122 = chr $ ord c - 97 + 120146  -- Lowercase letter
toDouble c = c

-- | Transforms double-struck characters back into their normal variant.
fromDouble :: Char -> Char
fromDouble c = case ord c of
    8450 -> 'C'
    8461 -> 'H'
    8469 -> 'N'
    8473 -> 'P'
    8474 -> 'Q'
    8477 -> 'R'
    8484 -> 'Z'
    code
        | 120792 <= code && code <= 120801 -> chr $ code - 120792 + 48
        | 120120 <= code && code <= 120145 -> chr $ code - 120120 + 65
        | 120146 <= code && code <= 120171 -> chr $ code - 120146 + 97
    code -> chr code

-- | Transforms characters into sub- and superscript alternatingly.
mockSubSuper :: Text -> Text
mockSubSuper txt = T.pack $ zipWith toSubSuper (intersperse True $ repeat False) (T.unpack txt)

-- | Transforms a character into a unicode sub- or superscript variant. If true is given and a subscript version is available, that is used. If none is available or false is given, a superscript version is used. If none is available, the character is left unchanged.
toSubSuper :: Bool -> Char -> Char
toSubSuper = curry $ \case
    (_, 'A') -> chr 7468
    (_, 'B') -> chr 7470
    (_, 'D') -> chr 7472
    (_, 'E') -> chr 7473
    (_, 'G') -> chr 7475
    (_, 'H') -> chr 7476
    (_, 'I') -> chr 7477
    (_, 'J') -> chr 7478
    (_, 'K') -> chr 7479
    (_, 'L') -> chr 7480
    (_, 'M') -> chr 7481
    (_, 'N') -> chr 7482
    (_, 'O') -> chr 7484
    (_, 'P') -> chr 7486
    (_, 'R') -> chr 7487
    (_, 'T') -> chr 7488
    (_, 'U') -> chr 7489
    (_, 'V') -> chr 11389
    (_, 'W') -> chr 7490
    (False, 'a') -> 'ᵃ'
    (True, 'a') -> 'ₐ'
    (_, 'b') -> 'ᵇ'
    (_, 'c') -> 'ᶜ'
    (_, 'd') -> 'ᵈ'
    (False, 'e') -> 'ᵉ'
    (True, 'e') -> 'ₑ'
    (_, 'f') -> 'ᶠ'
    (_, 'g') -> 'ᵍ'
    (False, 'h') -> 'ʰ'
    (True, 'h') -> 'ₕ'
    (False, 'i') -> 'ⁱ'
    (True, 'i') -> 'ᵢ'
    (False, 'j') -> 'ʲ'
    (True, 'j') -> 'ⱼ'
    (False, 'k') -> 'ᵏ'
    (True, 'k') -> 'ₖ'
    (False, 'l') -> 'ˡ'
    (True, 'l') -> 'ₗ'
    (False, 'm') -> 'ᵐ'
    (True, 'm') -> 'ₘ'
    (False, 'n') -> 'ⁿ'
    (True, 'n') -> 'ₙ'
    (False, 'o') -> 'ᵒ'
    (True, 'o') -> 'ₒ'
    (False, 'p') -> 'ᵖ'
    (True, 'p') -> 'ₚ'
    (False, 'r') -> 'ʳ'
    (True, 'r') -> 'ᵣ'
    (False, 's') -> 'ˢ'
    (True, 's') -> 'ₛ'
    (False, 't') -> 'ᵗ'
    (True, 't') -> 'ₜ'
    (False, 'u') -> 'ᵘ'
    (True, 'u') -> 'ᵤ'
    (False, 'v') -> 'ᵛ'
    (True, 'v') -> 'ᵥ'
    (_, 'w') -> 'ʷ'
    (False, 'x') -> 'ˣ'
    (True, 'x') -> 'ₓ'
    (_, 'y') -> 'ʸ'
    (_, 'z') -> 'ᶻ'
    (_, c) -> c

-- | Transforms lowercase characters into their unicode small capital variants.
toSmallCap :: Char -> Char
toSmallCap = \case
    'a' -> chr 7424
    'b' -> chr 665
    'c' -> chr 7428
    'd' -> chr 7429
    'e' -> chr 7431
    'f' -> chr 42800
    'g' -> chr 610
    'h' -> chr 668
    'i' -> chr 618
    'j' -> chr 7434
    'k' -> chr 7435
    'l' -> chr 671
    'm' -> chr 7437
    'n' -> chr 628
    'o' -> chr 7439
    'p' -> chr 7448
    'q' -> chr 491
    'r' -> chr 640
    's' -> chr 42801
    't' -> chr 7451
    'u' -> chr 7452
    'v' -> chr 7456
    'w' -> chr 7457
    'y' -> chr 655
    'z' -> chr 7458
    c -> c

-- | Replaces some characters with cyrillic ones *looking* similarly.
toCyrillic :: Char -> Char
toCyrillic = \case
    'A' -> 'Д'
    'B' -> 'Б'
    'E' -> 'З'
    'N' -> 'И'
    'O' -> 'Ө'
    'R' -> 'Я'
    'U' -> 'Ц'
    'W' -> 'Щ'
    'X' -> 'Ж'
    'a' -> 'д'
    'b' -> 'в'
    'e' -> 'ё'
    'h' -> 'Ђ'
    'i' -> 'ɪ'
    'k' -> 'к'
    'o' -> 'ө'
    'r' -> 'я'
    't' -> 'т'
    'u' -> 'ц'
    'y' -> 'џ'
    c -> c

-- | Replaces all occurences of *lowercase* "g", "ck" and "k" in a string with "cc".
mockCC :: Text -> Text
mockCC = T.replace "k" "cc" . T.replace "ck" "cc"

-- | Replaces all occurences of "b", "B", "p" and "P" with B button emojis.
mockB :: Text -> Text
mockB = T.replace "b" "🅱️" . T.replace "B" "🅱️"

-- | Makes a square of a string by putting it with spaces in the first line and then all characters except the first in single lines after that first line.
mockSquare :: Text -> Text
mockSquare text = T.concat [T.intersperse ' ' text, "\n", T.intercalate "\n" (T.chunksOf 1 $ T.tail text)]

-- | Uses unicode U+0336 to let a text look struck through.
mockStrike :: Text -> Text
mockStrike text
    | text == T.empty = T.empty
    | otherwise = T.intersperse '\822' text `T.append` "\822"

toFraktur :: Char -> Char
toFraktur = \case  -- special cases with letter code out of order
    'C' -> 'ℭ'
    'H' -> 'ℌ'
    'I' -> 'ℑ'
    'R' -> 'ℜ'
    'Z' -> 'ℨ'
    c
        | 65 <= ord c && ord c <= 90  -> chr $ 120068 + (ord c - 65)  -- upper case letters
        | 97 <= ord c && ord c <= 122 -> chr $ 120094 + (ord c - 97)  -- lower case letters
        | otherwise -> c

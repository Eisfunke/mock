{-# LANGUAGE OverloadedStrings #-}

module Mock (styles, mockAlternate, mockRandom, letterspace, toDouble) where

import Data.Char
import Data.List
import qualified Data.Text as T
import Data.Hashable
import System.Random


-- |List of possible mock style names and their functions.
styles :: [(T.Text, T.Text -> T.Text)]
styles = [
    ("random", mockRandom),
    ("alternate", mockAlternate),
    ("space", letterspace 1),
    ("space2", letterspace 2),
    ("space3", letterspace 3),
    ("lines", T.intersperse '\n'),
    ("upper", T.toUpper),
    ("lower", T.toLower),
    ("double", T.map toDouble),
    ("cc", mockCC)]

-- |Transforms a String into uppercase where the corresponding list is True. For False the String isn't changed.
toUpperBy :: [Bool] -> T.Text -> T.Text
toUpperBy bs = T.pack . zipWith f bs . T.unpack where
    f :: Bool -> Char -> Char
    f True c = toUpper c
    f False c = c

-- |Transforms every other of the Chars of a String into uppercase. The other Chars aren't changed.
mockAlternate :: T.Text -> T.Text
mockAlternate = toUpperBy $ intersperse True $ repeat False

-- |Tansforms random (that is, random per input String) Chars of a String into uppercase.
mockRandom :: T.Text -> T.Text
mockRandom txt = toUpperBy (randoms $ mkStdGen (hash txt)) txt

-- |Letterspaces a String with the given number of blanks between the Chars.
letterspace :: Int -> T.Text -> T.Text
letterspace n = T.pack . intercalate (replicate n ' ') . map (\c -> [c]) . T.unpack

-- |Transforms a character into its double-struck variant (if it is alphanumeric, else it is left unchanged).
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

-- |Replaces all occurences of lowercase "ck" and "k" in a string with "cc"s.
mockCC :: T.Text -> T.Text
mockCC = T.replace "k" "cc" . T.replace "ck" "cc"

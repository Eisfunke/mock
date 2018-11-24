module Mock (styles, mockAlternate, mockRandom, letterspace, toDS) where

import Data.Char
import Data.List
import Data.Hashable
import System.Random


-- |List of possible mock style names and their functions.
styles :: [(String, String -> String)]
styles = [
    ("random", mockRandom),
    ("alternate", mockAlternate),
    ("space", letterspace 1),
    ("space2", letterspace 2),
    ("space3", letterspace 3),
    ("lines", intersperse '\n'),
    ("upper", map toUpper),
    ("lower", map toLower),
    ("double", map toDS)]

-- |Transforms a String into uppercase where the corresponding list is True. For False the String isn't changed.
toUpperBy :: String -> [Bool] -> String
toUpperBy (c:cs) (True:bs) = toUpper c : toUpperBy cs bs
toUpperBy (c:cs) (False:bs) = c : toUpperBy cs bs
toUpperBy (cs) [] = cs
toUpperBy [] _ = []

-- |Transforms every other of the Chars of a String into uppercase. The other Chars aren't changed.
mockAlternate :: String -> String
mockAlternate str = toUpperBy str $ intersperse True $ repeat False

-- |Tansforms random (that is, random per input String) Chars of a String into uppercase.
mockRandom :: String -> String
mockRandom str = toUpperBy str (randoms $ mkStdGen (hash str))

-- |Letterspaces a String with the given number of blanks between the Chars.
letterspace :: Int -> String -> String
letterspace n = intercalate (replicate n ' ') . map (\c -> [c])

-- |Transforms a character into its double-struck variant (if it is alphanumeric, else it is left unchanged).
toDS :: Char -> Char
toDS 'C' = chr 8450
toDS 'H' = chr 8461
toDS 'N' = chr 8469
toDS 'P' = chr 8473
toDS 'Q' = chr 8474
toDS 'R' = chr 8477
toDS 'Z' = chr 8484
toDS c
    | 48 <= ord c && ord c <= 57 =  chr $ ord c - 48 + 120792  -- Number
    | 65 <= ord c && ord c <= 90 =  chr $ ord c - 65 + 120120  -- Uppercase letter
    | 97 <= ord c && ord c <= 122 = chr $ ord c - 97 + 120146  -- Lowercase letter
toDS c = c

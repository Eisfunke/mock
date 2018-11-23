module Mock (mockAlternate, mockRandom, letterspace) where

import Data.Char
import Data.List
import System.Random
import Data.Time.Clock.POSIX


-- |Transforms a String into uppercase where the corresponding list is True. For False the String isn't changed.
toUpperBy :: String -> [Bool] -> String
toUpperBy (c:cs) (True:bs) = toUpper c : toUpperBy cs bs
toUpperBy (c:cs) (False:bs) = c : toUpperBy cs bs
toUpperBy (cs) [] = cs
toUpperBy [] _ = []

-- |Transforms every other of the Chars of a String into uppercase. The other Chars aren't changed.
mockAlternate :: String -> String
mockAlternate str = toUpperBy str $ intersperse True $ repeat False

-- |Tansforms random Chars of a String into uppercase.
mockRandom :: String -> IO String
mockRandom str = do
    time <- fmap round getPOSIXTime
    return $ toUpperBy str $ randoms $ mkStdGen time

-- |Letterspaces a String with the given number of blanks between the Chars.
letterspace :: Int -> String -> String
letterspace n = intercalate (replicate n ' ') . map (\c -> [c])

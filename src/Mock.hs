module Mock (mockAlternate, mockRandom, mockSpace) where

import Data.Char
import Data.List
import System.Random
import Data.Time.Clock.POSIX


toUpperBy :: String -> [Bool] -> String
toUpperBy (c:cs) (True:bs) = toUpper c : toUpperBy cs bs
toUpperBy (c:cs) (False:bs) = c : toUpperBy cs bs
toUpperBy (cs) [] = cs
toUpperBy [] _ = []

mockAlternate :: String -> String
mockAlternate str = toUpperBy str $ intersperse True $ repeat False

mockRandom :: String -> IO String
mockRandom str = do
    time <- fmap round getPOSIXTime
    return $ toUpperBy str $ randoms $ mkStdGen time

mockSpace :: Int -> String -> String
mockSpace n = intercalate (replicate n ' ') . map (\c -> [c])

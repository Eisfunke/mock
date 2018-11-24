module Main where

import Mock (styles)
import Data.List
import Data.Char
import Data.Maybe
import System.Environment


-- |Main function.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn help
        ["--help"] -> putStrLn help
        [style] -> do
            input <- getContents -- Read from stdin
            putStrLn $ handle style [input]
        (style:text) -> putStrLn $ handle style text

-- |Returns an IO action handling the given list of arguments.
handle :: String -> [String] -> String
handle style = fromMaybe (const help) (lookup style styles) . dropWhileEnd isSpace . intercalate " "

-- |Help string.
help :: String
help = "Mock - a program to transform text.\n\
       \\n\
       \Usage: mock [STYLE] [TEXT]\n\
       \Styles: " ++ (intercalate ", " $ map fst styles)

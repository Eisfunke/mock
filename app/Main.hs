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
        [style] -> getContents >>= (\input -> handle [style, input])  -- Read from stdin
        _ -> handle args

-- |Returns an IO action handling the given list of arguments.
handle :: [String] -> IO ()
handle (style:text) = transform (dropWhileEnd isSpace (intercalate " " text)) >>= putStrLn where
    transform :: String -> IO String
    transform = case lookup style styles of  -- Lookup style name in styles list
        Just f -> f  -- Use the found style function
        Nothing -> const $ return help  -- If the style isn't found, always return just the help string

-- |Help string.
help :: String
help = "Mock - a program to transform text.\n\
       \\n\
       \Usage: mock [STYLE] [TEXT]\n\
       \Styles: " ++ (intercalate ", " $ map fst styles)

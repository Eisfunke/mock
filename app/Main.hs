module Main where

import Mock
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
handle (style:text) = transform (intercalate " " text) >>= putStrLn where
    transform :: String -> IO String
    transform = case lookup style styles of  -- Lookup style name in styles list
        Just f -> f  -- Use the found style function
        Nothing -> const $ return help  -- If the style isn't found, always return just the help string

-- |List of possible mock styles names and their functions.
styles :: [(String, String -> IO String)]
styles = [
    ("random", mockRandom),
    ("alternate", toIO mockAlternate),
    ("space", toIO $ mockSpace 1),
    ("space2", toIO $ mockSpace 2),
    ("space3", toIO $ mockSpace 3),
    ("upper", toIO $ map toUpper),
    ("lower", toIO $ map toLower)]

-- |Lifts a simple function into an IO operation simply returning what the function would return.
toIO :: (a -> b) -> (a -> IO b)
toIO f = \x -> return $ f x

-- |Help string.
help :: String
help = "Mock - a program to transform text.\n\
       \\n\
       \Usage: mock [STYLE] [TEXT]\n\
       \Styles: " ++ (intercalate ", " $ map fst styles)

module Main where

import Mock
import Data.List
import Data.Char
import Data.Maybe
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> putStrLn help
        1 -> getLine >>= (\text -> handle [head args, text])
        _ -> handle args

handle :: [String] -> IO ()
handle (style:text) = transform (intercalate " " text) >>= putStrLn where
    transform :: String -> IO String
    transform = case lookup style styles of
        Just f -> f
        Nothing -> const $ return help

styles :: [(String, String -> IO String)]
styles = [
    ("random", mockRandom),
    ("alternate", toIO mockAlternate),
    ("space", toIO $ mockSpace 1),
    ("space2", toIO $ mockSpace 2),
    ("space3", toIO $ mockSpace 3),
    ("upper", toIO $ map toUpper),
    ("lower", toIO $ map toLower)
    ]

toIO :: (a -> b) -> (a -> IO b)
toIO f = (\x -> return $ f x)

help :: String
help = "Mock - a program to transform text.\n\n\
       \Usage: mock [STYLE] [TEXT]\n\
       \Styles: " ++ (intercalate ", " $ map fst styles)

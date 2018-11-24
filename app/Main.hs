{-# LANGUAGE OverloadedStrings #-}

module Main where

import Mock (styles)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char
import Data.Maybe
import System.Environment


-- |Main function.
main :: IO ()
main = do
    args' <- getArgs
    let args = map T.pack args'  -- Transform String arguments into Text
    case args of
        [] -> T.putStrLn help
        ["--help"] -> T.putStrLn help
        [style] -> do
            input <- T.getContents  -- Read from stdin
            T.putStrLn $ handle style [input]
        (style:str) -> T.putStrLn $ handle style str

-- |Returns an IO action handling the given list of arguments.
handle :: T.Text -> [T.Text] -> T.Text
handle style = fromMaybe (const help) (lookup style styles) . T.dropWhileEnd isSpace . T.intercalate " "

-- |Help string.
help :: T.Text
help = T.unlines [
    "Mock 3.0.0 - a program to transform text.",
    "",
    "Usage: mock [STYLE] [TEXT]",
    "Styles: " `T.append` (T.intercalate ", " $ map fst styles)]

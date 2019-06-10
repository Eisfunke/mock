{-# LANGUAGE OverloadedStrings #-}

module Main where

import Mock (styles)
import Mock.Help (styleHelp)
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
    " ╔════════════════════╗",
    " ║     Mock 3.5.0     ║",
    " ╚════════════════════╝",
    "",
    "A Great PrOgrAM tO TRANsFoRM TEXt, wRiTten iN HaSKeLL.",
    "By Nicolas Lenz. Free and open source under the WTFPL.",
    "Webpage (source code, issues, pull requests): https://git.eisfunke.com/software/mock",
    "",
    "Usage: mock [STYLE] [TEXT]",
    "Help:  mock --help",
    "",
    "Styles: ",
    T.intercalate "\n" styleHelps] where
        styleHelps = map
            (\(name, _) -> T.concat ["  - ", name, ": ", T.replicate (maxNameLength - T.length name) " " , styleHelp name])
            styles
        maxNameLength = maximum . map (T.length . fst) $ styles

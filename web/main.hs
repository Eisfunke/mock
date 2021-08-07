{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, LambdaCase, ViewPatterns #-}

import qualified Text.Mock as Mock
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Yesod

data Mock = Mock
instance Yesod Mock

mkYesod "Mock" [parseRoutes|
    /mock MockR GET
|]

getMockR :: Handler T.Text
getMockR = do
    stylesText <- lookupGetParams "styles"
    let styles = fmap (\txt -> fromMaybe id (lookup txt Mock.styles)) stylesText
    lookupGetParam "text" >>= \case
        Just text -> return $ foldr (flip (.)) id styles text
        Nothing -> return ""

main :: IO ()
main = warp 8080 Mock

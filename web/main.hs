{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, LambdaCase, ViewPatterns #-}

import Text.Mock
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Yesod

data Mock = Mock
instance Yesod Mock

mkYesod "Mock" [parseRoutes|
    /#T.Text MockR GET
|]


getMockR :: T.Text -> Handler T.Text
getMockR style = do
    lookupGetParam "text" >>= \case
        Just text -> return $ (fromMaybe (const "") (lookup style styles)) text
        Nothing -> return ""

main :: IO ()
main = warp 8080 Mock

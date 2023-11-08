{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, LambdaCase, ViewPatterns #-}

import qualified Text.Mock as Mock
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Yesod.Core
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Handler.Warp (run)
import Text.Read (readMaybe)
import System.Environment (lookupEnv)

data Mock = Mock

mkYesod "Mock" [parseRoutes|
    /api/mock MockR GET
|]

instance Yesod Mock

getMockR :: Handler T.Text
getMockR = do
    -- addHeader "Access-Control-Allow-Origin" "*"
    stylesText <- lookupGetParams "styles"
    let styles = fmap (\txt -> fromMaybe id (lookup txt Mock.styles)) stylesText
    lookupGetParam "text" >>= \case
        Just text -> return $ foldr (flip (.)) id styles text
        Nothing -> return ""

main :: IO ()
main = do
    app <- toWaiApp Mock
    port <- (>>= readMaybe) <$> lookupEnv "MOCK_WEB_PORT"
    run (fromMaybe 8080 port) $ addHeaders [("Access-Control-Allow-Origin", "*")] app

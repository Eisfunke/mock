{-# LANGUAGE OverloadedStrings #-}

module Mock.Help (styleHelp) where

import qualified Data.Text as T
import Data.Text (Text)

styleHelp :: Text -> Text
styleHelp styleName = case styleName of
    "random" -> "Flips lowercase characters pseudo-randomly into uppercase letters."
    "alternate" -> "Flips every second letter into an uppercase one, starting with the second character."
    "alternate2" -> "Like alternate, but ignores case in the input. Equivalent to lower|alternate."
    "space" -> "Inserts a  s p a c e  between every two characters."
    "space2" -> "Inserts two   s  p  a  c  e  s   between every two characters."
    "space3" -> "Inserts three    s   p   a   c   e   s    between every two characters."
    "lines" -> "Puts each character on a single line."
    "upper" -> "Turns all characters into UPPERCASE ones."
    "lower" -> "Turns all characters into lowercase ones."
    "double" -> "Turns characters (latin letters and numbers) into their double-struck variants (𝕖𝕩𝕒𝕞𝕡𝕝𝕖). Also known as blackboard bold."
    "dedouble" -> "Turns double-struck characters (like from the \"double\" style) back into normal ones."
    "smallcaps" -> "Turns lowercase letters into small capitals."
    "cc" -> "Replaces all occurences of lowercase \"c\", \"ck\" and \"k\" with \"cc\"."
    "b" -> "Replaces all occurences of Bs (lower- and uppercase) with B-button emojis (🅱)."
    "square" -> "Shows the input spaced in the first line and the tail of the input lined afterwards."
    "strike" -> "Turns the input into strikethrough using Unicode combinators (e̶x̶a̶m̶p̶l̶e̶)."
    _ -> "No documentation available."

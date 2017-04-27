{-# LANGUAGE
    OverloadedStrings
  , PatternGuards
  #-}

module Text.BraVal.Report where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import Data.Text.Template
import Data.Tuple (swap)
import Control.Monad.Trans.Writer
import Control.Arrow ((>>>))
import Text.BraVal.Types

type Text = Text.Text


report :: Writer [Symbol] [Symbol] -> String
report = runWriter >>> ( \(left, extra) ->
    Text.concat [if length left /= 0 then reportLeft left else "", if length extra /= 0 then reportExtra extra else ""] `Text.append` "\n" )
            >>> Text.unpack

        where

        reportOne :: Symbol -> Text
        reportOne symbol =
            Lazy.toStrict $ substitute "Character '${c}' at line $line, column $column.\n" (contextOne symbol)

        contextOne :: Symbol -> Text -> Text

        contextOne sym tag | tag == "c" = contextOneChar sym
                           | tag == "line" = contextOneLine sym
                           | tag == "column" = contextOneColumn sym
                           | otherwise = undefined -- Should never happen.

        contextOneChar (Symbol _ sym) | (Blank string) <- sym = Text.pack string
                                      | Just char <- lookup sym (swap <$> table) = Text.pack [char]

        contextOneChar, contextOneLine, contextOneColumn :: Symbol -> Text
        contextOneLine (Symbol cursor _) = Text.pack . show $ line cursor
        contextOneColumn (Symbol cursor _) = Text.pack . show $ column cursor


        reportLeft left = "Left open:\n" `Text.append` Text.concat (reportOne <$> left)
        reportExtra extra = "Close nothing:\n" `Text.append` Text.concat (reportOne <$> extra)

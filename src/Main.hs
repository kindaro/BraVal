module Main where

import Text.BracketsValidator
import Control.Monad.Trans.Writer

main = interact (show . runWriter . parser . (lexer :: String -> [Symbol]))

module Main where

import Text.BraVal
import Control.Monad.Trans.Writer

main = interact (show . runWriter . parser . (lexer :: String -> [Symbol]))

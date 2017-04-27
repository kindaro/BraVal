module Main where

import Text.BraVal
import Control.Monad.Trans.Writer
import Text.BraVal.Report

main = interact (report . parser . (lexer :: String -> [Symbol]))

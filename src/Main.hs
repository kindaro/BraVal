module Main where

import Text.BracketsValidator

main = interact (report . parser . (lexer :: String -> [SymbolPrimitive]))

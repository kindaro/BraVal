module Main where

import Text.BracketsValidator

main = interact (undefined . parser . (lexer :: String -> [SymbolPrimitive]))

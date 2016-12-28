module Text.BracketsValidator
    ( lexer
    , parser
    , report
    , module Text.BracketsValidator.Types
    ) where

import Text.BracketsValidator.Types

lexer :: String -> [SymbolPrimitive]
lexer [] = []
lexer (x:xs)
    | x == '(' = proceed ORound
    | x == '[' = proceed OSquare
    | x == '{' = proceed OCurled
    | x == '}' = proceed CCurled
    | x == ']' = proceed CSquare
    | x == ')' = proceed CRound
    | otherwise = case lexer xs of
        (Blank string) : _ -> Blank (x:string) : lexer (drop (length string) xs) -- Lookahead!
        _ -> proceed $ Blank (x:[])
    where proceed = (: lexer xs)

insert :: SymbolPrimitive -> [SymbolPrimitive] -> Validation [SymbolPrimitive]
insert (Blank _) a = pure a
insert s a
    | isOpen s = pure (s:a)
    | (not . null) a && (head a) `isMatching` s = pure (tail a)
    | otherwise = impure (s:a)
    where
    taint (Validation s a) = Validation (s { status = False }) a
    impure x = taint $ pure x

parser :: [SymbolPrimitive] -> Validation [SymbolPrimitive]
parser = ( foldl (>>=) (return []) ) . (fmap insert)

report :: Validation [SymbolPrimitive] -> String
report (Validation state stack)
    | status state && length stack == 0
        = "Validation succeeded."
    | status state
        = "Validation incomplete."
    | otherwise = "Validation failed."


module Text.BracketsValidator
    ( lexer
    , parser
    , report
    , module Text.BracketsValidator.Types
    ) where

import Text.BracketsValidator.Types

lexer :: String -> [Symbol]
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

insert :: Symbol -> [Symbol] -> Validation [Symbol]
insert s a = (flip add 1) $ insert' s a
    where
    add (Validation s x) n = Validation ( s { position = position s + n }) x

insert' (Blank _) a = pure a
insert' s a
    | isOpen s = pure (s:a)
    | (not . null) a && (head a) `isMatching` s = pure (tail a)
    | otherwise = impure (s:a)
    where
    taint (Validation s a) = Validation (s { status = False }) a
    impure x = taint $ pure x

parser :: [Symbol] -> Validation [Symbol]
parser = ( foldl (>>=) (return []) ) . (fmap insert)

report :: Validation [Symbol] -> String
report (Validation state stack)
    | status state && length stack == 0
        = "Validation succeeded with " ++ (show $ position state) ++ " symbols parsed."
    | status state
        = "Validation incomplete with " ++ (show $ position state) ++ " symbols parsed and "
            ++ (show $ length stack) ++ " symbols left in stack."
    | otherwise = "Validation failed after " ++ (show $ position state) ++ " symbols parsed. "
            ++ (show $ length stack) ++ " symbols left in stack."


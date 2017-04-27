module Text.BracketsValidator
    ( parser
    , module Text.BracketsValidator.Types
    ) where

import Text.BracketsValidator.Types

insert :: (Symbolic a) => a -> [a] -> Validation [a]
insert s a
    | isBlank s = pure a
    | isOpen s = pure (s:a)
    | (not . null) a && (head a) `isMatching` s = pure (tail a)
    | otherwise = impure (s:a)
    where
    taint (Validation s a) = Validation (s { status = False }) a
    impure x = taint $ pure x

parser :: (Symbolic a) => [a] -> Validation [a]
parser = ( foldl (>>=) (return []) ) . (fmap insert)


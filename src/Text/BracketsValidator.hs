module Text.BracketsValidator
    ( parser
    , module Text.BracketsValidator.Types
    ) where

import Text.BracketsValidator.Types
import Data.Monoid

insert :: (Symbolic a) => a -> [a] -> Validation [String] [a]
insert s a
    | isBlank s = pure a
    | isOpen s = pure (s:a)
    | (not . null) a && (head a) `isMatching` s = pure (tail a)
    | otherwise = (taint ["Error!"]) . pure $ (a)
    where
        taint :: Monoid state => state -> Validation state a -> Validation state a
        taint message (Validation s x) = Validation (s <> message) x

parser :: (Symbolic a) => [a] -> Validation [String] [a]
parser = ( foldl (>>=) (return []) ) . (fmap insert)


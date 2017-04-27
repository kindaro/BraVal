module Text.BracketsValidator
    ( parser
    , module Text.BracketsValidator.Types
    ) where

import Text.BracketsValidator.Types
import Data.Monoid ((<>))
import Control.Monad.Trans.Writer
import Control.Arrow ((>>>))

insert :: (Symbolic a) => a -> [a] -> Writer [a] [a]
insert s a
    | isBlank s = pure a
    | isOpen s = pure (s:a)
    | (not . null) a && (head a) `isMatching` s = pure (tail a)
    | otherwise = curry writer a [s]

parser :: (Symbolic a) => [a] -> Writer [a] [a]
parser = (fmap insert) >>> foldl (>>=) (pure mempty)


module Text.BracketsValidator.Types
    ( Symbolic (..)
    , Symbol (..)
    , SymbolPrimitive (..)
    , Cursor (..), startingCursor, advanceLine, advanceColumn
    , Validation (..)
    ) where

import Control.Arrow ((>>>))
import Data.List (groupBy)
import Data.Monoid ((<>))

data SymbolPrimitive = ORound | OSquare | OCurled | CRound | CSquare | CCurled | Blank String
    deriving (Eq, Show, Read)

data Cursor = Cursor { line :: Integer, column :: Integer } deriving Show

startingCursor = Cursor { line = 1, column = 1 }
advanceLine p = p { line = (line p + 1), column = 0 }
advanceColumn p = p { column = (column p + 1) }

data Symbol = Symbol Cursor SymbolPrimitive deriving Show

class Symbolic s where
    isOpen, isClose, isBlank :: s -> Bool
    isMatching :: s -> s -> Bool
    lexer :: String -> [s]

fromChar x
    | x == '(' = ORound
    | x == '[' = OSquare
    | x == '{' = OCurled
    | x == '}' = CCurled
    | x == ']' = CSquare
    | x == ')' = CRound
    | otherwise = Blank [x]

smap f (Symbol p s) = f s

instance Symbolic SymbolPrimitive where

    isOpen x = case x of
        ORound -> True
        OSquare -> True
        OCurled -> True
        _ -> False

    isClose x = case x of
        CRound -> True
        CSquare -> True
        CCurled -> True
        _ -> False

    isBlank x = case x of
        Blank _ -> True
        _ -> False

    o `isMatching` c = case (o,c) of
        (ORound, CRound)   -> True
        (OSquare, CSquare) -> True
        (OCurled, CCurled) -> True
        _                  -> False

    lexer [] = []
    lexer (x:xs)
        | (not . isBlank . fromChar) x = proceed $ fromChar x
        | otherwise = case lexer xs of
            (Blank string) : _ -> Blank (x:string) : lexer (drop (length string) xs) -- Lookahead!
            _                  -> proceed $ Blank (x:[])

        where proceed = (: lexer xs)

instance Symbolic Symbol where

    isOpen = smap isOpen
    isClose = smap isClose
    isBlank = smap isBlank
    (Symbol p s) `isMatching` (Symbol q t) = s `isMatching` t

    lexer = lines >>> (zipMatrix grid) >>> concat
        >>> (fmap mkSymbol) >>> (groupBy eq) >>> (fmap glue)

        where

        eq x y
            | (not . isBlank) x || (not . isBlank) y = False
            | otherwise = True

        zipMatrix = zipWith $ zipWith (,)

        grid = fmap (\x -> fmap (\y -> (x,y)) [1..] ) [1..]

        mkSymbol ((line,char),s) = Symbol (Cursor { line = line, column = char }) (fromChar s)

        glue :: [Symbol] -> Symbol
        glue [] = undefined -- Should never happen.
        glue [x] = x
        glue ((Symbol c (Blank s)) : xs)
            = Symbol c (Blank (s ++ (extract . glue $ xs)))
        glue xs = undefined -- Should never happen.

        extract (Symbol _ (Blank x)) = x

data Validation state stack = Validation state stack
    deriving (Eq, Read, Show)

instance Functor (Validation a) where
    f `fmap` Validation s x = Validation s (f x)

instance (Monoid state) => Applicative (Validation state) where

    pure = Validation mempty

    (Validation s f) <*> (Validation t x) = f <$> Validation (s <> t) x

instance (Monoid state) => Monad (Validation state) where

    a >>= f = join . (f <$>) $ a

        where
        join (Validation s (Validation t x)) = Validation (s <> t) x


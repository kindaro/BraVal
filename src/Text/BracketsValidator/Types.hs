module Text.BracketsValidator.Types
    ( Symbolic (..)
    , Symbol (..)
    , SymbolPrimitive (..)
    , State (..)
    , Cursor (..), startingCursor, advanceLine, advanceColumn
    , Validation (..)
    ) where

import Data.List (groupBy)

data SymbolPrimitive = ORound | OSquare | OCurled | CRound | CSquare | CCurled | Blank String
    deriving (Eq, Show, Read)

data Cursor = Cursor { line :: Integer, column :: Integer } deriving Show

startingCursor = Cursor { line = 1, column = 1 }
advanceLine p = p { line = (line p + 1), column = 0 }
advanceColumn p = p { column = (column p + 1) }

data Symbol = Symbol Cursor SymbolPrimitive deriving Show

smap f (Symbol p s) = f s

class Symbolic s where
    isOpen, isClose, isBlank :: s -> Bool
    isMatching :: s -> s -> Bool
    lexer :: String -> [s]

instance Symbolic SymbolPrimitive where

    -- TODO: Try applying Pattern Guards here.
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

    -- TODO: Rewrite with "case (o, c) of".
    isMatching ORound CRound = True
    isMatching OSquare CSquare = True
    isMatching OCurled CCurled = True
    isMatching _ _ = False

    lexer [] = []
    lexer (x:xs)
        | (not . isBlank . fromChar) x
            = proceed $ fromChar x
        | otherwise = case lexer xs of
            (Blank string) : _ -> Blank (x:string) : lexer (drop (length string) xs) -- Lookahead!
            _ -> proceed $ Blank (x:[])
        where
        proceed = (: lexer xs)

fromChar x
    | x == '(' = ORound
    | x == '[' = OSquare
    | x == '{' = OCurled
    | x == '}' = CCurled
    | x == ']' = CSquare
    | x == ')' = CRound
    | otherwise = Blank [x]

(>>>) = flip (.)

instance Symbolic Symbol where
    isOpen = smap isOpen
    isClose = smap isClose
    isBlank = smap isBlank
    isMatching (Symbol p s) (Symbol q t) = s `isMatching` t

    lexer = lines >>> (zipMatrix grid) >>> concat >>> (fmap mkSymbol) >>> (groupBy eq)
                                                                                >>> (fmap glue)
        where
        eq x y
            | (not . isBlank) x || (not . isBlank) y = False
            | otherwise = True
        zipMatrix = zipWith $ zipWith (,)
        grid = fmap (\x -> fmap (\y -> (x,y)) [1..] ) [1..]
        mkSymbol ((line,char),s) = Symbol (Cursor { line = line, column = char }) (fromChar s)
        glue :: [Symbol] -> Symbol
        glue [] = undefined
        glue [x] = x
        glue ((Symbol cursor (Blank s)) : xs)
            = Symbol cursor (Blank (s ++ (extract . glue $ xs)))
        glue xs = undefined
        extract (Symbol _ (Blank x)) = x

data State = State { status :: Bool }
    deriving (Eq, Read, Show)

instance Monoid State where
    mempty = State { status = True }
    x `mappend` y
        | status x && status y = State { status = True }
        | otherwise = State { status = False }

data Validation x = Validation State x
    deriving (Eq, Read, Show)

instance Functor Validation where
    fmap f (Validation s a) = Validation s (f a)

instance Applicative Validation where
    pure x = Validation mempty x
    (Validation s f) <*> (Validation t x) = Validation (s `mappend` t) (f x)

instance Monad Validation where
    a@(Validation s x) >>= f
        | status s == False = Validation s (unf x) -- Do nothing.
        | otherwise = s `into` f x
        -- Maybe I don't need this part.
        where unpack (Validation s a) = a
              unf = unpack . f
              into s (Validation t y) = Validation (s `mappend` t) y

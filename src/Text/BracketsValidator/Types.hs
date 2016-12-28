module Text.BracketsValidator.Types
    ( Symbol (..)
    , State (..)
    , Validation (..)
    , isOpen
    , isClose
    , isBlank
    , isMatching
    ) where

data Symbol = ORound | OSquare | OCurled | CRound | CSquare | CCurled | Blank String
    deriving (Eq, Show, Read)

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

isMatching ORound CRound = True
isMatching OSquare CSquare = True
isMatching OCurled CCurled = True
isMatching _ _ = False

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

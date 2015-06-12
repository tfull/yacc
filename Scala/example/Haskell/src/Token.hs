module Token where

import Base

data Token = Token {
    tokenV :: TokenV,
    lineS :: Int,
    charS :: Int,
    lineG :: Int,
    charG :: Int
} deriving Eq

data TokenV = Int Int
    | Plus
    | Star
    | Minus
    | Percent
    | Slash
    | LPar
    | RPar
    | End
    deriving (Show, Eq)

make :: TokenV -> (Int, Int) -> (Int, Int) -> Token
make tv (ls, cs) (lg, cg) = Token { tokenV = tv, lineS = ls, charS = cs, lineG = lg, charG = cg }

makeEnd :: (Int, Int) -> Token
makeEnd (l, c) = Token { tokenV = End, lineS = l, charS = c, lineG = l, charG = c }

instance Show Token where
    show t = (show . tokenV) t ++ " [" ++ (show . lineS) t ++ ":" ++ (show . charS) t ++ "-" ++ (show . lineG) t ++ ":" ++ (show . charG) t ++ "]"

instance Debug Token where
    debug (Token { tokenV = x }) = show x

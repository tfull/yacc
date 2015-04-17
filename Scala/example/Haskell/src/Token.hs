module Token where

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
    deriving (Show, Eq)

make :: TokenV -> (Int, Int) -> (Int, Int) -> Token
make tv (ls, cs) (lg, cg) = Token { tokenV = tv, lineS = ls, charS = cs, lineG = lg, charG = cg }

instance Show Token where
    show t = (show . tokenV) t ++ " [" ++ (show . lineS) t ++ ":" ++ (show . charS) t ++ "-" ++ (show . lineG) t ++ ":" ++ (show . charG) t ++ "]"

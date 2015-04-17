module Type where

data Tree = Tree {
    treeV :: TreeV,
    lineS :: Int,
    charS :: Int,
    lineG :: Int,
    charG :: Int
} deriving (Show, Eq)

data TreeV = Int Int
    | Add Tree Tree
    | Sub Tree Tree
    | Mul Tree Tree
    | Div Tree Tree
    | Mod Tree Tree
    | Minus Tree
    deriving (Show, Eq)


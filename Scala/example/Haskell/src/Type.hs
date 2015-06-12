module Type where

import Base
import qualified Data.List as L

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

makeIndent :: String -> Int -> String
makeIndent s i = L.concat $ L.replicate i s

debugF :: String -> Int -> Tree -> String
debugF s i (Tree { treeV = Int x }) =  makeIndent s i ++ "Int " ++ show x
debugF s i (Tree { treeV = Add t1 t2 }) = makeIndent s i ++ L.intercalate "\n" ["Add", debugF s (i + 1) t1, debugF s (i + 1) t2]
debugF s i (Tree { treeV = Sub t1 t2 }) = makeIndent s i ++ L.intercalate "\n" ["Sub", debugF s (i + 1) t1, debugF s (i + 1) t2]
debugF s i (Tree { treeV = Mul t1 t2 }) = makeIndent s i ++ L.intercalate "\n" ["Mul", debugF s (i + 1) t1, debugF s (i + 1) t2]
debugF s i (Tree { treeV = Div t1 t2 }) = makeIndent s i ++ L.intercalate "\n" ["Div", debugF s (i + 1) t1, debugF s (i + 1) t2]
debugF s i (Tree { treeV = Mod t1 t2 }) = makeIndent s i ++ L.intercalate "\n" ["Mod", debugF s (i + 1) t1, debugF s (i + 1) t2]
debugF s i (Tree { treeV = Minus t }) = makeIndent s i ++ "Minus\n" ++ debugF s (i + 1) t

instance Debug Tree where
    debug a = debugF "  " 0 a

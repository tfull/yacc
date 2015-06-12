module Tokenizer(tokenize) where

import Base
import qualified Token as T
import qualified Data.Char as C
import qualified Data.List as L

data Automaton = Plain | Int

type Position = (Int, Int)

type Store = (String, Position)

tokenize :: Monad m => String -> m [T.Token]
tokenize s = scan Plain ([], (0, 0)) (1, 0) (scanC (1, 0) s) s

scanC :: Position -> String -> Position
scanC _ [] = (-1, -1)
scanC (l, c) ('\n' : _) = (l + 1, 0)
scanC (l, c) _ = (l, c + 1)

noStore :: (String, Position)
noStore = ([], (-1, -1))

showPosition :: Position -> String
showPosition (l, c) = "(" ++ show l ++ " line, " ++ show c ++ " character)"

scan :: Monad m => Automaton -> Store -> Position -> Position -> String -> m [T.Token]
scan am st prev _ [] = scanBottom am st prev
scan Plain _ prev now (c : cs)
    | c >= '0' && c <= '9' = scan Int ([c], now) now (scanC now cs) cs
    | C.isSpace c = scan Plain noStore now (scanC now cs) cs
    | otherwise =
        let mts = scan Plain noStore now (scanC now cs) cs in
        let f = \x -> T.make x now now in
        case c of
            '+' -> mts >>= (\x -> return $ f T.Plus : x)
            '*' -> mts >>= (\x -> return $ f T.Star : x)
            '-' -> mts >>= (\x -> return $ f T.Minus : x)
            '/' -> mts >>= (\x -> return $ f T.Slash : x)
            '%' -> mts >>= (\x -> return $ f T.Percent : x)
            '(' -> mts >>= (\x -> return $ f T.LPar : x)
            ')' -> mts >>= (\x -> return $ f T.RPar : x)
            _ -> fail $ "scan[Plain]: " ++ showPosition now
scan Int (store, sp) prev now (c : cs)
    | c >= '0' && c <= '9' = scan Int (c : store, sp) now (scanC now cs) cs
    | C.isSpace c = do
        let int = T.make (T.Int . read . L.reverse $ store) sp prev
        ts <- scan Plain noStore now (scanC now cs) cs
        return $ int : ts
    | otherwise =
        let int = T.make (T.Int . read . L.reverse $ store) sp prev in
        let mts = scan Plain noStore now (scanC now cs) cs in
        let f = \x -> T.make x sp prev in
        case c of
            '+' -> mts >>= (\x -> return $ int : f T.Plus : x)
            '*' -> mts >>= (\x -> return $ int : f T.Star : x)
            '-' -> mts >>= (\x -> return $ int : f T.Minus : x)
            '/' -> mts >>= (\x -> return $ int : f T.Slash : x)
            '%' -> mts >>= (\x -> return $ int : f T.Percent : x)
            '(' -> mts >>= (\x -> return $ int : f T.LPar : x)
            ')' -> mts >>= (\x -> return $ int : f T.RPar : x)
            _ -> fail $ "scan(Int): " ++ showPosition now

scanBottom :: Monad m => Automaton -> Store -> Position -> m [T.Token]
scanBottom Plain (store, _) pos
    | store == [] = return [T.makeEnd pos]
    | otherwise = fail "failed in scanBottom(Plain)"
scanBottom Int (store, sp) p
    | store == [] = fail "failed in scanBottom(Int)"
    | otherwise = return [T.make (T.Int . read . L.reverse $ store) sp p, T.makeEnd p]

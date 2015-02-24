module Regular where

import qualified Data.List as L
import qualified Data.Tuple as T

import qualified Parser as P

data Regular = REmpty
    | RChar Char
    | RSet String
    | RPlus Regular
    | RStar Regular
    | RQuestion Regular
    | ROr Regular Regular
    | RAnd Regular Regular
    deriving Show

data Symbol = SEmpty
    | SChar Char
    | SSet String
    | SVBar
    | SLPar | SRPar
    | SLSq | SRSq
    | SQues
    | SHyphen
    | SDot
    | SStar
    | SPlus
    deriving Show

data Token = Empty
    | Char Char
    | Set String
    | VBar
    | LPar | RPar
    | Ques
    | Star
    | Plus
    deriving Show

data N = SD | S | A | B | C deriving (Show, Eq, Ord)
data T = Emp | At | As | LP | RP | Vb | Qu | St | Pl | EOT deriving (Show, Eq, Ord)

normalChar :: String
normalChar = "\t\n !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"

symbolHash :: [(Char, Symbol)]
symbolHash = [('(',SLPar),(')',SRPar),('|',SVBar),('.',SDot),('[',SLSq),(']',SRSq),('?',SQues),('*',SStar),('+',SPlus),('-',SHyphen)]

stringToSymbols :: (Monad m) => Bool -> String -> m [Symbol]
stringToSymbols False [] = return []
stringToSymbols False ('\\' : cs) = stringToSymbols True cs
stringToSymbols False (c : cs) = stringToSymbols False cs >>= return . (look c symbolHash :)
    where
        look :: Char -> [(Char, Symbol)] -> Symbol
        look c h = case L.lookup c h of
            Just sym -> sym
            Nothing -> SChar c
stringToSymbols True [] = fail "empty after '\\'"
stringToSymbols True (c : cs) = do
    ch <- get c symbolHash
    syms <- stringToSymbols False cs
    return $ ch : syms
    where
        get :: (Monad m) => Char -> [(Char, a)] -> m Symbol
        get 'd' _ = return $ SSet "0123456789"
        get 'e' _ = return SEmpty
        get c h
            | L.elem c $ '\\' : T.fst (L.unzip h) = return $ SChar c
            | otherwise = fail "invalid after '\\'"

tokenize :: (Monad m) => [Symbol] -> m [Token]
tokenize [] = return []
tokenize (SLSq : xs) = do
    (y, ys) <- tokenizeS [] xs
    ms <- tokenize ys
    return $ y : ms
tokenize (SRSq : _) = fail "']' appears before '['"
tokenize (SEmpty : xs) = tokenize xs >>= return . (Empty :)
tokenize (SChar c : xs) = tokenize xs >>= return . (Char c :)
tokenize (SSet s : xs) = tokenize xs >>= return . (Set s :)
tokenize (SVBar : xs) = tokenize xs >>= return . (VBar :)
tokenize (SLPar : xs) = tokenize xs >>= return . (LPar :)
tokenize (SRPar : xs) = tokenize xs >>= return . (RPar :)
tokenize (SQues : xs) = tokenize xs >>= return . (Ques :)
tokenize (SDot : xs) = tokenize xs >>= return . (Set (L.delete '\n' normalChar) :)
tokenize (SStar : xs) = tokenize xs >>= return . (Star :)
tokenize (SPlus : xs) = tokenize xs >>= return . (Plus :)

tokenizeS :: (Monad m) => [Either Char String] -> [Symbol] -> m (Token, [Symbol])
tokenizeS _ [] = fail "terminal ']' do not appear"
tokenizeS xs (SRSq : ys) = return (Set . L.nub $ f xs, ys)
    where
        f :: [Either Char String] -> String
        f [] = []
        f (Left c : eccs) = c : f eccs
        f (Right cs : eccs) = cs ++ f eccs
tokenizeS xs (SChar c : ys) = tokenizeS (Left c : xs) ys
tokenizeS (Left c : xs) (SHyphen : SChar a : ys)
    | c < a = tokenizeS (Right [c..a] : xs) ys
    | otherwise = fail $ "invalid order (" ++ show c ++ " to " ++ show a ++ ")"
tokenizeS _ _ = fail "invalid in set"

tokens :: [Token] -> [(T, Token)]
tokens [] = [(EOT, Empty)]
tokens (Empty : ts) = (Emp, Empty) : tokens ts
tokens (Char c : ts) = (At, Char c) : tokens ts
tokens (Set s : ts) = (As, Set s) : tokens ts
tokens (VBar : ts) = (Vb, VBar) : tokens ts
tokens (LPar : ts) = (LP, LPar) : tokens ts
tokens (RPar : ts) = (RP, RPar) : tokens ts
tokens (Ques : ts) = (Qu, Ques) : tokens ts
tokens (Star : ts) = (St, Star) : tokens ts
tokens (Plus : ts) = (Pl, Plus) : tokens ts

rules :: [(N, [P.Symbol N T], [P.ParseItem Token Regular] -> Regular)]
rules = [(S, [P.NT S, P.TS Vb, P.NT A], f1), (S, [P.NT A], f2), (A, [P.NT A, P.NT B], f3), (A, [P.NT B], f2), (B, [P.TS LP, P.NT S, P.TS RP], f4), (B, [P.NT B, P.TS Qu], f5 RQuestion), (B, [P.NT B, P.TS St], f5 RStar), (B, [P.NT B, P.TS Pl], f5 RPlus), (B, [P.NT C], f2), (C, [P.TS At], f6), (C, [P.TS As], f7), (C, [P.TS Emp], f8)]
    where
        f1 [P.PSt a, _, P.PSt c] = ROr a c
        f2 [P.PSt a] = a
        f3 [P.PSt a, P.PSt b] = RAnd a b
        f4 [_, P.PSt b, _] = b
        f5 f [P.PSt a, _] = f a
        f6 [P.PSym (Char c)] = RChar c
        f7 [P.PSym (Set s)] = RSet s
        f8 [P.PSym Empty] = REmpty

graph = P.prepare rules

compile :: (Monad m) => String -> m Regular
compile s = do
    syms <- stringToSymbols False s
    tkns <- tokenize syms
    P.useParser graph $ tokens tkns

instance P.NonTerminal N where
    sop = SD
    nop = S

instance P.Terminal T where
    eot = EOT
    oot = EOT

instance P.Token Token where

instance P.Statement Regular where



module Parser (useParser, prepare, parse, Statement, Terminal, NonTerminal, Token, Symbol(..), ParseItem(..), sop, nop, eot, oot) where

import qualified Data.Tuple as T
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Char as C

import Control.Applicative

data Symbol a b = NT a | TS b deriving (Show, Eq, Ord)
data ParseItem a b = PSym a | PSt b deriving (Show, Eq, Ord)

type Production a b = (a, [Symbol a b])
type Location a b = ([Symbol a b], [Symbol a b])
type Term a b = ((a, Location a b), b)
type Vertex a b = S.Set (Term a b)
type Edge a b = (Vertex a b, Symbol a b, Vertex a b)

-- basic

addUp :: Eq a => (b -> b -> b) -> (a, b) -> [(a, b)] -> [(a, b)]
addUp _ (k, v) [] = [(k, v)]
addUp f (k, v) ((kk, vv) : xs)
    | k == kk = (k, v `f` vv) : xs
    | otherwise = (kk, vv) : addUp f (k, v) xs

splits :: [a] -> [([a], [a])]
splits xs = f [] xs
    where
        f xs [] = [(xs, [])]
        f xs (y : ys) = (xs, y : ys) : f (xs ++ [y]) ys

xsInXSet :: Ord a => S.Set a -> [a] -> Bool
xsInXSet set = L.all (\x -> S.member x set)

lookUpWithDefault :: Eq a => b -> a -> [(a, b)] -> b
lookUpWithDefault d _ [] = d
lookUpWithDefault d k ((kk, vv) : xs)
    | k == kk = vv
    | otherwise = lookUpWithDefault d k xs

-- debug

showVertex :: (NonTerminal a, Terminal b, ShowK a, ShowK b) => S.Set (Vertex a b) -> String
showVertex setT = sv 1 $ S.toList setT
    where
        sv :: (NonTerminal a, Terminal b, ShowK a, ShowK b) => Integer -> [Vertex a b] -> String
        sv i [] = []
        sv i [v] = show i ++ "\n" ++ (showTerm $ S.toList v)
        sv i (v : vs) = show i ++ "\n" ++ (showTerm $ S.toList v) ++ "\n" ++ sv (i + 1) vs
        showTerm :: (NonTerminal a, Terminal b, ShowK a, ShowK b) => [Term a b] -> String
        showTerm [] = []
        showTerm [((a, (x1, x2)), b)] = showK a ++ " -> " ++ st x1 ++ " . " ++ st x2 ++ "   [" ++ showK b ++ "]"
        showTerm (((a, (x1, x2)), b) : ts) = showK a ++ " -> " ++ st x1 ++ " . " ++ st x2 ++ "   [" ++ showK b ++ "]" ++ "\n" ++ showTerm ts
        st :: (NonTerminal a, Terminal b, ShowK a, ShowK b) => [Symbol a b] -> String
        st [] = []
        st [TS s] = L.map C.toLower $ showK s
        st [NT s] = showK s
        st (TS s : ss) = showK s ++ " " ++ st ss
        st (NT s : ss) = showK s ++ " " ++ st ss

showProduction :: (NonTerminal a, Terminal b, ShowK a, ShowK b) => Production a b -> String
showProduction (x, gamma) = showK x ++ " -> " ++ sp gamma
    where
        sp :: (NonTerminal a, Terminal b, ShowK a, ShowK b) => [Symbol a b] -> String
        sp [] = []
        sp [NT a] = showK a
        sp [TS b] = showK b
        sp (NT a : ss) = showK a ++ " " ++ sp ss
        sp (TS b : ss) = showK b ++ " " ++ sp ss

showEdgeReduce :: (NonTerminal a, Terminal b) => (S.Set (Vertex a b), S.Set (Edge a b)) -> String
showEdgeReduce (setT, setE) =
    let hash = indexHash $ S.toList setT
        se = L.map (\(x, y, z) -> (lookUpWithError "allocateE" x hash, y, lookUpWithError "allocateE" z hash)) $ S.toList setE
        r = L.map (\((x, y), z) -> ((lookUpWithError "allocateR" x hash, y), z)) $ S.toList $ reduces setT
        (s, g) = edgeToSG se
    in
    "shift\n" ++ show s ++ "\nreduce\n" ++ show r ++ "\ngoto\n" ++ show g

-- special

makeNullableSet :: (NonTerminal a, Terminal b) => [Production a b] -> S.Set (Symbol a b)
makeNullableSet prods = loop0 S.empty prods
    where
        loop0 :: (NonTerminal a, Terminal b) => S.Set (Symbol a b) -> [Production a b] -> S.Set (Symbol a b)
        loop0 set prods = let nset = loop1 set prods in if set == nset then set else loop0 nset prods
            where
                loop1 :: (NonTerminal a, Terminal b) => S.Set (Symbol a b) -> [Production a b] -> S.Set (Symbol a b)
                loop1 set [] = set
                loop1 set ((kX, ks) : prods)
                    | xsInXSet set ks = loop1 (S.insert (NT kX) set) prods
                    | otherwise = loop1 set prods

gatherTerminals :: (NonTerminal a, Terminal b) => [Production a b] -> S.Set (Symbol a b)
gatherTerminals = S.fromList . L.filter f . L.foldl (++) [] . L.map (\(a, b) -> b)
    where
        f :: (NonTerminal a, Terminal b) => Symbol a b -> Bool
        f (NT _) = False
        f (TS _) = True

makeFirstSets :: (NonTerminal a, Terminal b) => [Production a b] -> [(Symbol a b, S.Set b)]
makeFirstSets prods = (loop0 (makeNullableSet prods) (L.map (\x -> (TS x, S.singleton x)) $ sToB $ S.toList $ gatherTerminals prods) prods) ++ [(TS eot, S.singleton eot)]
    where
        sToB :: [Symbol a b] -> [b]
        sToB [] = []
        sToB ((TS b) : xs) = b : sToB xs
        sToB ((NT _) : _) = error "sToB in makeFirstSets" -- not occur
        loop0 :: (NonTerminal a, Terminal b) => S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> [Production a b] -> [(Symbol a b, S.Set b)]
        loop0 setN setF prods = let nsetF = loop1 setN setF prods in if nsetF == setF then setF else loop0 setN nsetF prods
            where
                loop1 :: (NonTerminal a, Terminal b) => S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> [Production a b] -> [(Symbol a b, S.Set b)]
                loop1 setN setF [] = setF
                loop1 setN setF (prod : prods) = loop1 setN (loop2 setN setF prod) prods
                    where
                        loop2 :: (NonTerminal a, Terminal b) => S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> Production a b -> [(Symbol a b, S.Set b)]
                        loop2 setN setF (kX, ks) = loop3 setN setF (NT kX) $ splits ks
                            where
                                loop3 :: (NonTerminal a, Terminal b) => S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> Symbol a b -> [([Symbol a b], [Symbol a b])] -> [(Symbol a b, S.Set b)]
                                loop3 setN setF s [] = setF
                                loop3 setN setF s ((_, []) : zs) = loop3 setN setF s zs
                                loop3 setN setF s ((xs, y : ys) : zs)
                                    | xsInXSet setN xs = loop3 setN (addUp S.union (s, (lookUpWithDefault S.empty y setF)) setF) s zs
                                    | otherwise = loop3 setN setF s zs

closure :: (NonTerminal a, Terminal b) => [Production a b] -> S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> Vertex a b -> Vertex a b
closure prods setN setF setI = let nI = loop0 prods setN setF setI $ S.toList setI in if nI == setI then setI else closure prods setN setF nI
    where
        getF :: (NonTerminal a, Terminal b) => S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> [Symbol a b] -> S.Set b
        getF setN setF [] = S.empty
        getF setN setF (x : xs)
            | x `S.member` setN = lookUpWithDefault S.empty x setF `S.union` getF setN setF xs
            | otherwise = lookUpWithDefault S.empty x setF
        loop0 :: (NonTerminal a, Terminal b) => [Production a b] -> S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> Vertex a b -> [Term a b] -> Vertex a b
        loop0 prods setN setF setI [] = setI
        loop0 prods setN setF setI (((_, (_, NT b : bt)), z) : ts) = 
            let ps = L.filter (\(x, _) -> x == b) prods
                fs = S.toList $ getF setN setF $ bt ++ [TS z]
            in
            loop0 prods setN setF (loop1 setI $ (,) <$> ps <*> fs) ts
            where
                loop1 :: (NonTerminal a, Terminal b) => Vertex a b -> [(Production a b, b)] -> Vertex a b
                loop1 setI [] = setI
                loop1 setI (((x, y), z) : ws) = loop1 (S.insert ((x, ([], y)), z) setI) ws
        loop0 prods setN setF setI (_ : ts) = loop0 prods setN setF setI ts

goTo :: (NonTerminal a, Terminal b) => [Production a b] -> S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> Vertex a b -> Symbol a b -> Vertex a b
goTo prods setN setF setI kX = closure prods setN setF $ S.fromList $ f kX $ S.toList setI
    where
        f :: (NonTerminal a, Terminal b) => Symbol a b -> [Term a b] -> [Term a b]
        f _ [] = []
        f kX (((a, (b, c : cs)), d) : ts)
            | kX == c = ((a, (b ++ [c], cs)), d) : f kX ts
            | otherwise = f kX ts
        f kX (_ : ts) = f kX ts

reduces :: (NonTerminal a, Terminal b) => S.Set (Vertex a b) -> S.Set ((Vertex a b, b), Production a b)
reduces setT = S.fromList $ L.concat $ L.map forI $ S.toList setT
    where
        forI :: (NonTerminal a, Terminal b) => Vertex a b -> [((Vertex a b, b), Production a b)]
        forI setI = f setI $ S.toList setI
        f :: (NonTerminal a, Terminal b) => Vertex a b -> [Term a b] -> [((Vertex a b, b), Production a b)]
        f _ [] = []
        f setI (((a, (b, [])), d) : xs) = ((setI, d), (a, b)) : f setI xs
        f setI (_ : xs) = f setI xs

makeParserGraph :: (NonTerminal a, Terminal b) => [Production a b] -> (S.Set (Vertex a b), S.Set (Edge a b))
makeParserGraph prods =
    let setN = makeNullableSet prods
        setF = makeFirstSets prods
        clos = closure prods setN setF $ S.singleton ((sop, ([], [NT nop, TS eot])), oot)
    in
    loop0 prods setN setF (S.singleton clos, S.empty)
        where
            loop0 :: (NonTerminal a, Terminal b) => [Production a b] -> S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> (S.Set (Vertex a b), S.Set (Edge a b)) -> (S.Set (Vertex a b), S.Set (Edge a b))
            loop0 prods setN setF (setT, setE) =
                let (nT, nE) = loop1 prods setN setF (setT, setE) $ S.toList setT in
                if (nT, nE) == (setT, setE) then (setT, setE) else loop0 prods setN setF (nT, nE)
                where
                    loop1 :: (NonTerminal a, Terminal b) => [Production a b] -> S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> (S.Set (Vertex a b), S.Set (Edge a b)) -> [Vertex a b] -> (S.Set (Vertex a b), S.Set (Edge a b))
                    loop1 prods setN setF (setT, setE) [] = (setT, setE)
                    loop1 prods setN setF (setT, setE) (setI : setIs) = loop1 prods setN setF (loop2 prods setN setF (setT, setE) setI $ S.toList setI) setIs
                        where
                            loop2 :: (NonTerminal a, Terminal b) => [Production a b] -> S.Set (Symbol a b) -> [(Symbol a b, S.Set b)] -> (S.Set (Vertex a b), S.Set (Edge a b)) -> Vertex a b -> [Term a b] -> (S.Set (Vertex a b), S.Set (Edge a b))
                            loop2 prods setN setF (setT, setE) setI [] = (setT, setE)
                            loop2 prods setN setF (setT, setE) setI (((_, (_, [])), _) : ts) = loop2 prods setN setF (setT, setE) setI ts
                            loop2 prods setN setF (setT, setE) setI (((_, (_, kX : _)), _) : ts)
                                | kX == TS eot = loop2 prods setN setF (setT, setE) setI ts
                                | otherwise =
                                    let setJ = goTo prods setN setF setI kX in
                                    loop2 prods setN setF (S.insert setJ setT, S.insert (setI, kX, setJ) setE) setI ts

indexHash :: (NonTerminal a, Terminal b) => [Vertex a b] -> [(Vertex a b, Integer)]
indexHash xs = index 3 xs
    where
        index :: (NonTerminal a, Terminal b) => Integer -> [Vertex a b] -> [(Vertex a b, Integer)]
        index i [] = []
        index i (v : vs)
            | S.member ((sop, ([], [NT nop, TS eot])), oot) v = (v, 1) : index i vs
            | S.member ((sop, ([NT nop], [TS eot])), oot) v = (v, 2) : index i vs
            | otherwise = (v, i) : index (i + 1) vs

lookUpWithError :: Eq a => String -> a -> [(a, b)] -> b
lookUpWithError s a [] = error s
lookUpWithError s a ((k, v) : xs)
    | a == k = v
    | otherwise = lookUpWithError s a xs

edgeToSG :: (NonTerminal a, Terminal b) => [(Integer, Symbol a b, Integer)] -> ([((Integer, b), Integer)], [((Integer, a), Integer)])
edgeToSG edges = etsg ([], []) edges
    where
        etsg :: (NonTerminal a, Terminal b) => ([((Integer, b), Integer)], [((Integer, a), Integer)]) -> [(Integer, Symbol a b, Integer)] -> ([((Integer, b), Integer)], [((Integer, a), Integer)])
        etsg (s, g) [] = (s, g)
        etsg (s, g) ((i, NT a, j) : es) = etsg (s, g ++ [((i, a), j)]) es
        etsg (s, g) ((i, TS b, j) : es) = etsg (s ++ [((i, b), j)], g) es

prepare :: (NonTerminal a, Terminal b, Token c, Statement s) => [(a, [Symbol a b], [ParseItem c s] -> s)] -> ([((Integer, b), Integer)], [((Integer, b), Production a b)], [((Integer, a), Integer)], [(Production a b, [ParseItem c s] -> s)])
prepare rules =
    let prods = L.map (\(x, y, _) -> (x, y)) rules
        funs = L.map (\(x, y, z) -> ((x, y), z)) rules
        (setT, setE) = makeParserGraph prods
        setR = reduces setT
        hash = indexHash $ S.toList setT
        es = L.map (\(x, y, z) -> (lookUpWithError "allocateE" x hash, y, lookUpWithError "allocateE" z hash)) $ S.toList setE
        rs = L.map (\((x, y), z) -> ((lookUpWithError "allocateR" x hash, y), z)) $ S.toList setR
        (s, g) = edgeToSG es
    in
    (s, rs, g, funs)

useParser :: (NonTerminal a, Terminal b, Token c, Monad m, Statement s) => ([((Integer, b), Integer)], [((Integer, b), Production a b)], [((Integer, a), Integer)], [(Production a b, [ParseItem c s] -> s)]) -> [(b, c)] -> m s
useParser (s, r, g, f) tokens = parseLoop (s, r, g) f [(1, error "fatal error: empty stack")] tokens
    where
        parseLoop :: (NonTerminal a, Terminal b, Token c, Monad m, Statement s) => ([((Integer, b), Integer)], [((Integer, b), Production a b)], [((Integer, a), Integer)]) -> [(Production a b, [ParseItem c s] -> s)] -> [(Integer, ParseItem c s)] -> [(b, c)] -> m s
        parseLoop (_, _, _) _ [] _ = error "fatal error: empty stack (2)"
        parseLoop (_, _, _) _ _ [] = fail "parse error: end of tokens reached"
        parseLoop (sS, sR, sG) fs ((cond, st) : stack) ((b, c) : ts) =
            case L.lookup (cond, b) sS of
                Just j -> parseLoop (sS, sR, sG) fs ((j, PSym c) : (cond, st) : stack) ts
                Nothing -> case L.lookup (cond, b) sR of
                    Just (x, gamma) ->
                        let fun = lookUpWithError "fatal error: no function" (x, gamma) fs
                            (tokens, ((ni, nt) : newstack)) = takeF (L.length gamma) ([], (cond, st) : stack)
                        in
                        parseLoop (sS, sR, sG) fs ((lookUpWithError "fatal error: no goto arrow" (ni, x) sG, PSt $ fun tokens) : (ni, nt) : newstack) ((b, c) : ts)
                    Nothing -> if cond == 2 && b == eot
                        then case st of
                            PSt ans -> return ans
                            _ -> error "fatal error: cannot reduce to statement"
                        else fail $ "unexpected token: " ++ show c
        takeF :: Int -> ([b], [(a, b)]) -> ([b], [(a, b)])
        takeF i (xs, (z, y) : ys)
            | i > 0 = takeF (i - 1) (y : xs, ys)
            | otherwise = (xs, (z, y) : ys)
        takeF _ (_, []) = error "fatal error: empty stack (3) "


parse :: (NonTerminal a, Terminal b, Token c, Monad m, Statement s) => [(a, [Symbol a b], [ParseItem c s] -> s)] -> [(b, c)] -> m s
parse rules tokens =
    let prods = L.map (\(x, y, _) -> (x, y)) rules
        funs = L.map (\(x, y, z) -> ((x, y), z)) rules
        (setT, setE) = makeParserGraph prods
        setR = reduces setT
        hash = indexHash $ S.toList setT
        es = L.map (\(x, y, z) -> (lookUpWithError "allocateE" x hash, y, lookUpWithError "allocateE" z hash)) $ S.toList setE
        rs = L.map (\((x, y), z) -> ((lookUpWithError "allocateR" x hash, y), z)) $ S.toList setR
        (s, g) = edgeToSG es
    in
    parseLoop (s, rs, g) funs [(1, error "fatal error: empty stack (1)")] tokens
        where
            parseLoop :: (NonTerminal a, Terminal b, Token c, Monad m, Statement s) => ([((Integer, b), Integer)], [((Integer, b), Production a b)], [((Integer, a), Integer)]) -> [(Production a b, [ParseItem c s] -> s)] -> [(Integer, ParseItem c s)] -> [(b, c)] -> m s
            parseLoop (_, _, _) _ [] _ = error "fatal error: empty stack (2)"
            parseLoop (_, _, _) _ _ [] = fail "parse error: end of tokens reached"
            parseLoop (sS, sR, sG) fs ((cond, st) : stack) ((b, c) : ts) =
                case L.lookup (cond, b) sS of
                    Just j -> parseLoop (sS, sR, sG) fs ((j, PSym c) : (cond, st) : stack) ts
                    Nothing -> case L.lookup (cond, b) sR of
                        Just (x, gamma) ->
                            let fun = lookUpWithError "fatal error: no function" (x, gamma) fs
                                (tokens, ((ni, nt) : newstack)) = takeF (L.length gamma) ([], (cond, st) : stack)
                            in
                            parseLoop (sS, sR, sG) fs ((lookUpWithError "fatal error: no goto arrow" (ni, x) sG, PSt $ fun tokens) : (ni, nt) : newstack) ((b, c) : ts)
                        Nothing -> if cond == 2 && b == eot
                            then case st of
                                PSt ans -> return ans
                                _ -> error "fatal error: cannot reduce to statement"
                            else fail $ "unexpected token: " ++ show c
            takeF :: Show b => Int -> ([b], [(a, b)]) -> ([b], [(a, b)])
            takeF i (xs, (z, y) : ys)
                | i > 0 = takeF (i - 1) (y : xs, ys)
                | otherwise = (xs, (z, y) : ys)
            takeF i (xs, []) = error $ "fatal error: empty stack (3) " ++ show (i, xs)



-- definition

class (Show a, Ord a) => NonTerminal a where
    sop :: a
    nop :: a

class (Show a, Ord a) => Terminal a where
    eot :: a
    oot :: a

class (Show a) => Token a where

class (Show a) => Statement a where

class ShowK a where
    showK :: a -> String

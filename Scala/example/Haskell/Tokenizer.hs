module Tokenizer(tokenize) where

import Base
import qualified Type
import qualified Data.Char as C
import qualified Data.List as L

data AutomatonMode = Plain | Int

type Store = (String, Int, Int)

tokenize :: Monad m => String -> m [Type.Token]
tokenize s = scan Plain ([], 0, 0) (scanCharacter (1, 0) s) s

scanCharacter :: (Int, Int) -> String -> (Int, Int)
scanCharacter _ [] = (-1, -1)
scanCharacter (l, c) ('\n' : _) = (l + 1, 0)
scanCharacter (l, c) _ = (l, c + 1)

noStore :: (String, Int, Int)
noStore = ([], -1, -1)

scan :: Monad m => AutomatonMode -> Store -> (Int, Int) -> String -> m [Type.Token]
scan am st lc [] = scanBottom am st lc
scan Plain (store, _, _) n@(line, character) (c : cs)
    | c >= '0' && c <= '9' = scan Int ([c], line, character) (scanCharacter n cs) cs
    | C.isSpace c = scan Plain noStore (scanCharacter n cs) cs
    | otherwise = case c of
        '+' -> do
            ts <- scan Plain noStore (scanCharacter n cs) cs
            return $ Type.Token { Type.tokenT = Type.TokenPlus, Type.line = line, Type.character = character } : ts
        '*' -> do
            ts <- scan Plain noStore (scanCharacter n cs) cs
            return $ Type.Token { Type.tokenT = Type.TokenStar, Type.line = line, Type.character = character } : ts
        '(' -> do
            ts <- scan Plain noStore (scanCharacter n cs) cs
            return $ Type.Token { Type.tokenT = Type.TokenLPar, Type.line = line, Type.character = character } : ts
        ')' -> do
            ts <- scan Plain noStore (scanCharacter n cs) cs
            return $ Type.Token { Type.tokenT = Type.TokenLPar, Type.line = line, Type.character = character } : ts
        _ -> fail $ "scan(Plain): " ++ show line ++ " line, " ++ show character ++ " character"
scan Int (store, s_l, s_c) n@(line, character) (c : cs)
    | c >= '0' && c <= '9' = scan Int (c : store, s_l, s_c) (scanCharacter n cs) cs
    | C.isSpace c = do
        let int = Type.Token { Type.tokenT = Type.TokenInt . read . L.reverse $ store, Type.line = s_l, Type.character = s_c }
        ts <- scan Plain noStore (scanCharacter n cs) cs
        return $ int : ts
    | otherwise =
        let int = Type.Token { Type.tokenT = Type.TokenInt . read . L.reverse $ store, Type.line = s_l, Type.character = s_c } in
        let mts = scan Plain noStore (scanCharacter n cs) cs in
        case c of
            '+' -> do
                ts <- mts
                let t = Type.Token { Type.tokenT = Type.TokenPlus, Type.line = line, Type.character = character }
                return $ int : t : ts
            '*' -> do
                ts <- mts
                let t = Type.Token { Type.tokenT = Type.TokenStar, Type.line = line, Type.character = character }
                return $ int : t : ts
            '(' -> do
                ts <- mts
                let t = Type.Token { Type.tokenT = Type.TokenLPar, Type.line = line, Type.character = character }
                return $ int : t : ts
            ')' -> do
                ts <- mts
                let t = Type.Token { Type.tokenT = Type.TokenRPar, Type.line = line, Type.character = character }
                return $ int : t : ts
            _ -> fail $ "scan(Int): " ++ show line ++ " line, " ++ show character ++ " character"

scanBottom :: Monad m => AutomatonMode -> Store -> (Int, Int) -> m [Type.Token]
scanBottom Plain (store, _, _) _
    | store == [] = return []
    | otherwise = fail "failed in scanBottom(Plain)"
scanBottom Int (store, sl, sc) _
    | store == [] = fail "failed in scanBottom(Int)"
    | otherwise = return [Type.Token { Type.tokenT = Type.TokenInt . read . L.reverse $ store, Type.line = sl, Type.character = sc }]

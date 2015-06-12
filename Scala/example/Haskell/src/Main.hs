module Main where

import Base
import qualified Type
import qualified Token
import qualified Tokenizer
import qualified Parser
import qualified Data.List as L

main :: IO ()
main = do
    s <- getLine
    putStrLn "[Tokenize]"
    let tokenM = Tokenizer.tokenize s
    case tokenM of
        Reject s -> putStrLn $ "Reject: " ++ s
        Accept a -> do
            putStrLn $ L.intercalate "\n" $ L.map debug a
            putStrLn "[Parse]"
            let treeM = Parser.parse a
            case treeM of
                Reject s -> putStrLn $ "Reject: " ++ s
                Accept a -> do
                    putStrLn $ debug a
                    putStrLn "[Evaluate]"
                    let valueM = evaluate a
                    case valueM of
                        Reject s -> putStrLn $ "Reject: " ++ s
                        Accept a -> putStrLn $ show a

showTreePosition :: Type.Tree -> String
showTreePosition (Type.Tree{ Type.lineS = ls, Type.charS = cs, Type.lineG = lg, Type.charG = cg }) = show ls ++ ":" ++ show cs ++ "-" ++ show lg ++ ":" ++ show cg

evaluate :: Type.Tree -> Result Int
evaluate t = case Type.treeV t of
    Type.Int i -> return i
    Type.Add e1 e2 -> do
        v1 <- evaluate e1
        v2 <- evaluate e2
        return $ v1 + v2
    Type.Sub e1 e2 -> do
        v1 <- evaluate e1
        v2 <- evaluate e2
        return $ v1 - v2
    Type.Mul e1 e2 -> do
        v1 <- evaluate e1
        v2 <- evaluate e2
        return $ v1 * v2
    Type.Div e1 e2 -> do
        v1 <- evaluate e1
        v2 <- evaluate e2
        if v2 /= 0 then return () else (fail $ showTreePosition t ++ " divide by zero")
        return $ v1 `div` v2
    Type.Mod e1 e2 -> do
        v1 <- evaluate e1
        v2 <- evaluate e2
        if v2 /= 0 then return () else (fail $ showTreePosition t ++ " divide by zero")
        return $ v1 `mod` v2
    Type.Minus e -> do
        v <- evaluate e
        return (- v)

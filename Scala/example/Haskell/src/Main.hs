module Main where

import Base
import qualified Type
import qualified Token
import qualified Tokenizer

main :: IO ()
main = do
    s <- getLine
    let mts = Tokenizer.tokenize s
    putStrLn $ showTokensM mts

showTokensM :: Result [Token.Token] -> String
showTokensM (Reject s) = "Rejected: " ++ s
showTokensM (Accept ts) = "Accept\n" ++ showTokens ts
    where
        showTokens :: [Token.Token] -> String
        showTokens ts = joinString "\n" $ map show ts

joinString :: String -> [String] -> String
joinString _ [] = []
joinString _ [x] = x
joinString s (x : xs) = x ++ s ++ joinString s xs

showPosition :: (Int, Int) -> String
showPosition (a, b) = "(" ++ show a ++ ", " ++ show b ++ ")"

module Type where

data SyntaxTree = STInt Int | STAdd SyntaxTree SyntaxTree | STMul SyntaxTree SyntaxTree deriving (Show, Eq)

data Token = Token { tokenT :: TokenT, line :: Int, character :: Int } deriving Show
data TokenT = TokenInt Int | TokenPlus | TokenStar | TokenLPar | TokenRPar deriving Show

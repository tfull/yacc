module Parser(parse) where

import Type
import qualified Token

import qualified Data.Array as A
import qualified Data.List

data TFParserItem = TFParserToken Token.Token | TFParserTree Tree

type TFParserStack = [(Int, TFParserItem)]

parse :: Monad m => [Token.Token] -> m Tree
parse tokens = tfParserLoop [(0, undefined)] tokens

tfParserLoop :: Monad m => TFParserStack -> [Token.Token] -> m Tree
tfParserLoop stack@((cond, item) : ss) tokens@(t : ts)
    | tfParserAccept A.! cond A.! tfParserGetTokenID t =
        case (ts, item) of
            ([], TFParserTree t) -> return t
            _ -> fail "error in accept"
    | tfParserShift A.! cond A.! tfParserGetTokenID t /= -1 =
        tfParserLoop ((tfParserShift A.! cond A.! tfParserGetTokenID t, TFParserToken t) : stack) ts
    | tfParserReduce A.! cond A.! tfParserGetTokenID t /= -1 =
        let (itemN, f, gotoN) = tfParserReduceProperties A.! (tfParserReduce A.! cond A.! tfParserGetTokenID t) in
        let (cis, remains@((remi, remt) : rems)) = Data.List.splitAt itemN stack in
        tfParserLoop ((tfParserGoto A.! remi A.! gotoN, f $ Data.List.map snd $ Data.List.reverse cis) : remains) tokens
    | otherwise = fail "not acceptable"

tfParserGetTokenID :: Token.Token -> Int
tfParserGetTokenID (Token.Token {Token.tokenV = Token.Int _}) = 0
tfParserGetTokenID (Token.Token {Token.tokenV = Token.RPar}) = 4
tfParserGetTokenID (Token.Token {Token.tokenV = Token.LPar}) = 3
tfParserGetTokenID (Token.Token {Token.tokenV = Token.Star}) = 2
tfParserGetTokenID (Token.Token {Token.tokenV = Token.Plus}) = 1
tfParserGetTokenID (Token.Token {Token.tokenV = Token.Slash}) = 6
tfParserGetTokenID (Token.Token {Token.tokenV = Token.Minus}) = 5
tfParserGetTokenID (Token.Token {Token.tokenV = Token.End}) = 7
tfParserGetTokenID (Token.Token {Token.tokenV = Token.Percent}) = 8

tfParserAccept0 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept1 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept2 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept3 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept4 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,True),(8,False)]
tfParserAccept5 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept6 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept7 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept8 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept9 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept10 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept11 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept12 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept13 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept14 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept15 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept16 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept17 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept18 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept19 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept20 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept21 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept22 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept23 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept24 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept25 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept26 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept27 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept28 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept29 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept30 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept31 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept32 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept33 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept34 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept35 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept36 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept37 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept38 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]
tfParserAccept39 = A.array (0, 8) [(0,False),(1,False),(2,False),(3,False),(4,False),(5,False),(6,False),(7,False),(8,False)]

tfParserAccept :: A.Array Int (A.Array Int Bool)
tfParserAccept = A.array (0, 39) [(0,tfParserAccept0),(1,tfParserAccept1),(2,tfParserAccept2),(3,tfParserAccept3),(4,tfParserAccept4),(5,tfParserAccept5),(6,tfParserAccept6),(7,tfParserAccept7),(8,tfParserAccept8),(9,tfParserAccept9),(10,tfParserAccept10),(11,tfParserAccept11),(12,tfParserAccept12),(13,tfParserAccept13),(14,tfParserAccept14),(15,tfParserAccept15),(16,tfParserAccept16),(17,tfParserAccept17),(18,tfParserAccept18),(19,tfParserAccept19),(20,tfParserAccept20),(21,tfParserAccept21),(22,tfParserAccept22),(23,tfParserAccept23),(24,tfParserAccept24),(25,tfParserAccept25),(26,tfParserAccept26),(27,tfParserAccept27),(28,tfParserAccept28),(29,tfParserAccept29),(30,tfParserAccept30),(31,tfParserAccept31),(32,tfParserAccept32),(33,tfParserAccept33),(34,tfParserAccept34),(35,tfParserAccept35),(36,tfParserAccept36),(37,tfParserAccept37),(38,tfParserAccept38),(39,tfParserAccept39)]

tfParserShift0 = A.array (0, 8) [(0,5),(1,-1),(2,-1),(3,2),(4,-1),(5,1),(6,-1),(7,-1),(8,-1)]
tfParserShift1 = A.array (0, 8) [(0,5),(1,-1),(2,-1),(3,2),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift2 = A.array (0, 8) [(0,17),(1,-1),(2,-1),(3,15),(4,-1),(5,13),(6,-1),(7,-1),(8,-1)]
tfParserShift3 = A.array (0, 8) [(0,-1),(1,-1),(2,10),(3,-1),(4,-1),(5,-1),(6,9),(7,-1),(8,8)]
tfParserShift4 = A.array (0, 8) [(0,-1),(1,12),(2,-1),(3,-1),(4,-1),(5,11),(6,-1),(7,-1),(8,-1)]
tfParserShift5 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift6 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift7 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift8 = A.array (0, 8) [(0,5),(1,-1),(2,-1),(3,2),(4,-1),(5,1),(6,-1),(7,-1),(8,-1)]
tfParserShift9 = A.array (0, 8) [(0,5),(1,-1),(2,-1),(3,2),(4,-1),(5,1),(6,-1),(7,-1),(8,-1)]
tfParserShift10 = A.array (0, 8) [(0,5),(1,-1),(2,-1),(3,2),(4,-1),(5,1),(6,-1),(7,-1),(8,-1)]
tfParserShift11 = A.array (0, 8) [(0,5),(1,-1),(2,-1),(3,2),(4,-1),(5,1),(6,-1),(7,-1),(8,-1)]
tfParserShift12 = A.array (0, 8) [(0,5),(1,-1),(2,-1),(3,2),(4,-1),(5,1),(6,-1),(7,-1),(8,-1)]
tfParserShift13 = A.array (0, 8) [(0,17),(1,-1),(2,-1),(3,15),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift14 = A.array (0, 8) [(0,-1),(1,26),(2,-1),(3,-1),(4,27),(5,25),(6,-1),(7,-1),(8,-1)]
tfParserShift15 = A.array (0, 8) [(0,17),(1,-1),(2,-1),(3,15),(4,-1),(5,13),(6,-1),(7,-1),(8,-1)]
tfParserShift16 = A.array (0, 8) [(0,-1),(1,-1),(2,30),(3,-1),(4,-1),(5,-1),(6,29),(7,-1),(8,28)]
tfParserShift17 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift18 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift19 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift20 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift21 = A.array (0, 8) [(0,-1),(1,26),(2,-1),(3,-1),(4,34),(5,25),(6,-1),(7,-1),(8,-1)]
tfParserShift22 = A.array (0, 8) [(0,-1),(1,-1),(2,10),(3,-1),(4,-1),(5,-1),(6,9),(7,-1),(8,8)]
tfParserShift23 = A.array (0, 8) [(0,-1),(1,-1),(2,10),(3,-1),(4,-1),(5,-1),(6,9),(7,-1),(8,8)]
tfParserShift24 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift25 = A.array (0, 8) [(0,17),(1,-1),(2,-1),(3,15),(4,-1),(5,13),(6,-1),(7,-1),(8,-1)]
tfParserShift26 = A.array (0, 8) [(0,17),(1,-1),(2,-1),(3,15),(4,-1),(5,13),(6,-1),(7,-1),(8,-1)]
tfParserShift27 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift28 = A.array (0, 8) [(0,17),(1,-1),(2,-1),(3,15),(4,-1),(5,13),(6,-1),(7,-1),(8,-1)]
tfParserShift29 = A.array (0, 8) [(0,17),(1,-1),(2,-1),(3,15),(4,-1),(5,13),(6,-1),(7,-1),(8,-1)]
tfParserShift30 = A.array (0, 8) [(0,17),(1,-1),(2,-1),(3,15),(4,-1),(5,13),(6,-1),(7,-1),(8,-1)]
tfParserShift31 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift32 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift33 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift34 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift35 = A.array (0, 8) [(0,-1),(1,-1),(2,30),(3,-1),(4,-1),(5,-1),(6,29),(7,-1),(8,28)]
tfParserShift36 = A.array (0, 8) [(0,-1),(1,-1),(2,30),(3,-1),(4,-1),(5,-1),(6,29),(7,-1),(8,28)]
tfParserShift37 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift38 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserShift39 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]

tfParserShift :: A.Array Int (A.Array Int Int)
tfParserShift = A.array (0, 39) [(0,tfParserShift0),(1,tfParserShift1),(2,tfParserShift2),(3,tfParserShift3),(4,tfParserShift4),(5,tfParserShift5),(6,tfParserShift6),(7,tfParserShift7),(8,tfParserShift8),(9,tfParserShift9),(10,tfParserShift10),(11,tfParserShift11),(12,tfParserShift12),(13,tfParserShift13),(14,tfParserShift14),(15,tfParserShift15),(16,tfParserShift16),(17,tfParserShift17),(18,tfParserShift18),(19,tfParserShift19),(20,tfParserShift20),(21,tfParserShift21),(22,tfParserShift22),(23,tfParserShift23),(24,tfParserShift24),(25,tfParserShift25),(26,tfParserShift26),(27,tfParserShift27),(28,tfParserShift28),(29,tfParserShift29),(30,tfParserShift30),(31,tfParserShift31),(32,tfParserShift32),(33,tfParserShift33),(34,tfParserShift34),(35,tfParserShift35),(36,tfParserShift36),(37,tfParserShift37),(38,tfParserShift38),(39,tfParserShift39)]

tfParserReduce0 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce1 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce2 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce3 = A.array (0, 8) [(0,-1),(1,3),(2,-1),(3,-1),(4,-1),(5,3),(6,-1),(7,3),(8,-1)]
tfParserReduce4 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce5 = A.array (0, 8) [(0,-1),(1,10),(2,10),(3,-1),(4,-1),(5,10),(6,10),(7,10),(8,10)]
tfParserReduce6 = A.array (0, 8) [(0,-1),(1,9),(2,9),(3,-1),(4,-1),(5,9),(6,9),(7,9),(8,9)]
tfParserReduce7 = A.array (0, 8) [(0,-1),(1,7),(2,7),(3,-1),(4,-1),(5,7),(6,7),(7,7),(8,7)]
tfParserReduce8 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce9 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce10 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce11 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce12 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce13 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce14 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce15 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce16 = A.array (0, 8) [(0,-1),(1,3),(2,-1),(3,-1),(4,3),(5,3),(6,-1),(7,-1),(8,-1)]
tfParserReduce17 = A.array (0, 8) [(0,-1),(1,10),(2,10),(3,-1),(4,10),(5,10),(6,10),(7,-1),(8,10)]
tfParserReduce18 = A.array (0, 8) [(0,-1),(1,9),(2,9),(3,-1),(4,9),(5,9),(6,9),(7,-1),(8,9)]
tfParserReduce19 = A.array (0, 8) [(0,-1),(1,7),(2,7),(3,-1),(4,7),(5,7),(6,7),(7,-1),(8,7)]
tfParserReduce20 = A.array (0, 8) [(0,-1),(1,8),(2,8),(3,-1),(4,-1),(5,8),(6,8),(7,8),(8,8)]
tfParserReduce21 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce22 = A.array (0, 8) [(0,-1),(1,2),(2,-1),(3,-1),(4,-1),(5,2),(6,-1),(7,2),(8,-1)]
tfParserReduce23 = A.array (0, 8) [(0,-1),(1,1),(2,-1),(3,-1),(4,-1),(5,1),(6,-1),(7,1),(8,-1)]
tfParserReduce24 = A.array (0, 8) [(0,-1),(1,8),(2,8),(3,-1),(4,8),(5,8),(6,8),(7,-1),(8,8)]
tfParserReduce25 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce26 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce27 = A.array (0, 8) [(0,-1),(1,11),(2,11),(3,-1),(4,-1),(5,11),(6,11),(7,11),(8,11)]
tfParserReduce28 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce29 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce30 = A.array (0, 8) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1),(5,-1),(6,-1),(7,-1),(8,-1)]
tfParserReduce31 = A.array (0, 8) [(0,-1),(1,6),(2,6),(3,-1),(4,-1),(5,6),(6,6),(7,6),(8,6)]
tfParserReduce32 = A.array (0, 8) [(0,-1),(1,5),(2,5),(3,-1),(4,-1),(5,5),(6,5),(7,5),(8,5)]
tfParserReduce33 = A.array (0, 8) [(0,-1),(1,4),(2,4),(3,-1),(4,-1),(5,4),(6,4),(7,4),(8,4)]
tfParserReduce34 = A.array (0, 8) [(0,-1),(1,11),(2,11),(3,-1),(4,11),(5,11),(6,11),(7,-1),(8,11)]
tfParserReduce35 = A.array (0, 8) [(0,-1),(1,1),(2,-1),(3,-1),(4,1),(5,1),(6,-1),(7,-1),(8,-1)]
tfParserReduce36 = A.array (0, 8) [(0,-1),(1,2),(2,-1),(3,-1),(4,2),(5,2),(6,-1),(7,-1),(8,-1)]
tfParserReduce37 = A.array (0, 8) [(0,-1),(1,4),(2,4),(3,-1),(4,4),(5,4),(6,4),(7,-1),(8,4)]
tfParserReduce38 = A.array (0, 8) [(0,-1),(1,5),(2,5),(3,-1),(4,5),(5,5),(6,5),(7,-1),(8,5)]
tfParserReduce39 = A.array (0, 8) [(0,-1),(1,6),(2,6),(3,-1),(4,6),(5,6),(6,6),(7,-1),(8,6)]

tfParserReduce :: A.Array Int (A.Array Int Int)
tfParserReduce = A.array (0, 39) [(0,tfParserReduce0),(1,tfParserReduce1),(2,tfParserReduce2),(3,tfParserReduce3),(4,tfParserReduce4),(5,tfParserReduce5),(6,tfParserReduce6),(7,tfParserReduce7),(8,tfParserReduce8),(9,tfParserReduce9),(10,tfParserReduce10),(11,tfParserReduce11),(12,tfParserReduce12),(13,tfParserReduce13),(14,tfParserReduce14),(15,tfParserReduce15),(16,tfParserReduce16),(17,tfParserReduce17),(18,tfParserReduce18),(19,tfParserReduce19),(20,tfParserReduce20),(21,tfParserReduce21),(22,tfParserReduce22),(23,tfParserReduce23),(24,tfParserReduce24),(25,tfParserReduce25),(26,tfParserReduce26),(27,tfParserReduce27),(28,tfParserReduce28),(29,tfParserReduce29),(30,tfParserReduce30),(31,tfParserReduce31),(32,tfParserReduce32),(33,tfParserReduce33),(34,tfParserReduce34),(35,tfParserReduce35),(36,tfParserReduce36),(37,tfParserReduce37),(38,tfParserReduce38),(39,tfParserReduce39)]

tfParserGoto0 = A.array (0, 4) [(0,-1),(1,6),(2,7),(3,3),(4,4)]
tfParserGoto1 = A.array (0, 4) [(0,-1),(1,20),(2,-1),(3,-1),(4,-1)]
tfParserGoto2 = A.array (0, 4) [(0,-1),(1,18),(2,19),(3,16),(4,14)]
tfParserGoto3 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto4 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto5 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto6 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto7 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto8 = A.array (0, 4) [(0,-1),(1,6),(2,31),(3,-1),(4,-1)]
tfParserGoto9 = A.array (0, 4) [(0,-1),(1,6),(2,32),(3,-1),(4,-1)]
tfParserGoto10 = A.array (0, 4) [(0,-1),(1,6),(2,33),(3,-1),(4,-1)]
tfParserGoto11 = A.array (0, 4) [(0,-1),(1,6),(2,7),(3,22),(4,-1)]
tfParserGoto12 = A.array (0, 4) [(0,-1),(1,6),(2,7),(3,23),(4,-1)]
tfParserGoto13 = A.array (0, 4) [(0,-1),(1,24),(2,-1),(3,-1),(4,-1)]
tfParserGoto14 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto15 = A.array (0, 4) [(0,-1),(1,18),(2,19),(3,16),(4,21)]
tfParserGoto16 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto17 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto18 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto19 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto20 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto21 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto22 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto23 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto24 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto25 = A.array (0, 4) [(0,-1),(1,18),(2,19),(3,36),(4,-1)]
tfParserGoto26 = A.array (0, 4) [(0,-1),(1,18),(2,19),(3,35),(4,-1)]
tfParserGoto27 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto28 = A.array (0, 4) [(0,-1),(1,18),(2,39),(3,-1),(4,-1)]
tfParserGoto29 = A.array (0, 4) [(0,-1),(1,18),(2,38),(3,-1),(4,-1)]
tfParserGoto30 = A.array (0, 4) [(0,-1),(1,18),(2,37),(3,-1),(4,-1)]
tfParserGoto31 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto32 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto33 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto34 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto35 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto36 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto37 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto38 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]
tfParserGoto39 = A.array (0, 4) [(0,-1),(1,-1),(2,-1),(3,-1),(4,-1)]

tfParserGoto :: A.Array Int (A.Array Int Int)
tfParserGoto = A.array (0, 39) [(0,tfParserGoto0),(1,tfParserGoto1),(2,tfParserGoto2),(3,tfParserGoto3),(4,tfParserGoto4),(5,tfParserGoto5),(6,tfParserGoto6),(7,tfParserGoto7),(8,tfParserGoto8),(9,tfParserGoto9),(10,tfParserGoto10),(11,tfParserGoto11),(12,tfParserGoto12),(13,tfParserGoto13),(14,tfParserGoto14),(15,tfParserGoto15),(16,tfParserGoto16),(17,tfParserGoto17),(18,tfParserGoto18),(19,tfParserGoto19),(20,tfParserGoto20),(21,tfParserGoto21),(22,tfParserGoto22),(23,tfParserGoto23),(24,tfParserGoto24),(25,tfParserGoto25),(26,tfParserGoto26),(27,tfParserGoto27),(28,tfParserGoto28),(29,tfParserGoto29),(30,tfParserGoto30),(31,tfParserGoto31),(32,tfParserGoto32),(33,tfParserGoto33),(34,tfParserGoto34),(35,tfParserGoto35),(36,tfParserGoto36),(37,tfParserGoto37),(38,tfParserGoto38),(39,tfParserGoto39)]

tfParserReduceFunction0 [TFParserTree (Tree { treeV = x1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }), TFParserToken (Token.Token { Token.lineS = ls2, Token.charS = cs2, Token.lineG = lg2, Token.charG = cg2 })] = TFParserTree $ Tree { treeV = x1, lineS = ls1, charS = cs1, lineG = lg2, charG = cg2 }
tfParserReduceFunction1 [TFParserTree x1@(Tree { treeV = y1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }), TFParserToken (Token.Token { Token.lineS = ls2, Token.charS = cs2, Token.lineG = lg2, Token.charG = cg2 }), TFParserTree x3@(Tree { treeV = y3, lineS = ls3, charS = cs3, lineG = lg3, charG = cg3 })] = TFParserTree $ Tree { treeV = Add x1 x3, lineS = ls1, charS = cs1, lineG = lg3, charG = cg3 }
tfParserReduceFunction2 [TFParserTree x1@(Tree { treeV = y1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }), TFParserToken (Token.Token { Token.lineS = ls2, Token.charS = cs2, Token.lineG = lg2, Token.charG = cg2 }), TFParserTree x3@(Tree { treeV = y3, lineS = ls3, charS = cs3, lineG = lg3, charG = cg3 })] = TFParserTree $ Tree { treeV = Sub x1 x3, lineS = ls1, charS = cs1, lineG = lg3, charG = cg3 }
tfParserReduceFunction3 [TFParserTree (Tree { treeV = x1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 })] = TFParserTree $ Tree { treeV = x1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }
tfParserReduceFunction4 [TFParserTree x1@(Tree { treeV = y1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }), TFParserToken (Token.Token { Token.lineS = ls2, Token.charS = cs2, Token.lineG = lg2, Token.charG = cg2 }), TFParserTree x3@(Tree { treeV = y3, lineS = ls3, charS = cs3, lineG = lg3, charG = cg3 })] = TFParserTree $ Tree { treeV = Mul x1 x3, lineS = ls1, charS = cs1, lineG = lg3, charG = cg3 }
tfParserReduceFunction5 [TFParserTree x1@(Tree { treeV = y1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }), TFParserToken (Token.Token { Token.lineS = ls2, Token.charS = cs2, Token.lineG = lg2, Token.charG = cg2 }), TFParserTree x3@(Tree { treeV = y3, lineS = ls3, charS = cs3, lineG = lg3, charG = cg3 })] = TFParserTree $ Tree { treeV = Div x1 x3, lineS = ls1, charS = cs1, lineG = lg3, charG = cg3 }
tfParserReduceFunction6 [TFParserTree x1@(Tree { treeV = y1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }), TFParserToken (Token.Token { Token.lineS = ls2, Token.charS = cs2, Token.lineG = lg2, Token.charG = cg2 }), TFParserTree x3@(Tree { treeV = y3, lineS = ls3, charS = cs3, lineG = lg3, charG = cg3 })] = TFParserTree $ Tree { treeV = Mod x1 x3, lineS = ls1, charS = cs1, lineG = lg3, charG = cg3 }
tfParserReduceFunction7 [TFParserTree (Tree { treeV = x1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 })] = TFParserTree $ Tree { treeV = x1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }
tfParserReduceFunction8 [TFParserToken (Token.Token { Token.lineS = ls1, Token.charS = cs1, Token.lineG = lg1, Token.charG = cg1 }), TFParserTree x2@(Tree { treeV = y2, lineS = ls2, charS = cs2, lineG = lg2, charG = cg2 })] = TFParserTree $ Tree { treeV = Minus x2, lineS = ls1, charS = cs1, lineG = lg2, charG = cg2 }
tfParserReduceFunction9 [TFParserTree (Tree { treeV = x1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 })] = TFParserTree $ Tree { treeV = x1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }
tfParserReduceFunction10 [TFParserToken (Token.Token { Token.tokenV = Token.Int x1, Token.lineS = ls1, Token.charS = cs1, Token.lineG = lg1, Token.charG = cg1 })] = TFParserTree $ Tree { treeV = Int x1, lineS = ls1, charS = cs1, lineG = lg1, charG = cg1 }
tfParserReduceFunction11 [TFParserToken (Token.Token { Token.lineS = ls1, Token.charS = cs1, Token.lineG = lg1, Token.charG = cg1 }), TFParserTree (Tree { treeV = x2, lineS = ls2, charS = cs2, lineG = lg2, charG = cg2 }), TFParserToken (Token.Token { Token.lineS = ls3, Token.charS = cs3, Token.lineG = lg3, Token.charG = cg3 })] = TFParserTree $ Tree { treeV = x2, lineS = ls1, charS = cs1, lineG = lg3, charG = cg3 }

tfParserReduceProperties :: A.Array Int (Int, [TFParserItem] -> TFParserItem, Int)
tfParserReduceProperties = A.array (0, 12) [(0,(2,tfParserReduceFunction0,0)),(1,(3,tfParserReduceFunction1,4)),(2,(3,tfParserReduceFunction2,4)),(3,(1,tfParserReduceFunction3,4)),(4,(3,tfParserReduceFunction4,3)),(5,(3,tfParserReduceFunction5,3)),(6,(3,tfParserReduceFunction6,3)),(7,(1,tfParserReduceFunction7,3)),(8,(2,tfParserReduceFunction8,2)),(9,(1,tfParserReduceFunction9,2)),(10,(1,tfParserReduceFunction10,1)),(11,(3,tfParserReduceFunction11,1))]

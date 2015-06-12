package yacc

import java.io.PrintWriter
import java.util.Scanner
import scala.io.Source
import scala.collection.mutable

object Haskell{
    def main(args: Array[String]){
        val (file_i, file_o, file_e): (String, String, String) = Argument.parse(args)
        val parser: Parser = Parser(file_i)
        val yacc: YACC = new YACC(parser.rules.map(_._1), parser.terminal_map, parser.non_terminal_set)
        val writer: PrintWriter = new PrintWriter(file_o)
        val yacc_item = yacc.makeGraph(file_e)
        val indent: String = "    "

        writer.println(parser.header)

        writer.println("import qualified Data.Array as A")
        writer.println("import qualified Data.List")

        writer.println()

        writer.println("data TFParserItem = TFParserToken %s | TFParserTree %s".format(parser.token_class, parser.tree_class))

        writer.println()

        writer.println("type TFParserStack = [(Int, TFParserItem)]")

        writer.println()

        writer.println("parse :: Monad m => [%s] -> m %s".format(parser.token_class, parser.tree_class))
        writer.println("parse tokens = tfParserLoop [(0, undefined)] tokens")
        writer.println()
        writer.println("tfParserLoop :: Monad m => TFParserStack -> [%s] -> m %s".format(parser.token_class, parser.tree_class))
        writer.println("tfParserLoop stack@((cond, item) : ss) tokens@(t : ts)")
        writer.println(indent + "| tfParserAccept A.! cond A.! tfParserGetTokenID t =")
        writer.println(indent * 2 + "case (ts, item) of")
        writer.println(indent * 3 + "([], TFParserTree t) -> return t")
        writer.println(indent * 3 + "_ -> fail \"error in accept\"")
        writer.println(indent + "| tfParserShift A.! cond A.! tfParserGetTokenID t /= -1 =")
        writer.println(indent * 2 + "tfParserLoop ((tfParserShift A.! cond A.! tfParserGetTokenID t, TFParserToken t) : stack) ts")
        writer.println(indent + "| tfParserReduce A.! cond A.! tfParserGetTokenID t /= -1 =")
        writer.println(indent * 2 + "let (itemN, f, gotoN) = tfParserReduceProperties A.! (tfParserReduce A.! cond A.! tfParserGetTokenID t) in")
        writer.println(indent * 2 + "let (cis, remains@((remi, remt) : rems)) = Data.List.splitAt itemN stack in")
        writer.println(indent * 2 + "tfParserLoop ((tfParserGoto A.! remi A.! gotoN, f $ Data.List.map snd $ Data.List.reverse cis) : remains) tokens")
        writer.println(indent + "| otherwise = fail \"not acceptable\"")

        writer.println()

        writer.println("tfParserGetTokenID :: %s -> Int".format(parser.token_class))
        for(t <- parser.terminal_map.keys){
            val i: Int = yacc.terminal_imap.keyA(t)
            val s: String = parser.terminal_map(t)
            if(parser.terminal_value_map.contains(t)){
                writer.println("tfParserGetTokenID (%s {%s = %s _}) = %d".format(parser.token_class, parser.token_value, s, i))
            }else{
                writer.println("tfParserGetTokenID (%s {%s = %s}) = %d".format(parser.token_class, parser.token_value, s, i))
            }
        }

        writer.println()

        for(i <- 0 until yacc_item.accept.size){
            var n: Int = yacc_item.accept(i).size
            var a: Array[String] = new Array[String](n)
            for(j <- 0 until n){
                a(j) = "(%d,%s)".format(j, if(yacc_item.accept(i)(j)){"True"}else{"False"})
            }
            writer.println(a.mkString("tfParserAccept%d = A.array (0, %d) [".format(i, n - 1), ",", "]"))
        }

        writer.println()
        writer.println("tfParserAccept :: A.Array Int (A.Array Int Bool)")
        writer.println("tfParserAccept = A.array (0, %d) %s".format(yacc_item.accept.size - 1, Range(0, yacc_item.accept.size).map({ x => "(%d,tfParserAccept%d)".format(x, x)}).mkString("[", ",", "]")))
        writer.println()

        for(i <- 0 until yacc_item.shift.size){
            var n: Int = yacc_item.shift(i).size
            var a: Array[String] = new Array[String](n)
            for(j <- 0 until n){
                a(j) = "(%d,%d)".format(j, yacc_item.shift(i)(j))
            }
            writer.println(a.mkString("tfParserShift%d = A.array (0, %d) [".format(i, n - 1), ",", "]"))
        }

        writer.println()
        writer.println("tfParserShift :: A.Array Int (A.Array Int Int)")
        writer.println("tfParserShift = A.array (0, %d) %s".format(yacc_item.shift.size - 1, Range(0, yacc_item.shift.size).map({ x => "(%d,tfParserShift%d)".format(x, x)}).mkString("[", ",", "]")))
        writer.println()

        for(i <- 0 until yacc_item.reduce.size){
            var n: Int = yacc_item.reduce(i).size
            var a: Array[String] = new Array[String](n)
            for(j <- 0 until n){
                a(j) = "(%d,%d)".format(j, yacc_item.reduce(i)(j))
            }
            writer.println(a.mkString("tfParserReduce%d = A.array (0, %d) [".format(i, n - 1), ",", "]"))
        }

        writer.println()
        writer.println("tfParserReduce :: A.Array Int (A.Array Int Int)")
        writer.println("tfParserReduce = A.array (0, %d) %s".format(yacc_item.reduce.size - 1, Range(0, yacc_item.reduce.size).map({ x => "(%d,tfParserReduce%d)".format(x, x)}).mkString("[", ",", "]")))
        writer.println()

        for(i <- 0 until yacc_item.goto.size){
            var n: Int = yacc_item.goto(i).size
            var a: Array[String] = new Array[String](n)
            for(j <- 0 until n){
                a(j) = "(%d,%d)".format(j, yacc_item.goto(i)(j))
            }
            writer.println(a.mkString("tfParserGoto%d = A.array (0, %d) [".format(i, n - 1), ",", "]"))
        }

        writer.println()
        writer.println("tfParserGoto :: A.Array Int (A.Array Int Int)")
        writer.println("tfParserGoto = A.array (0, %d) %s".format(yacc_item.goto.size - 1, Range(0, yacc_item.goto.size).map({ x => "(%d,tfParserGoto%d)".format(x, x)}).mkString("[", ",", "]")))

        writer.println()

        def getItemsString(rule: ((NonTerminal, SymbolArray), YTree)): String = {
            val ytree = rule._2
            val sa = rule._1._2
            val n: Int = sa.size
            var a: Array[String] = new Array[String](n)

            def f(tree: YTree, flag: Boolean){
                tree match{
                    case leaf: YLeaf => {
                        sa(leaf.number - 1) match{
                            case t: Terminal => {
                                if(parser.terminal_value_map.contains(t)){
                                    a(leaf.number - 1) = "TFParserToken (%s { %s = %s x%d, %s = ls%d, %s = cs%d, %s = lg%d, %s = cg%d })".format(parser.token_class, parser.token_value, parser.terminal_map(t), leaf.number, parser.token_position(0), leaf.number, parser.token_position(1), leaf.number, parser.token_position(2), leaf.number, parser.token_position(3), leaf.number)
                                }else{
                                    throw new Exception("access terminal")
                                }
                            }
                            case n: NonTerminal => {
                                if(flag){
                                    a(leaf.number - 1) = "TFParserTree (%s { %s = x%d, %s = ls%d, %s = cs%d, %s = lg%d, %s = cg%d })".format(parser.tree_class, parser.tree_value, leaf.number, parser.tree_position(0), leaf.number, parser.tree_position(1), leaf.number, parser.tree_position(2), leaf.number, parser.tree_position(3), leaf.number)
                                }else{
                                    a(leaf.number - 1) = "TFParserTree x%d@(%s { %s = y%d, %s = ls%d, %s = cs%d, %s = lg%d, %s = cg%d })".format(leaf.number, parser.tree_class, parser.tree_value, leaf.number, parser.tree_position(0), leaf.number, parser.tree_position(1), leaf.number, parser.tree_position(2), leaf.number, parser.tree_position(3), leaf.number)
                                }
                            }
                        }
                    }
                    case node: YNode => {
                        for(t <- node.trees){
                            f(t, false)
                        }
                    }
                }
            }

            f(ytree, true)

            for(i <- 0 until a.size){
                if(a(i) == null){
                    a(i) = "TFParserToken (%s { %s = ls%d, %s = cs%d, %s = lg%d, %s = cg%d })".format(parser.token_class, parser.token_position(0), i + 1, parser.token_position(1), i + 1, parser.token_position(2), i + 1, parser.token_position(3), i + 1)
                }
            }

            a.mkString("[", ", ", "]")
        }

        def getTreeString(tree: YTree): String = {
            tree match{
                case leaf: YLeaf => "x" + leaf.number.toString
                case node: YNode => {
                    if(node.trees.size == 0){
                        node.name
                    }else{
                        node.trees.map({ x =>
                            x match{
                                case leaf: YLeaf => "x" + leaf.number.toString
                                case node: YNode => {
                                    if(node.trees.size == 0){
                                        node.name
                                    }else{
                                        node.name + "(" + getTreeString(x) + ")"
                                    }
                                }
                            }
                        }).mkString(node.name + " ", " ", "")
                    }
                }
            }
        }

        for((prod, tree) <- parser.rules){
            val index: Int = yacc.production_imap.keyA(prod)
            writer.println("tfParserReduceFunction%d %s = TFParserTree $ %s { %s = %s, %s = ls1, %s = cs1, %s = lg%d, %s = cg%d }".format(index, getItemsString((prod, tree)), parser.tree_class, parser.tree_value, getTreeString(tree), parser.tree_position(0), parser.tree_position(1), parser.tree_position(2), prod._2.size, parser.tree_position(3), prod._2.size))
        }

        writer.println()

        writer.println("tfParserReduceProperties :: A.Array Int (Int, [TFParserItem] -> TFParserItem, Int)")
        writer.println("tfParserReduceProperties = A.array (0, %d) [%s]".format(parser.rules.size, Range(0, parser.rules.size).map({ x => "(%d,(%d,tfParserReduceFunction%d,%d))".format(x, parser.rules(x)._1._2.size, x, yacc.non_terminal_imap.keyA(parser.rules(x)._1._1)) }).mkString(",")))

        writer.close()
    }

    class Parser(val header: String, val token_class: String, val token_value: String, val token_position: Array[String], val terminal_map: Map[Terminal, String], val terminal_value_map: Map[Terminal, String], val tree_class: String, val tree_value: String, val tree_position: Array[String], val non_terminal_set: Set[NonTerminal], val rules: Array[((NonTerminal, SymbolArray), YTree)])

    object Parser{
        def getSymbol(ts: mutable.ArrayBuffer[Symbol], x: Symbol): Symbol = {
            for(t <- ts){
                if(t == x){
                    return t
                }
            }
            ts += x
            x
        }

        def apply(fname: String): Parser = {
            var mode: Int = 0

            var header: mutable.ArrayBuffer[String] = null

            var symbols: mutable.ArrayBuffer[Symbol] = mutable.ArrayBuffer[Symbol]()

            var token_class: String = null
            var token_value: String = null
            var token_position: Array[String] = null
            var tree_class: String = null
            var tree_position: Array[String] = null
            var terminal_map: mutable.Map[Terminal, String] = null
            var tree_value: String = null
            var terminal_value_map: mutable.Map[Terminal, String] = null

            val source = Source.fromFile(fname)
            val lines = source.getLines

            var rules: mutable.ArrayBuffer[((NonTerminal, SymbolArray), YTree)] = null

            var line_n = 0

            lines.foreach({ line =>
                line_n += 1

                if(line.length == 0 || line(0) == '/'){
                    if(mode == 1){
                        header += line
                    }
                }else if(line(0) == '%'){
                    var scanner = new Scanner(line)

                    scanner.next() match{
                        case "%header" => {
                            mode = 1
                            header = mutable.ArrayBuffer[String]()
                        }
                        case "%token_class" => {
                            mode = 0
                            token_class = scanner.next()
                        }
                        case "%token_value" => {
                            mode = 0
                            token_value = scanner.next()
                        }
                        case "%token_position" => {
                            mode = 0
                            var s1: String = scanner.next()
                            var s2: String = scanner.next()
                            var s3: String = scanner.next()
                            var s4: String = scanner.next()
                            token_position = Array(s1, s2, s3, s4)
                        }
                        case "%terminal_map" => {
                            mode = 2
                            terminal_map = mutable.Map[Terminal, String]()
                            terminal_value_map = mutable.Map[Terminal, String]()
                        }
                        case "%tree_class" => {
                            mode = 0
                            tree_class = scanner.next()
                        }
                        case "%tree_value" => {
                            mode = 0
                            tree_value = scanner.next()
                        }
                        case "%tree_position" => {
                            mode = 0
                            var s1: String = scanner.next()
                            var s2: String = scanner.next()
                            var s3: String = scanner.next()
                            var s4: String = scanner.next()
                            tree_position = Array(s1, s2, s3, s4)
                        }
                        case "%rule" => {
                            mode = 3
                            rules = mutable.ArrayBuffer()
                        }
                        case _ => {
                            mode = 0
                        }
                    }
                }else{
                    if(mode == 1){
                        header += line
                    }else if(mode == 2){
                        val scanner: Scanner = new Scanner(line)
                        val t: Terminal = getSymbol(symbols, new Terminal(scanner.next())) match{
                            case t: Terminal => t
                        }
                        val s: String = scanner.next()
                        
                        terminal_map(t) = s

                        if(scanner.hasNext){
                            terminal_value_map(t) = scanner.next()
                        }
                    }else if(mode == 3){
                        val xs = line.split("%").map(_.trim)

                        if(xs.size != 3){
                            throw new Exception("line %d: wrong arguments")
                        }

                        val s = xs(0)

                        val nt: NonTerminal = {
                            if(! (s(0) >= 'A' && s(0) <= 'Z')){
                                throw new Exception("line %d: illegal non terminal")
                            }
                            getSymbol(symbols, new NonTerminal(s)) match{
                                case n: NonTerminal => n
                            }
                        }

                        val sa: Array[Symbol] = xs(1).split("\\s+").map({ x =>
                                if(x(0) >= 'A' && x(0) <= 'Z'){
                                    getSymbol(symbols, new NonTerminal(x))
                                }else if(x(0) >= 'a' && x(0) <= 'z' || x(0) == '$'){
                                    getSymbol(symbols, new Terminal(x))
                                }else{
                                    throw new Exception("line %d: no such symbol".format(line_n))
                                }
                        })

                        val yt: YTree = YACC.makeTree(xs(2))

                        rules += (((nt, new SymbolArray(sa)), yt))
                    }
                }
            })

            val non_terminal_set: Set[NonTerminal] = symbols.collect{
                case n: NonTerminal => n
            }.toSet
            
            new Parser(header.mkString("\n"), token_class, token_value, token_position, terminal_map.toMap, terminal_value_map.toMap, tree_class, tree_value, tree_position, non_terminal_set, rules.toArray)
        }
    }
}

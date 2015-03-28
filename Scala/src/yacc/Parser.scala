package yacc

import java.util.Scanner
import scala.io.Source
import scala.collection.mutable

class Parser(val modules: Array[String], val token: String, val token_enum: String, val tree: String, val tree_enum: String, val rules: Array[((NonTerminal, SymbolArray), YTree)], val terminal_map: Map[Terminal, String], val non_terminal_set: Set[NonTerminal]){
    override def toString(): String = {
        "Module(" + modules.mkString("[", ",", "]") + ")\n" + "Token(" + token + ")\n" + "TokenEnum(" + token_enum + ")\n" + "Tree(" + tree + ")\n" + "TreeEnum(" + tree_enum + ")\n" + ")\n" + "Rules(" + Parser.showRules(rules) + ")"
    }
}

object Parser{
    type Production = (NonTerminal, SymbolArray)

    def showRules(rules: Array[(Production, YTree)]): String = {
        rules.map({ rule => 
            val ((nt, xs), tr) = rule
            "{ " + nt.toString() + " -> " + xs.toString() + " => " + tr.toString() + " }"
        }).mkString("[", ",", "]")
    }

    def getTerminal(ts: mutable.ArrayBuffer[Terminal], x: Terminal): Terminal = {
        for(t <- ts){
            if(t == x){
                return t
            }
        }
        ts += x
        x
    }

    def getNonTerminal(ns: mutable.ArrayBuffer[NonTerminal], x: NonTerminal): NonTerminal = {
        for(n <- ns){
            if(n == x){
                return n
            }
        }
        ns += x
        x
    }

    def readFile(fname: String): Parser = {
        var mode = 0
        val source = Source.fromFile(fname)
        val lines = source.getLines

        var token: String = null
        var tree: String = null
        var mods: mutable.ArrayBuffer[String] = null
        var map: mutable.Map[Terminal, String] = null
        var rls: mutable.ArrayBuffer[(Production, YTree)] = null
        var token_enum: String = null
        var tree_enum: String = null
        var terminals: mutable.ArrayBuffer[Terminal] = mutable.ArrayBuffer()
        var non_terminals: mutable.ArrayBuffer[NonTerminal] = mutable.ArrayBuffer()

        lines.foreach({ line =>
            if(line.length == 0 || line(0) == '/'){
            }else if(line(0) == '%'){
                var scanner = new Scanner(line)
                scanner.next() match{
                    case "%module" => {
                        mods = mutable.ArrayBuffer()
                        mode = 1
                    }
                    case "%token" => {
                        token = scanner.next()
                    }
                    case "%token_enum" => {
                        token_enum = scanner.next()
                    }
                    case "%tree" => {
                        tree = scanner.next()
                    }
                    case "%tree_enum" => {
                        tree_enum = scanner.next()
                    }
                    case "%token_map" => {
                        map = mutable.Map[Terminal, String]()
                        mode = 2
                    }
                    case "%rule" => {
                        rls = mutable.ArrayBuffer[(Production, YTree)]()
                        mode = 3
                    }
                }
            }else{
                var scanner = new Scanner(line)
                mode match{
                    case 1 => {
                        mods += scanner.next()
                    }
                    case 2 => {
                        map(new Terminal(scanner.next())) = scanner.next()
                    }
                    case 3 => {
                        val array: Array[String] = line.split("%").map(_.trim)
                        if(array.length != 3){
                            throw new Exception()
                        }

                        val nt = getNonTerminal(non_terminals, new NonTerminal(array(0).trim))

                        val ar: SymbolArray = new SymbolArray(array(1).split("\\s+").map({ x =>
                            if(x(0) >= 'A' && x(0) <= 'Z'){
                                getNonTerminal(non_terminals, new NonTerminal(x))
                            }else if(x(0) >= 'a' && x(0) <= 'z' || x == "$"){
                                getTerminal(terminals, new Terminal(x))
                            }else{
                                throw new Exception()
                            }
                        }))

                        val br: List[String] = "(" :: array(2).replace("(", " ( ").replace(")", " ) ").split("\\s+").toList ++ List(")")

                        val (ytree, rs) = makeTree(br)

                        if(rs.length > 0){
                            throw new Exception()
                        }

                        rls += (((nt, ar), ytree))
                    }
                }
            }
        })
        new Parser(mods.toArray, token, token_enum, tree, tree_enum, rls.toArray, map.toMap, non_terminals.toSet)
    }

    def makeTree(ls: List[String]): (YTree, List[String]) = {
        ls match{
            case "(" :: xs => {
                var zs = mutable.ArrayBuffer[YTree]()
                var ys = xs
                while(ys(0) != ")"){
                    val (t, nys) = makeTree(ys)
                    zs += t
                    ys = nys
                }
                zs(0) match{
                    case yn: YNode => {
                        (new YNode(yn.name, zs.toArray.slice(1, zs.length)), ys.slice(1, ys.length))
                    }
                    case yn: YLeaf => {
                        if(zs.length > 1){
                            throw new Exception()
                        }else{
                            (yn, ys.slice(1, ys.length))
                        }
                    }
                }
            }
            case ")" :: _ => throw new Exception()
            case x :: xs => {
                if(x(0) >= 'A' && x(0) <= 'Z'){
                    (new YNode(x, Array()), xs)
                }else if(x(0) == '$'){
                    (new YLeaf(x.slice(1, x.length).toInt), xs)
                }else{
                    throw new Exception()
                }
            }
            case _ => {
                throw new Exception()
            }
        }
    }

    /*
    def debug(fname: String): Array[(NonTerminal, Array[Symbol])] = {
        val abuffer: mutable.ArrayBuffer[(NonTerminal, Array[Symbol])] = mutable.ArrayBuffer()
        val source = Source.fromFile(fname)
        val lines = source.getLines

        lines.foreach({ line =>
            val ts: Array[String] = line.split("\\s+")
            val x: NonTerminal = new NonTerminal(ts(0))
            val xs: Array[Symbol] = new Array[Symbol](ts.length - 1)

            for(i <- 1 to ts.length - 1){
                val c = ts(i).charAt(0)
                if(c >= 'A' && c <= 'Z'){
                    xs(i - 1) = new NonTerminal(ts(i))
                }else if(c >= 'a' && c <= 'z' || ts(i) == "$"){
                    xs(i - 1) = new Terminal(ts(i))
                }
            }
            abuffer += ((x, xs))
        })

        source.close

        abuffer.toArray
    }
    */
}

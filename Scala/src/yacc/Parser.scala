package yacc

import java.util.Scanner
import scala.io.Source
import scala.collection.mutable

class Parser(val modules: Array[String], val token_class: String, val token_type: (String, String), val token_value: (String, String), val token_position: Array[String], val tree_class: String, val tree_type: (String, String), val tree_value: (String, String), val tree_position: Array[String], val rules: Array[((NonTerminal, SymbolArray), YTree)], val terminal_map: Map[Terminal, String], val tree_map: Map[String, String], val non_terminal_set: Set[NonTerminal], val token_value_map: Map[Terminal, String], val tree_value_map: Map[String, String]){
    override def toString(): String = "modules(" + modules.mkString("[", ",", "]") + ")\n" + "Token(" + token_class + ")\n" + "TokenType(" + token_type + ")\n" + "Tree(" + tree_class + ")\n" + "TreeType(" + tree_type + ")\n" + ")\n" + "Rules(" + Parser.showRules(rules) + ")"
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

        var token_class: String = null
        var token_type: (String, String) = null
        var token_value: (String, String) = null
        var token_position: Array[String] = null
        var tree_class: String = null
        var tree_type: (String, String) = null
        var tree_value: (String, String) = null
        var tree_position: Array[String] = null
        var modules: mutable.ArrayBuffer[String] = null
        var terminal_map: mutable.Map[Terminal, String] = null
        var token_value_map: mutable.Map[Terminal, String] = null
        var tree_map: mutable.Map[String, String] = null
        var tree_value_map: mutable.Map[String, String] = null
        var rules: mutable.ArrayBuffer[(Production, YTree)] = null
        var terminals: mutable.ArrayBuffer[Terminal] = mutable.ArrayBuffer()
        var non_terminals: mutable.ArrayBuffer[NonTerminal] = mutable.ArrayBuffer()

        lines.foreach({ line =>
            if(line.length == 0 || line(0) == '/'){
            }else if(line(0) == '%'){
                var scanner = new Scanner(line)
                scanner.next() match{
                    case "%module" => {
                        modules = mutable.ArrayBuffer()
                        mode = 1
                    }
                    case "%token_class" => {
                        token_class = scanner.next()
                    }
                    case "%token_type" => {
                        val s1: String = scanner.next
                        if(scanner.hasNext){
                            val s2: String = scanner.next
                            token_type = (s1, s2)
                        }else{
                            token_type = (s1, null)
                        }
                    }
                    case "%token_value" => {
                        val s1: String = scanner.next
                        if(scanner.hasNext){
                            val s2: String = scanner.next
                            token_value = (s1, s2)
                        }else{
                            token_value = (s1, null)
                        }
                    }
                    case "%token_position" => {
                        var a = new Array[String](4)
                        for(i <- 0 until 4){
                            a(i) = scanner.next()
                        }
                        token_position = a
                    }
                    case "%tree_class" => {
                        tree_class = scanner.next()
                    }
                    case "%tree_type" => {
                        val s1 = scanner.next
                        if(scanner.hasNext){
                            val s2 = scanner.next
                            tree_type = (s1, s2)
                        }else{
                            tree_type = (s1, null)
                        }
                    }
                    case "%tree_value" => {
                        val s1 = scanner.next
                        if(scanner.hasNext){
                            val s2 = scanner.next
                            tree_value = (s1, s2)
                        }else{
                            tree_value = (s1, null)
                        }
                    }
                    case "%tree_position" => {
                        var a = new Array[String](4)
                        for(i <- 0 until 4){
                            a(i) = scanner.next()
                        }
                        tree_position = a
                    }
                    case "%token_map" => {
                        terminal_map = mutable.Map[Terminal, String]()
                        token_value_map = mutable.Map[Terminal, String]()
                        mode = 2
                    }
                    case "%tree_map" => {
                        tree_map = mutable.Map[String, String]()
                        tree_value_map = mutable.Map[String, String]()
                        mode = 4
                    }
                    case "%rule" => {
                        rules = mutable.ArrayBuffer[(Production, YTree)]()
                        mode = 3
                    }
                }
            }else{
                var scanner = new Scanner(line)
                mode match{
                    case 1 => {
                        modules += scanner.next()
                    }
                    case 2 => {
                        val t: Terminal = new Terminal(scanner.next)
                        val s: String = scanner.next

                        terminal_map(t) = s

                        if(scanner.hasNext){
                            token_value_map(t) = scanner.next
                        }
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

                        rules += (((nt, ar), ytree))
                    }
                    case 4 => {
                        val k = scanner.next
                        val v = scanner.next
                        val vv = scanner.next
                        tree_map(k) = v
                        tree_value_map(k) = vv
                    }
                }
            }
        })
        new Parser(modules.toArray, token_class, token_type, token_value, token_position, tree_class, tree_type, tree_value, tree_position, rules.toArray, terminal_map.toMap, tree_map.toMap, non_terminals.toSet, token_value_map.toMap, tree_value_map.toMap)
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

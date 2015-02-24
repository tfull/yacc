package yacc

import java.util.Scanner
import scala.io.Source
import scala.collection.mutable

class Parser(val modules: Array[String], val token: String, val syntax_tree: String, val token_map: Map[String, String], val rules: Array[((NonTerminal, Array[Symbol]), YTree)]){
    override def toString(): String = {
        "Module(" + modules.mkString("[", ",", "]") + ")\n" + "Token(" + token + ")\n" + "SyntaxTree(" + syntax_tree + ")\n" + "TokenMap(" + token_map.toString() + ")\n" + "Rules(" + Parser.showRules(rules) + ")"
    }
}

object Parser{
    type Production = (NonTerminal, Array[Symbol])

    def showRules(rules: Array[(Production, YTree)]): String = {
        rules.map({ rule => 
            val ((nt, xs), tr) = rule
            nt.toString() + " -> " + xs.mkString("[", ",", "]") + " => " + tr.toString()
        }).mkString("[", ",", "]")
    }

    def readFile(fname: String): Parser = {
        var mode = 0
        val source = Source.fromFile(fname)
        val lines = source.getLines

        var token: String = null
        var syntax_tree: String = null
        var mods: mutable.ArrayBuffer[String] = null
        var map: mutable.Map[String, String] = null
        var rls: mutable.ArrayBuffer[(Production, YTree)] = null

        lines.foreach({ line =>
            if(line.length == 0){
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
                    case "%syntax_tree" => {
                        syntax_tree = scanner.next()
                    }
                    case "%token_map" => {
                        map = mutable.Map[String, String]()
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
                        map(scanner.next()) = scanner.next()
                    }
                    case 3 => {
                        val array: Array[String] = line.split("%").map(_.trim)
                        if(array.length != 3){
                            throw new Exception()
                        }

                        val nt = new NonTerminal(array(0).trim)

                        val ar: Array[Symbol] = array(1).split("\\s+").map({ x =>
                            if(x(0) >= 'A' && x(0) <= 'Z'){
                                new NonTerminal(x)
                            }else if(x(0) >= 'a' && x(0) <= 'z' || x == "$"){
                                new Terminal(x)
                            }else{
                                throw new Exception()
                            }
                        })

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
        new Parser(mods.toArray, token, syntax_tree, map.toMap, rls.toArray)
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

    def show(o: Any): String = {
        o match{
            case x: Array[_] => x.map(show(_)).mkString("[",",","]")
            case x: (_, _) => "(" + show(x._1) + "," + show(x._2) + ")"
            case x => x.toString()
        }
    }
}

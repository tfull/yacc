package yacc

import java.io.PrintWriter
import scala.collection.mutable

class YACC(val productions: Array[(NonTerminal, SymbolArray)], val terminal_map: Map[Terminal, String], val non_terminal_set: Set[NonTerminal]){
    type Production = (NonTerminal, SymbolArray)
    type Term = ((NonTerminal, Location), Terminal)
    type Vertex = Set[Term]
    // type Edge = (Vertex, Symbol, Vertex)

    //val productions: Array[(NonTerminal, SymbolArray)] = rules.map(_._1)
    val nullable_set: Set[NonTerminal] = YACC.makeNullableSet(productions)
    val terminal_set: Set[Terminal] = terminal_map.map(_._1).toSet
//    val terminal_set: Set[Terminal] = YACC.gatherTerminalSet(productions)
//    val non_terminal_set: Set[NonTerminal] = YACC.gatherNonTerminalSet(productions)
    val first_map: Map[Symbol, Set[Terminal]] = YACC.makeFirstSets(productions, nullable_set, terminal_set)

    val terminal_n: Int = terminal_set.size
    val non_terminal_n: Int = non_terminal_set.size

    val terminal_imap: InteractiveMap[Terminal, Int] = {
        var index: Int = 0
        var m: mutable.Map[Terminal, Int] = mutable.Map()
        for(t <- terminal_set){
            m += { t -> index }
            index += 1
        }
        new InteractiveMap[Terminal, Int](m.toMap)
    }
    val non_terminal_imap: InteractiveMap[NonTerminal, Int] = {
        var index: Int = 0
        var m: mutable.Map[NonTerminal, Int] = mutable.Map()
        for(n <- non_terminal_set){
            m += { n -> index }
            index += 1
        }
        new InteractiveMap[NonTerminal, Int](m.toMap)
    }
    val production_imap: InteractiveMap[Production, Int] = {
        var index: Int = 0
        var m: mutable.Map[Production, Int] = mutable.Map()
        for(p <- productions){
            m += { p -> index }
            index += 1
        }
        new InteractiveMap[Production, Int](m.toMap)
    }

    def first(xs: Array[Symbol]): Set[Terminal] = {
        var set: Set[Terminal] = Set()
        var flag = true
        var i = 0
        while(i < xs.length && flag){
            set = set | first_map(xs(i))
            flag = xs(i) match{
                case n: NonTerminal => nullable_set.contains(n)
                case _ => false
            }
            i += 1
        }
        set
    }

    def closure(v: Vertex): Vertex = {
        var flag = true
        var ver = mutable.Set(v.toArray:_*)

        while(flag){
            flag = false
            for(term <- ver.toArray){
                val ((a, l), z) = term
                if(! l.isBottom){
                    l.next match{
                        case n: NonTerminal => {
                            var beta = l.following
                            for(prod <- this.productions){
                                if(prod._1 == n){
                                    for(w <- first(beta ++ Array(z))){
                                        var tm: Term = (((n, new Location(prod._2, 0)), w))
                                        if(! ver.contains(tm)){
                                            ver += tm
                                            flag = true
                                        }
                                    }
                                }
                            }
                        }
                        case _ => ;
                    }
                }
            }
        }
        ver.toSet
    }

    def goto(v: Vertex, q: Symbol): Vertex = {
        var w: mutable.Set[Term] = mutable.Set()
        for(term <- v){
            var ((a, l), z) = term
            if(! l.isBottom){
                var x = l.next
                if(x == q){
                    w += (((a, l.go), z))
                }
            }
        }
        closure(w.toSet)
    }

    def makeGraph(log_file: String): YACCItem = {
        var flag = true
        val endt = new Terminal("$")
        var init: Vertex = closure(Set(((new NonTerminal("S'"), new Location(new SymbolArray(Array(new NonTerminal("S"), endt)), 0)), null)))
        var sete: mutable.Set[/*Edge*/(Int, Symbol, Int)] = mutable.Set()
        var map: mutable.Map[Vertex, Int] = mutable.Map({init -> 0})
        var index = 1
        var log: PrintWriter = null
        var acs: List[(Int, Int)] = List()

        while(flag){
            flag = false
            for(seti <- map.toArray.map(_._1)){
                for(prod <- seti){
                    val ((a, l), z) = prod
                    if(! l.isBottom){
                        val x = l.next
                        if(x == endt){
                            acs = ((map(seti), terminal_imap.keyA(endt))) :: acs
                        }else{
                            val setj = goto(seti, x)
                            var es = map(seti)
                            var eg = 0

                            if(map.contains(setj)){
                                eg = map(setj)
                            }else{
                                map(setj) = index
                                eg = index
                                index += 1
                                flag = true
                            }

                            val edge = (es, x, eg)

                            if(! sete.contains(edge)){
                                sete += edge
                                flag = true
                            }
                        }
                    }
                }
            }
        }

        val vertex_n: Int = map.size

        val accept_matrix: Array[Array[Boolean]] = {
            var mat: Array[Array[Boolean]] = Array.ofDim[Boolean](vertex_n, terminal_n)
            for(i <- 0 until vertex_n){
                for(j <- 0 until terminal_n){
                    mat(i)(j) = false
                }
            }
            for((x, y) <- acs){
                mat(x)(y) = true
            }
            mat
        }

        val shift_matrix: Array[Array[Int]] = {
            var mat: Array[Array[Int]] = Array.ofDim[Int](vertex_n, terminal_n)
            for(i <- 0 until vertex_n){
                for(j <- 0 until terminal_n){
                    mat(i)(j) = -1
                }
            }
            for((s, sym, g) <- sete){
                sym match{
                    case t: Terminal => mat(s)(terminal_imap.keyA(t)) = g
                    case _ => ;
                }
            }
            mat
        }
        val reduce_m: Array[Array[mutable.ArrayBuffer[Int]]] = {
            var mat: Array[Array[mutable.ArrayBuffer[Int]]] = Array.ofDim[mutable.ArrayBuffer[Int]](vertex_n, terminal_n)
            for((v, i) <- map){
                for(((a, l), z) <- v){
                    if(l.isBottom){
                        val r: Int = production_imap.keyA((a, l.array))
                        val j: Int = terminal_imap.keyA(z)
                        if(mat(i)(j) != null){
                            mat(i)(j) += r
                        }else{
                            mat(i)(j) = mutable.ArrayBuffer[Int](r)
                        }
                    }
                }
            }
            mat
        }
        val reduce_matrix: Array[Array[Int]] = {
            var a = Array.ofDim[Int](vertex_n, terminal_n)
            for(i <- 0 until vertex_n){
                for(j <- 0 until terminal_n){
                    if(shift_matrix(i)(j) != -1){
                        a(i)(j) = -1
                    }else if(reduce_m(i)(j) == null){
                        a(i)(j) = -1
                    }else{
                        a(i)(j) = reduce_m(i)(j)(0)
                    }
                }
            }
            a
        }
        val goto_matrix: Array[Array[Int]] = {
            var mat: Array[Array[Int]] = Array.ofDim[Int](vertex_n, non_terminal_n)
            for(i <- 0 until vertex_n){
                for(j <- 0 until non_terminal_n){
                    mat(i)(j) = -1
                }
            }
            for((s, sym, g) <- sete){
                sym match{
                    case n: NonTerminal => mat(s)(non_terminal_imap.keyA(n)) = g
                    case _ => ;
                }
            }
            mat
        }
        /*
        System.err.println(shift_matrix.map(_.mkString("[", ",", "]")).mkString("[", ",", "]"))
        System.err.println(goto_matrix.map(_.mkString("[", ",", "]")).mkString("[", ",", "]"))
        */

        new YACCItem(shift_matrix, reduce_matrix, goto_matrix, accept_matrix)
    }
}

object YACC{
    type Production = (NonTerminal, SymbolArray)

    def gatherTerminalSet(ps: Array[Production]): Set[Terminal] = {
        var s: mutable.Set[Terminal] = mutable.Set()
        for(p <- ps){
            for(x <- p._2.array){
                x match{
                    case t: Terminal => s += t
                    case _ => ;
                }
            }
        }
        s.toSet
    }

    def gatherNonTerminalSet(ps: Array[Production]): Set[NonTerminal] = {
        var s: mutable.Set[NonTerminal] = mutable.Set()
        for(p <- ps){
            for(x <- p._2.array){
                x match{
                    case t: NonTerminal => s += t
                    case _ => ;
                }
            }
        }
        s.toSet
    }

    def makeNullableSet(ps: Array[Production]): Set[NonTerminal] = {
        var s: mutable.Set[NonTerminal] = mutable.Set()
        var flag = true
        while(flag){
            flag = false
            for(p <- ps){
                val (k, xs) = p
                if(! (s contains k) && xs.forall(x =>
                    x match{
                        case n: NonTerminal => s contains n
                        case _ => false
                    }
                )){
                    s += k
                    flag = true
                }
            }
        }

        s.toSet
    }

    def makeFirstSets(ps: Array[Production], nullable_set: Set[NonTerminal], terminal_set: Set[Terminal]): Map[Symbol, Set[Terminal]] = {
        var m_map: mutable.Map[Symbol, Set[Terminal]] = mutable.Map()
        var flag = true

        for(t <- terminal_set){
            m_map(t) = Set(t)
        }

        while(flag){
            flag = false
            for((x, xs) <- ps){
                if(! m_map.contains(x)){
                    m_map(x) = Set()
                }
                if(xs.length > 0){
                    if(m_map.contains(xs(0))){
                        val a: Set[Terminal] = m_map(x) | m_map(xs(0))
                        if(m_map(x) != a){
                            m_map(x) = a
                            flag = true
                        }
                    }

                    var i = 0
                    var fl = true
                    var ab: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer()
                    while(i < xs.length && fl){
                        fl = xs(i) match{
                            case n: NonTerminal => nullable_set.contains(n)
                            case _ => false
                        }
                        if(fl){
                            ab += i
                            i += 1
                        }
                    }

                    for(j <- ab){
                        if(j < xs.length - 1){
                            if(m_map.contains(xs(j + 1))){
                                val a = m_map(x) | m_map(xs(j + 1))
                                if(m_map(x) != a){
                                    m_map(x) = a
                                    flag = true
                                }
                            }
                        }
                    }
                }
            }
        }

        m_map.toMap
    }

    def makeTree(s: String): YTree = {
        def sub(ls: List[String]): (YTree, List[String]) = {
            ls match{
                case "(" :: xs => {
                    var zs = mutable.ArrayBuffer[YTree]()
                    var ys = xs
                    while(ys(0) != ")"){
                        val (t, nys) = sub(ys)
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

        val k = sub("(" :: s.replace("(", " ( ").replace(")", " ) ").split("\\s+").toList ++ List(")"))
        k._1
    }
}

package yacc

import java.io.PrintWriter
import scala.collection.mutable

class YACC(val productions: Array[(NonTerminal, SymbolArray)]){
    type Production = (NonTerminal, SymbolArray)
    type Term = ((NonTerminal, Location), Terminal)
    type Vertex = Set[Term]
    // type Edge = (Vertex, Symbol, Vertex)

    val nullable_set: Set[NonTerminal] = YACC.makeNullableSet(productions)
    val terminal_set: Set[Terminal] = YACC.gatherTerminalSet(productions)
    val non_terminal_set: Set[NonTerminal] = YACC.gatherNonTerminalSet(productions)
    val first_map: Map[Symbol, Set[Terminal]] = YACC.makeFirstSets(productions, nullable_set, terminal_set)

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

    def makeGraph(log_file: String){
        var flag = true
        var init: Vertex = closure(Set(((new NonTerminal("S'"), new Location(new SymbolArray(Array(new NonTerminal("S"), new Terminal("$"))), 0)), null)))
        val endt = new Terminal("$")
        var sete: mutable.Set[/*Edge*/(Int, Symbol, Int)] = mutable.Set()
        var map: mutable.Map[Vertex, Int] = mutable.Map({init -> 0})
        var index = 1
        var log: PrintWriter = null

        while(flag){
            flag = false
            for(seti <- map.toArray.map(_._1)){
                for(prod <- seti){
                    val ((a, l), z) = prod
                    if(! l.isBottom){
                        val x = l.next
                        if(x == endt){
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
}

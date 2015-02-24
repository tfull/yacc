package yacc

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class YTree{
    override def toString(): String = "Expression"
}

class YNode(val name: String, val trees: Array[YTree]) extends YTree{
    override def toString(): String = "YNode{" + name + "}" + trees.mkString("(", "," , ")")
}

class YLeaf(val number: Int) extends YTree{
    override def toString(): String = "YLeaf(" + number.toString + ")"
}

class Symbol{
    override def toString(): String = "Symbol"
    def <(x: Symbol) = false
    def ==(x: Symbol) = true
    override def hashCode = 0
    override def equals(x: Any) = x match{
        case s: Symbol => true
        case _ => false
    }
    def >(x: Symbol) = false
}

class NonTerminal(val key: String) extends Symbol{
    override def toString(): String = "N(" + key + ")"
    override def hashCode = 1
    override def equals(x: Any) = x match{
        case t: NonTerminal => this.key == t.key
        case _ => false
    }
    def <(x: NonTerminal) = this.key < x.key
    def <(x: Terminal) = true
    def >(x: NonTerminal) = this.key > x.key
    def >(x: Terminal) = false
    def ==(x: NonTerminal) = this.key == x.key
    def ==(x: Terminal) = false
}

class Terminal(val key: String) extends Symbol{
    override def toString(): String = "T(" + key + ")"
    override def hashCode = 2
    override def equals(x: Any) = x match{
        case t: Terminal => this.key == t.key
        case _ => false
    }
    def <(x: NonTerminal) = false
    def <(x: Terminal) = this.key < x.key
    def >(x: NonTerminal) = true
    def >(x: Terminal) = this.key > x.key
    def ==(x: NonTerminal) = false
    def ==(x: Terminal) = this.key == x.key
}

class YACC(val productions: Array[(NonTerminal, Array[Symbol])]){
    type Production = (NonTerminal, Array[Symbol])
    type Location = (Array[Symbol], Array[Symbol])
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
            System.err.println(Parser.show(ver))
            flag = false
            for(term <- ver.toArray){
                val ((a, (alpha, xbeta)), z) = term
                if(xbeta.length > 0){
                    var x = xbeta(0)
                    x match{
                        case n: NonTerminal => {
                            var beta = xbeta.slice(1, xbeta.length)
                            for(prod <- this.productions){
                                if(prod._1 == a){
                                    for(w <- first(beta ++ Array(z))){
                                        var tm: Term = (((n, (Array(), prod._2)), w))
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
            var ((a, (alpha, xbeta)), z) = term
            if(xbeta.length > 0){
                var x = xbeta(0)
                if(x == q){
                    var beta = xbeta.slice(1, xbeta.length)
                    w += (((a, (alpha ++ Array(x), beta)), z))
                }
            }
        }
        closure(w.toSet)
    }

    def makeGraph(){
        var flag = true
        var init: Vertex = closure(Set(((new NonTerminal("S'"), (Array(), Array(new NonTerminal("S"), new Terminal("$")))), null)))
        // var sett: mutable.Set[Vertex] = Set(init)
        var sete: mutable.Set[/*Edge*/(Int, Symbol, Int)] = mutable.Set()
        var map: mutable.Map[Vertex, Int] = mutable.Map({init -> 0})
        var index = 1

        while(flag){
            flag = false
            for(seti <- map.toArray.map(_._1)){
                for(prod <- seti){
                    val ((a, (alpha, xbeta)), z) = prod
                    if(xbeta.length > 0){
                        val x = xbeta(0)
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
}

object YACC{
    type Production = (NonTerminal, Array[Symbol])

    def gatherTerminalSet(ps: Array[Production]): Set[Terminal] = {
        var s: mutable.Set[Terminal] = mutable.Set()
        for(p <- ps){
            for(x <- p._2){
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
            for(x <- p._2){
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
                    var ab: ArrayBuffer[Int] = ArrayBuffer()
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

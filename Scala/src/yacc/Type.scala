package yacc

import scala.collection.mutable

class SymbolArray(val array: Array[Symbol]){
    val length: Int = array.length
    def apply(index: Int) = array(index)
    def forall = array.forall(_)
    override def toString(): String = array.mkString("[", ",", "]")
    override def hashCode(): Int = array.length * 10000
    override def equals(o: Any) = o match{
        case x: SymbolArray => this.array.sameElements(x.array)
        case _ => false
    }
}

class Location(val array: SymbolArray, val index: Int){
    val length: Int = array.length
    if(index < 0 || index > length){
        throw new Exception()
    }

    override def toString(): String = "[" + array.array.slice(0, index).mkString(" ") + " . " + array.array.slice(index, length).mkString(" ") + "]"
    override def hashCode = 10000 + array.length * 1000 + index * 100
    override def equals(x: Any) = x match{
        case l: Location => (array equals l.array) && index == l.index
        case _ => false
    }
    def isTop(): Boolean = index == 0
    def isBottom(): Boolean = index == length
    def next(): Symbol = array(index)
    def following(): Array[Symbol] = array.array.slice(index + 1, length)
    def go(): Location = new Location(array, index + 1)
}

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
    override def hashCode = 0
    override def equals(x: Any) = x match{
        case s: Symbol => s.isInstanceOf[Symbol]
        case _ => false
    }
}

class NonTerminal(val key: String) extends Symbol{
    override def toString(): String = "N(" + key + ")"
    override def hashCode = 10000 + key.length * 100
    override def equals(x: Any) = x match{
        case t: NonTerminal => this.key == t.key
        case _ => false
    }
}

class Terminal(val key: String) extends Symbol{
    override def toString(): String = "T(" + key + ")"
    override def hashCode = 20000 + key.length * 100
    override def equals(x: Any) = x match{
        case t: Terminal => this.key == t.key
        case _ => false
    }
}

class InteractiveMap[A, B](val map_a: Map[A, B]){
    val map_b = InteractiveMap.inverseMap(map_a)

    def keyA(a: A): B = {
        map_a(a)
    }

    def keyB(b: B): A = {
        map_b(b)
    }
}

object InteractiveMap{
    def inverseMap[A, B](map_a: Map[A, B]): Map[B, A] = {
        var map: mutable.Map[B, A] = mutable.Map()
        for((key, value) <- map_a.toArray){
            map += { value -> key }
        }
        map.toMap
    }
}

package yacc

import java.io.PrintWriter
import java.util.Scanner
import scala.io.Source
import scala.collection.mutable

object Clang{
    def main(args: Array[String]){
        type Production = (NonTerminal, SymbolArray)

        val (file_i, file_o, file_e): (String, String, String) =  Argument.parse(args)
        val parser: Parser = Parser.readFile(file_i)
        val yacc: YACC = new YACC(parser.rules.map(_._1), parser.terminal_map, parser.non_terminal_set)
        val writer: PrintWriter = new PrintWriter(file_o)
        val yacc_item = yacc.makeGraph(file_e)
        val tf_class: String = "TFParser"
        val tf_method: String = "tfParser"
        val tf_value: String = "tf_parser"
        val indent: String = "\t"

        for(module <- parser.modules){
            writer.println("#include " + module)
        }
        writer.println("#include <stdlib.h>")
        writer.println("#include <stdio.h>")
        writer.println()

        writer.println("typedef union{")
        writer.println("%s token;".format(indent + parser.token_class))
        writer.println("%s *tree;".format(indent + parser.tree_class))
        writer.println("}%s;\n".format(tf_class + "Item"))

        writer.println("typedef struct{")
        writer.println(indent + "int state;")
        writer.println("%s item;".format(indent + tf_class + "Item"))
        writer.println("}%s;\n".format(tf_class + "StackItem"))

        writer.println("typedef struct{")
        writer.println(indent + "unsigned int size;")
        writer.println(indent + "unsigned int capacity;")
        writer.println("%s *data;".format(indent + tf_class + "StackItem"))
        writer.println("}%s;\n".format(tf_class + "Stack"))

        writer.println("int %s_accept[%d][%d] = {".format(tf_value, yacc_item.accept.size, yacc_item.accept(0).size))
        for(i <- 0 until yacc_item.accept.size){
            val a = yacc_item.accept(i)
            if(i == yacc_item.accept.size - 1){
                writer.println(a.map(if(_){ 1 }else{ 0 }).mkString(indent + "{ ", ", ", " }"))
            }else{
                writer.println(a.map(if(_){ 1 }else{ 0 }).mkString(indent + "{ ", ", ", " },"))
            }
        }
        writer.println("};\n")

        writer.println("int %s_shift[%d][%d] = {".format(tf_value, yacc_item.shift.size, yacc_item.shift(0).size))
        for(i <- 0 until yacc_item.shift.size){
            val a = yacc_item.shift(i)
            if(i == yacc_item.shift.size - 1){
                writer.println(a.mkString(indent + "{ ", ", ", " }"))
            }else{
                writer.println(a.mkString(indent + "{ ", ", ", " },"))
            }
        }
        writer.println("};\n")

        writer.println("int %s_reduce[%d][%d] = {".format(tf_value, yacc_item.reduce.size, yacc_item.reduce(0).size))
        for(i <- 0 until yacc_item.reduce.size){
            val a = yacc_item.reduce(i)
            if(i == yacc_item.reduce.size - 1){
                writer.println(a.mkString(indent + "{ ", ", ", " }"))
            }else{
                writer.println(a.mkString(indent + "{ ", ", ", " },"))
            }
        }
        writer.println("};\n")

        writer.println("int %s_goto[%d][%d] = {".format(tf_value, yacc_item.goto.size, yacc_item.goto(0).size))
        for(i <- 0 until yacc_item.goto.size){
            val a = yacc_item.goto(i)
            if(i == yacc_item.goto.size - 1){
                writer.println(a.mkString(indent + "{ " , ", ", " }"))
            }else{
                writer.println(a.mkString(indent + "{ ", ", ", " },"))
            }
        }
        writer.println("};\n")

        def makeReduceFunction(prod: Production, yt: YTree){

            def getIndex(vt: VTree): Int = {
                vt match{
                    case vn: VNode => vn.index
                    case vl: VLeaf => vl.index
                }
            }

            def isValue(yt: YTree): Boolean = {
                yt match{
                    case yn: YNode => false
                    case yl: YLeaf => {
                        prod._2.array(yl.number - 1) match{
                            case n: NonTerminal => false
                            case t: Terminal => true
                        }
                    }
                }
            }

            def sub(yt: YTree, vt: VTree){
                (yt, vt) match{
                    case (yn: YNode, vn: VNode) => {
                        val size: Int = yn.trees.size
                        if(size == 0){
                            writer.println(indent + "%s s%d_unit;".format(yn.name, vn.index))
                            writer.println(indent + "s%d.%s = s%d_unit;".format(vn.index, parser.tree_value_map(yn.name), vn.index))
                            writer.println(indent + "t%d = (%s*)malloc(sizeof(%s));".format(vn.index, parser.tree_class, parser.tree_class))
                            writer.println(indent + "t%d->%s = %s;".format(vn.index, parser.tree_type._2, parser.tree_map(yn.name)))
                            writer.println(indent + "t%d->%s = s%d;".format(vn.index, parser.tree_value._2, vn.index))
                        }else if(size == 1){
                            if(isValue(yn.trees(0))){
                                (yn.trees(0), vn.trees(0)) match{
                                    case (yl: YLeaf, vl: VLeaf) => {
                                        prod._2.array(yl.number - 1) match{
                                            case t: Terminal => {
                                                writer.println(indent + "%s v;".format(yn.name))
                                                writer.println(indent + "v.value = items[%d].token.%s.%s;".format(yl.number - 1, parser.token_value._2, parser.token_value_map(t)))
                                                writer.println(indent + "t%d = (%s*)malloc(sizeof(%s));".format(vn.index, parser.tree_class, parser.tree_class))
                                                writer.println(indent + "s%d.%s = v;".format(vn.index, parser.tree_value_map(yn.name)))
                                                writer.println(indent + "t%d->%s = %s;".format(vn.index, parser.tree_type._2, parser.tree_map(yn.name)))
                                                writer.println(indent + "t%d->%s = s%d;".format(vn.index, parser.tree_value._2, vn.index))
                                            }
                                        }
                                    }
                                }
                            }else{
                                sub(yn.trees(0), vn.trees(0))
                                writer.println(indent + "%s s%d_single;".format(yn.name, vn.index))
                                writer.println(indent + "s%d_single.tree = t%d;".format(vn.index, getIndex(vn.trees(0))))
                                writer.println(indent + "s%d.%s = s%d_single;".format(vn.index, parser.tree_value_map(yn.name), vn.index))
                                writer.println(indent + "t%d = (%s*)malloc(sizeof(%s));".format(vn.index, parser.tree_class, parser.tree_class))
                                writer.println(indent + "t%d->%s = %s;".format(vn.index, parser.tree_type._2, parser.tree_map(yn.name)))
                                writer.println(indent + "t%d->%s = s%d;".format(vn.index, parser.tree_value._2, vn.index))
                            }
                        }else{
                            writer.println(indent + "%s s%d_tuple;".format(yn.name, vn.index))
                            for(i <- 0 until size){
                                sub(yn.trees(i), vn.trees(i))
                                writer.println(indent + "s%d_tuple.tree%d = t%d;".format(vn.index, i + 1, getIndex(vn.trees(i))))

                            }
                            writer.println(indent + "t%d = (%s*)malloc(sizeof(%s));".format(vn.index, parser.tree_class, parser.tree_class))
                            writer.println(indent + "t%d->%s = %s;".format(vn.index, parser.tree_type._2, parser.tree_map(yn.name)))
                            writer.println(indent + "t%d->%s.%s = s%d_tuple;".format(vn.index, parser.tree_value._2, parser.tree_value_map(yn.name), vn.index))
                        }
                    }
                    case (yl: YLeaf, vl: VLeaf) => {
                        prod._2.array(yl.number - 1) match{
                            case n: NonTerminal => {
                                writer.println(indent + "t%d = items[%d].tree;".format(vl.index, yl.number - 1))
                            }
                        }
                    }
                }
            }

            val vt_i = makeVTree(yt, 0)
            val vt: VTree = vt_i._1
            val max_index: Int = vt_i._2 - 1

            for(i <- 0 to max_index){
                writer.println(indent + "%s *t%d;".format(parser.tree_class, i))
                writer.println(indent + "%s s%d;".format(parser.tree_value._1, i))
            }

            sub(yt, vt)

            writer.println(indent + "return t0;")
        }

        for((p, y) <- parser.rules){
            val i: Int = yacc.production_imap.keyA(p)
            writer.println("%s *%sReduce%d(%sItem *items){".format(parser.tree_class, tf_method, i, tf_class))
            makeReduceFunction(p, y)
            writer.println("}")
        }
        writer.println()

        writer.println("int %s_reduce_numbers[%d] = {%s};\n".format(tf_value, parser.rules.size, (0 until parser.rules.size).map(yacc.production_imap.keyB(_)._2.array.size).mkString(",")))
        writer.println("%s* (*%s_reduce_functions[%d])(%sItem*) = {\n%s\n};\n".format(parser.tree_class, tf_value, parser.rules.size, tf_class, (0 until parser.rules.size).map(indent + tf_method + "Reduce" + _.toString).mkString(",\n")))

        writer.println("int %s_goto_numbers[%d] = {%s};\n".format(tf_value, parser.rules.size, (0 until parser.rules.size).map({ x => yacc.non_terminal_imap.keyA(yacc.production_imap.keyB(x)._1)}).mkString(",")))

        writer.println("%sStack %sStack_allocate(){".format(tf_class, tf_class))
        writer.println("%sStack stack;".format(indent + tf_class))
        writer.println("%sstack.size = 0U;".format(indent))
        writer.println("%sstack.capacity = 64U;".format(indent))
        writer.println("%sstack.data = (%sStackItem*)malloc(sizeof(%sStackItem) * 64U);".format(indent, tf_class, tf_class))
        writer.println(indent + "return stack;")
        writer.println("}\n")

        writer.println("void %sStack_push(%sStack *stack, %sStackItem item){".format(tf_class, tf_class, tf_class))
        writer.println("%sif(stack->size >= stack->capacity){".format(indent))
        writer.println("%sunsigned int i;".format(indent * 2))
        writer.println("%sunsigned int ncap = stack->capacity * 2U;".format(indent * 2))
        writer.println("%sStackItem *ndata = (%sStackItem*)malloc(sizeof(%sStackItem) * ncap);".format(indent * 2 + tf_class, tf_class, tf_class))
        writer.println("%sfor(i = 0U; i < stack->size; i++){".format(indent * 2))
        writer.println("%sndata[i] = stack->data[i];".format(indent * 3))
        writer.println("%s}".format(indent * 2))
        writer.println("%sstack->capacity = ncap;".format(indent * 2))
        writer.println("%sfree(stack->data);".format(indent * 2))
        writer.println("%sstack->data = ndata;".format(indent * 2))
        writer.println("%s}".format(indent))
        writer.println("%sstack->data[stack->size] = item;".format(indent))
        writer.println("%sstack->size ++;".format(indent))
        writer.println("}\n")

        writer.println("%sStackItem %sStack_pop(%sStack *stack){".format(tf_class, tf_class, tf_class))
        writer.println("%sStackItem item = stack->data[stack->size - 1U];".format(indent + tf_class))
        writer.println("%sstack->size --;".format(indent))
        writer.println("%sreturn item;".format(indent))
        writer.println("}\n")

        writer.println("void %sStack_free(%sStack *stack){".format(tf_class, tf_class))
        writer.println("%sfree(stack->data);".format(indent))
        writer.println("}\n")

        writer.println("void %sInitStack(%sStack *stack){".format(tf_method, tf_class))
        writer.println("%sStackItem item = { 0 };".format(indent + tf_class))
        writer.println("%sStack_push(stack, item);".format(indent + tf_class))
        writer.println("}\n")

        writer.println("int %sGetNowState(%sStack *stack){".format(tf_method, tf_class))
        writer.println(indent + "return stack->data[stack->size - 1U].state;")
        writer.println("}\n")

        writer.println("int %sGetTerminalID(%s t){".format(tf_method, parser.token_class))
        if(true){
            var flag: Boolean = true
            for(t <- parser.terminal_map.keys){
                val i: Int = yacc.terminal_imap.keyA(t)
                val s: String = parser.terminal_map(t)
                if(flag){
                    writer.println("%sif(t.%s == %s){".format(indent, parser.token_type._2, s))
                    writer.println("%sreturn %d;".format(indent * 2, i))
                    flag = false
                }else{
                    writer.println("%s}else if(t.%s == %s){".format(indent, parser.token_type._2, s))
                    writer.println("%sreturn %d;".format(indent * 2, i))
                }
            }
            writer.println(indent + "}else{")
            writer.println(indent * 2 + "return -1;")
            writer.println(indent + "}")
        }
        writer.println("}\n")

        writer.println("void %sRaiseParseError(%s token){".format(tf_method, parser.token_class))
        writer.println("%sfprintf(stderr, \"parse error: token(%%u:%%u - %%u:%%u)\\n\", token.%s, token.%s, token.%s, token.%s);".format(indent, parser.token_position(0), parser.token_position(1), parser.token_position(2), parser.token_position(3)))
        writer.println("}\n")

        writer.println("%s *parse(%s *tokens){".format(parser.tree_class, parser.token_class))
        writer.println("%sStack stack = %sStack_allocate();".format(indent + tf_class, tf_class))
        writer.println("%sInitStack(&stack);".format(indent + tf_method))
        writer.println("%sint state;".format(indent))
        writer.println("%swhile(1){".format(indent))
        writer.println("%sint tid = %sGetTerminalID(*tokens);".format(indent * 2, tf_method))
        writer.println("%sstate = %sGetNowState(&stack);".format(indent * 2, tf_method))
        writer.println("%sif(%s_accept[state][tid]){".format(indent * 2, tf_value))
        writer.println("%sStackItem s_item = %sStack_pop(&stack);".format(indent * 3 + tf_class, tf_class))
        writer.println("%sStack_free(&stack);".format(indent * 3 + tf_class))
        writer.println("%sreturn s_item.item.tree;".format(indent * 3))
        writer.println("%s}else if(%s_shift[state][tid] != -1){".format(indent * 2, tf_value))
        writer.println("%sItem item;".format(indent * 3 + tf_class))
        writer.println("%sStackItem s_item;".format(indent * 3 + tf_class))
        writer.println("%sitem.token = *tokens;".format(indent * 3))
        writer.println("%ss_item.state = %s_shift[state][tid];".format(indent * 3, tf_value))
        writer.println("%ss_item.item = item;".format(indent * 3));
        writer.println("%sStack_push(&stack, s_item);".format(indent * 3 + tf_class))
        writer.println("%stokens ++;".format(indent * 3))
        writer.println("%s}else if(%s_reduce[state][tid] != -1){".format(indent * 2, tf_value))
        writer.println("%sItem items[%d];".format(indent * 3 + tf_class, parser.rules.map(_._1._2.array.size).max))
        writer.println("%s *tree;".format(indent * 3 + parser.tree_class))
        writer.println("%sItem item;".format(indent * 3 + tf_class))
        writer.println("%sStackItem s_item;".format(indent * 3 + tf_class))
        writer.println("%sint s_i;".format(indent * 3))
        writer.println("%sint r_n = %s_reduce[state][tid];".format(indent * 3, tf_value))
        writer.println("%sfor(s_i = %s_reduce_numbers[r_n] - 1; s_i >= 0; s_i--){".format(indent * 3, tf_value))
        writer.println("%sStackItem s_tmp = %sStack_pop(&stack);".format(indent * 4 + tf_class, tf_class))
        writer.println("%sitems[s_i] = s_tmp.item;".format(indent * 4))
        writer.println("%s}".format(indent * 3))
        writer.println("%sitem.tree = %s_reduce_functions[r_n](items);".format(indent * 3, tf_value))
        writer.println("%ss_item.state = %s_goto[%sGetNowState(&stack)][%s_goto_numbers[r_n]];".format(indent * 3, tf_value, tf_method, tf_value))
        writer.println("%ss_item.item = item;".format(indent * 3))
        writer.println("%sStack_push(&stack, s_item);".format(indent * 3 + tf_class))
        writer.println("%s}else{".format(indent * 2))
        writer.println("%sRaiseParseError(*tokens);".format(indent * 3 + tf_method))
        writer.println("%sreturn NULL;".format(indent * 3))
        writer.println("%s}".format(indent * 2))
        writer.println("%s}".format(indent))
        writer.println("}")

        writer.close
    }

    def makeVTree(yt: YTree, i: Int): (VTree, Int) = {
        yt match{
            case yn: YNode => {
                var index = i + 1
                val vs = new Array[VTree](yn.trees.size)
                for(j <- 0 until yn.trees.size){
                    val k = makeVTree(yn.trees(j), index)
                    vs(j) = k._1
                    index = k._2
                }
                (new VNode(i, vs), index)
            }
            case yl: YLeaf => {
                (new VLeaf(i), i + 1)
            }
        }
    }

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

                            val ytree = YACC.makeTree(array(2))

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

        
    }
}

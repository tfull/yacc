package yacc

import java.io.PrintWriter

object Clang{
    def main(args: Array[String]){
        val (file_i, file_o, file_e): (String, String, String) =  Argument.parse(args)
        val parser: Parser = Parser.readFile(file_i)
        val yacc: YACC = new YACC(parser.rules, parser.terminal_map, parser.non_terminal_set)
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

        writer.println("int %s_shift[%d][%d] = {".format(tf_value, yacc_item.shift.size, yacc_item.shift(0).size))
        for(i <- 0 until yacc_item.shift.size){
            var a = yacc_item.shift(i)
            if(i == yacc_item.shift.size - 1){
                writer.println(a.mkString(indent + "{ ", ", ", " }"))
            }else{
                writer.println(a.mkString(indent + "{ ", ", ", " },"))
            }
        }
        writer.println("};\n")

        writer.println("int %s_reduce[%d][%d] = {".format(tf_value, yacc_item.reduce.size, yacc_item.reduce(0).size))
        for(i <- 0 until yacc_item.reduce.size){
            var a = yacc_item.reduce(i)
            if(i == yacc_item.reduce.size - 1){
                writer.println(a.mkString(indent + "{ ", ", ", " }"))
            }else{
                writer.println(a.mkString(indent + "{ ", ", ", " },"))
            }
        }
        writer.println("};\n")

        writer.println("int %s_goto[%d][%d] = {".format(tf_value, yacc_item.goto.size, yacc_item.goto(0).size))
        for(i <- 0 until yacc_item.goto.size){
            var a = yacc_item.goto(i)
            if(i == yacc_item.goto.size - 1){
                writer.println(a.mkString(indent + "{ " , ", ", " }"))
            }else{
                writer.println(a.mkString(indent + "{ ", ", ", " },"))
            }
        }
        writer.println("};\n")

        writer.println("%sStack %sAllocateStack(){".format(tf_class, tf_method))
        writer.println("%sStack %s_stack;".format(indent + tf_class, tf_value))
        writer.println("%s_stack.size = 0U;".format(indent + tf_value))
        writer.println("%s_stack.capacity = 64U;".format(indent + tf_value))
        writer.println("%s_stack.data = (%s*)malloc(sizeof(%s) * 64U);".format(indent + tf_value, tf_class + "StackItem", tf_class + "StackItem"))
        writer.println(indent + "return %s_stack;".format(tf_value))
        writer.println("}\n")

        writer.println("void %sPushItem(%sStack *%s_stack, %sStackItem %s_item){".format(tf_method, tf_class, tf_value, tf_class, tf_value))
        writer.println("%sif(%s_stack->size >= %s_stack->capacity){".format(indent, tf_value, tf_value))
        writer.println("%sunsigned int i;".format(indent * 2))
        writer.println("%sunsigned int ncap = %s_stack->capacity * 2U;".format(indent * 2, tf_value))
        writer.println("%sStackItem *ndata = (%sStackItem*)malloc(sizeof(%sStackItem) * ncap);".format(indent * 2 + tf_class, tf_class, tf_class))
        writer.println("%sfor(i = 0U; i < %s_stack->size; i++){".format(indent * 2, tf_value))
        writer.println("%sndata[i] = %s_stack->data[i];".format(indent * 3, tf_value))
        writer.println("%s}".format(indent * 2))
        writer.println("%s_stack->capacity = ncap;".format(indent * 2 + tf_value))
        writer.println("%sfree(%s_stack->data);".format(indent * 2, tf_value))
        writer.println("%s_stack->data = ndata;".format(indent * 2 + tf_value))
        writer.println("%s}".format(indent))
        writer.println("%s_stack->data[size] = %s_item;".format(indent + tf_value, tf_value))
        writer.println("%s_stack->size ++;".format(indent + tf_value))
        writer.println("}\n")

        writer.println("void %sInitStack(%sStack *%s_stack)".format(tf_method, tf_class, tf_value))
        writer.println("%sStackItem %s_item = { 0 };".format(indent + tf_class, tf_value))
        writer.println("%sStack_Push(%s_stack, %s_item);".format(indent + tf_method, tf_value, tf_value))
        writer.println("}\n")

        writer.println("%s *parse(%s *tokens){".format(parser.tree_class, parser.token_class))
        writer.println("%sStack %s_stack = %sAllocateStack();".format(indent + tf_class, tf_value, tf_method))
        writer.println("%sInitStack(%s_stack);".format(indent + tf_method, tf_value))
        writer.println("}\n")

        writer.close
    }
}

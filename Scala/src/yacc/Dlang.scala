package yacc

import java.io.PrintWriter

object Dlang{
    def main(args: Array[String]){
        val (file_i, file_o, file_e): (String, String, String) =  Argument.parse(args)
        val parser: Parser = Parser.readFile(file_i)
        val writer: PrintWriter = new PrintWriter(file_o)

        for(module <- parser.modules){
            writer.println("import " + module)
        }

        writer.println("")

        writer.println(parser.syntax_tree + " parse(" + parser.token + "[] tokens){")
        writer.println("}")

        writer.close

//        val yacc = new YACC(parser.rules.map(_._1))
//        yacc.makeGraph(file_e)
    }
}

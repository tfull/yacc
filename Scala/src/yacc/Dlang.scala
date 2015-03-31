package yacc

import java.io.PrintWriter

object Dlang{
    def main(args: Array[String]){
        val (file_i, file_o, file_e): (String, String, String) =  Argument.parse(args)
        val parser: Parser = Parser.readFile(file_i)
        val yacc: YACC = new YACC(parser.rules, parser.terminal_map, parser.non_terminal_set)
        val writer: PrintWriter = new PrintWriter(file_o)
        //yacc.makeGraph(file_e)

        for(module <- parser.modules){
            writer.println("import " + module)
        }

        writer.close
    }
}

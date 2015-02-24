package yacc

object Dlang{
    def main(args: Array[String]){
        val parser = Parser.readFile(args(0))
        val yacc = new YACC(parser.rules.map(_._1))
        yacc.makeGraph()
        /*
        for(module <- parser.modules){
            println("import " + module + ";")
            println("")
        }
        */
    }
}

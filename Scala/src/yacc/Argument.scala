package yacc

class ArgumentException(message: String = null, cause: Throwable = null) extends Exception(message, cause)

object Argument{
    def parse(args: Array[String]): (String, String, String) = {
        var i: Int = 0
        val length: Int = args.length
        var fi: String = null
        var fo: String = null
        var fe: String = null

        while(i < length){
            val arg = args(i)
            if(arg.length == 0){
                throw new ArgumentException("arg.length == 0")
            }else if(arg(0) == '-'){
                arg match{
                    case "-i" => {
                        i += 1
                        if(i < length){
                            fi = args(i)
                        }else{
                            throw new ArgumentException("no -i file")
                        }
                    }
                    case "-o" => {
                        i += 1
                        if(i < length){
                            fo = args(i)
                        }else{
                            throw new ArgumentException("no -o file")
                        }
                    }
                    case "-e" => {
                        i += 1
                        if(i < length){
                            fe = args(i)
                        }else{
                            throw new ArgumentException("no -e file")
                        }
                    }
                    case _ => {
                        throw new ArgumentException("no such option")
                    }
                }
            }else{
                throw new ArgumentException("no option")
            }

            i += 1
        }

        if(fi == null){
            throw new ArgumentException("no input file")
        }
        if(fo == null){
            throw new ArgumentException("no output file")
        }

        (fi, fo, fe)
    }
}

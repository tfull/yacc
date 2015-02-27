import Type;
import std.array;
import std.conv: to;

enum AutomatonMode{
    PLAIN, INT
}

alias AutomatonMode Am;

class TokenizationError: Exception{
    this(string message, string file = __FILE__, int line = __LINE__){
        super(message, file, line);
    }
}

alias TokenizationError LexError;

string makeMessage(string mode, int line, int character){
    return mode ~ ": line = " ~ to!(string)(line) ~ ", character = " ~ to!(string)(character);
}


Token[] tokenize(string s){
    Am mode = Am.PLAIN;
    int line = 1;
    int character = 0;
    Token[] tokens;

    char[] store;
    int s_line;
    int s_character;

    for(int i = 0; i < s.length; i++){
        char c = s[i];

        if(c == '\n'){
            line += 1;
            character = 0;
        }else{
            character += 1;
        }

        if(mode == Am.PLAIN){
            if(c >= '0' && c <= '9'){
                s_line = line;
                s_line = character;
                mode = Am.INT;
                store ~= c;
            }else if(c == '('){
                tokens ~= new TokenLPar(line, character);
            }else if(c == ')'){
                tokens ~= new TokenRPar(line, character);
            }else if(c == '+'){
                tokens ~= new TokenPlus(line, character);
            }else if(c == '*'){
                tokens ~= new TokenStar(line, character);
            }else if(c == ' ' || c == '\t' || c == '\n'){
            }else{
                throw new LexError(makeMessage("PLAIN", line, character));
            }
        }else if(mode == Am.INT){
            if(c >= '0' && c <= '9'){
                store ~= c;
            }else if(c == ' '){
                tokens ~= new TokenInt(to!(int)(store), s_line, s_character);
                mode = Am.PLAIN;
                store = [];
            }else if(c == '('){
                tokens ~= new TokenInt(to!(int)(store), s_line, s_character);
                tokens ~= new TokenLPar(line, character);
                mode = Am.PLAIN;
                store = [];
            }else if(c == ')'){
                tokens ~= new TokenInt(to!(int)(store), s_line, s_character);
                tokens ~= new TokenRPar(line, character);
                mode = Am.PLAIN;
                store = [];
            }else if(c == '+'){
                tokens ~= new TokenInt(to!(int)(store), s_line, s_character);
                tokens ~= new TokenPlus(line, character);
                mode = Am.PLAIN;
                store = [];
            }else if(c == '*'){
                tokens ~= new TokenInt(to!(int)(store), s_line, s_character);
                tokens ~= new TokenStar(line, character);
                mode = Am.PLAIN;
                store = [];
            }else{
                throw new LexError(makeMessage("INT", line, character));
            }
        }
    }

    return tokens;
}

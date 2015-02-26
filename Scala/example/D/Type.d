import std.conv: to;

enum TAST{
    TASTINT, TASTADD, TASTMUL
}

class AST{
    TAST type;
    string show(){
        return "AST";
    }
}

class ASTInt : AST{
    int value;
    this(int v){
        this.type = TAST.TASTINT;
        this.value = v;
    }

    override string show(){
        return "Int(" ~ to!(string)(this.value) ~ ")";
    }
}

class ASTAdd : AST{
    AST a;
    AST b;
    this(AST a, AST b){
        this.type = TAST.TASTADD;
        this.a = a;
        this.b = b;
    }

    override string show(){
        return "Add(" ~ this.a.show() ~ "," ~ this.b.show() ~ ")";
    }
}

class ASTMul : AST{
    AST a;
    AST b;
    this(AST a, AST b){
        this.type = TAST.TASTMUL;
        this.a = a;
        this.b = b;
    }

    override string show(){
        return "Mul(" ~ this.a.show() ~ "," ~ this.b.show() ~ ")";
    }
}

enum TokenType{
    TOKENINT, TOKENTIMES, TOKENPLUS, TOKENLPAR, TOKENRPAR
}

class Token{
    int line;
    int character;
    TokenType type;
    this(int line, int character){
        this.line = line;
        this.character = character;
    }
    string show(){
        return "Token";
    }
}

class TokenInt : Token{
    int v;
    this(int v, int line = -1, int character = -1){
        super(line, character);
        this.type = TokenType.TOKENINT;
        this.v = v;
    }
}

class TokenPlus : Token{
    this(int line = -1, int character = -1){
        super(line, character);
        this.type = TokenType.TOKENPLUS;
    }
}

class TokenTimes : Token{
    this(int line = -1, int character = -1){
        super(line, character);
        this.type = TokenType.TOKENTIMES;
    }
}

class TokenLPar : Token{
    this(int line = -1, int character = -1){
        super(line, character);
        this.type = TokenType.TOKENLPAR;
    }
}

class TokenRPar : Token{
    this(int line = -1, int character = -1){
        super(line, character);
        this.type = TokenType.TOKENRPAR;
    }
}

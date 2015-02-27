import std.conv: to;

enum ASTType{
    INT, ADD, MUL
}

alias ASTType At;

class AST{
    ASTType type;
    string show(){
        return "AST";
    }
}

class ASTInt : AST{
    int value;
    this(int v){
        this.type = At.INT;
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
        this.type = At.ADD;
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
        this.type = At.MUL;
        this.a = a;
        this.b = b;
    }

    override string show(){
        return "Mul(" ~ this.a.show() ~ "," ~ this.b.show() ~ ")";
    }
}

enum TokenType{
    INT, TIMES, PLUS, LPAR, RPAR
}

alias TokenType Tt;

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
        this.type = Tt.INT;
        this.v = v;
    }
    override string show(){
        return "TokenInt(" ~ to!(string)(v) ~ ")";
    }
}

class TokenPlus : Token{
    this(int line = -1, int character = -1){
        super(line, character);
        this.type = Tt.PLUS;
    }
    override string show(){
        return "TokenPlus";
    }
}

class TokenStar : Token{
    this(int line = -1, int character = -1){
        super(line, character);
        this.type = Tt.TIMES;
    }
    override string show(){
        return "TokenStar";
    }
}

class TokenLPar : Token{
    this(int line = -1, int character = -1){
        super(line, character);
        this.type = Tt.LPAR;
    }
    override string show(){
        return "TokenLPar";
    }
}

class TokenRPar : Token{
    this(int line = -1, int character = -1){
        super(line, character);
        this.type = Tt.RPAR;
    }
    override string show(){
        return "TokenRPar";
    }
}

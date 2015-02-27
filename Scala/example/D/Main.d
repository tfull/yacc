import Type;
import Tokenizer;
import std.cstream;
import std.conv: to;
import std.algorithm;

int main(){
    string s = "(3 + 4) * 3 +7*(2 + 3 * 2) * 4";
    Token[] tokens = tokenize(s);

/*
    dout.writeLine(new ASTAdd(new ASTAdd(new ASTInt(3), new ASTInt(4)), new ASTInt(5)).show());
    char[] cs = ['3', '1', '0'];
    int n = to!(int)(cs);
    dout.writeLine(to!(string)(n));
    */

    dout.writeLine(to!(string)(map!(t => t.show())(tokens)));
    return 0;
}

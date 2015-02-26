import Type;
import std.cstream;
import std.conv: to;

int main(){
    dout.writeLine(new Type.ASTAdd(new Type.ASTAdd(new Type.ASTInt(3), new Type.ASTInt(4)), new Type.ASTInt(5)).show());
    string s = "hello";
    char c = s[0];
    dout.writeLine(to!(string)(c));
    return 0;
}

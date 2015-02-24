import Type;
import std.cstream;

int main(){
    dout.writeLine((new Type.ASTAdd(new Type.ASTInt(3), new Type.ASTInt(4))).show());
    return 0;
}

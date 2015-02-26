import Type;
import std.array;

enum AutomatonMode{
    A_PLAIN, A_INT
}

Token[] lex(string s){
    AutomatonMode mode = 0;

    char[] store;

    for(int i = 0; i < s.length; i++){
        char c = s[i];

        if(mode = PLAIN){

        }else if(mode == A_INT){
        }
    }

    return [];
}

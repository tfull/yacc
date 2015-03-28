#ifndef __TOKEN_H__
#define __TOKEN_H__

typedef enum{
    T_INT, T_PLUS, T_LPAR, T_RPAR,
    T_STAR, T_MINUS, T_SLASH, T_PER, T_END
}TokenType;

typedef union{
    void *p;
    int t_int;
}TokenValue;

typedef struct{
    unsigned int line;
    unsigned int character;
}Position;

typedef struct{
    TokenType tt;
    TokenValue tv;
    int line_s;
    int character_s;
    int line_g;
    int character_g;
}Token;

#endif

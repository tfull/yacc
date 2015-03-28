#include "Type.h"
#include "Token.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum{
    A_PLAIN, A_INT
}Automaton;

void makeTokens(Token *ts, int length, int capacity){
    int i;
    Token *nts = (Token *)malloc(sizeof(int) * capacity);

    for(i = 0; i < length; i++){
        nts[i] = ts[i];
    }

    free(ts);
    ts = nts;
}

Position nextPosition(Position p, char c){
    if(c == '\n'){
        Position q = { p.line + 1U, 0U };
        return q;
    }else{
        Position q = { p.line, p.character + 1U };
        return q;
    }
}

void pushChar(char *cs, unsigned int *capacity, unsigned int *index, char x){
    if(*index >= *capacity){
        unsigned int i;
        unsigned int ncap = *capacity * 2U;
        char *ncs = (char*)malloc(sizeof(char) * ncap);

        for(i = 0U; i < *index; i++){
            ncs[i] = cs[i];
        }
        ncs[*index] = x;
        free(cs);
        cs = ncs;
    }else{
        cs[*index] = x;
    }

    *index = *index + 1U;
}

void pushToken(Token *ts, unsigned int *capacity, unsigned int *index, Token x){
    if(*index >= *capacity){
        unsigned int i;
        unsigned int ncap = *capacity * 2U;
        Token *nts = (Token*)malloc(sizeof(Token) * ncap);

        for(i = 0U; i < *index; i++){
            nts[i] = ts[i];
        }
        nts[*index] = x;        
        free(ts);
        ts = nts;
    }else{
        ts[*index] = x;
    }

    *index = *index + 1U;
}

void raiseTokenError(Position p, char c){
    if(c == '\n'){
        fprintf(stderr, "Tokenizer: unexpected newline in %u:%u\n", p.line, p.character);
    }else if(c == '\t'){
        fprintf(stderr, "Tokenizer: unexpected tab in %u:%u\n", p.line, p.character);
    }else{
        fprintf(stderr, "Tokenizer: unexcepted %c in %u:%u\n", c, p.line, p.character);
    }
}

Token makeCharToken(TokenType tt, Position p){
    TokenValue tv;
    tv.p = NULL;
    Token t = { tt, tv, p.line, p.character, p.line, p.character };
    return t;
}

Token *tokenize(char *cs){
    Automaton am = A_PLAIN;
    unsigned int store_index = 0U;
    unsigned int store_capacity = 64U;
    char *store = (char*)malloc(sizeof(char) * store_capacity);
    unsigned int tokens_index = 0U;
    unsigned int tokens_capacity = 64U;
    Token *tokens = (Token*)malloc(sizeof(char) * tokens_capacity);
    Position p_store = { 0U, 0U };
    Position previous = { 1U, 0U };
    Position position = { 1U, 0U };

    char c;

    while(*cs != '\0'){
        c = *cs;
        previous = position;
        position = nextPosition(position, c);

        if(am == A_PLAIN){
            if(c >= '0' && c <= '9'){
                am = A_INT;
                p_store.line = position.line;
                p_store.character = position.character;
                pushChar(store, &store_capacity, &store_index, c);
            }else if(isspace(c)){
            }else if(c == '+'){
                pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_PLUS, position));
            }else if(c == '*'){
                pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_STAR, position));
            }else if(c == '/'){
                pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_SLASH, position));
            }else if(c == '%'){
                pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_PER, position));
            }else if(c == '-'){
                pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_MINUS, position));
            }else if(c == '('){
                pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_LPAR, position));
            }else if(c == ')'){
                pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_RPAR, position));
            }else{
                raiseTokenError(position, c);
                free(store);
                free(tokens);
                return NULL;
            }
        }else if(am == A_INT){
            if(c >= '0' && c <= '9'){
                pushChar(store, &store_capacity, &store_index, c);
            }else if(isspace(c) || c == '+' || c == '*' || c == '%' || c == '/' || c == '-' || c == '(' || c == ')'){
                am = A_PLAIN;
                pushChar(store, &store_capacity, &store_index, '\0');
                TokenValue tv;
                tv.t_int = atoi(store);
                store_index = 0U;
                Token t = { T_INT, tv, p_store.line, p_store.character, previous.line, previous.character };
                pushToken(tokens, &tokens_capacity, &tokens_index, t);

                if(c == '+'){
                    pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_PLUS, position));
                }else if(c == '*'){
                    pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_STAR, position));
                }else if(c == '/'){
                    pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_SLASH, position));
                }else if(c == '%'){
                    pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_PER, position));
                }else if(c == '-'){
                    pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_MINUS, position));
                }else if(c == '('){
                    pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_LPAR, position));
                }else if(c == ')'){
                    pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_RPAR, position));
                }
            }else{
                raiseTokenError(position, c);
                free(store);
                free(tokens);
                return NULL;
            }
        }

        cs ++;
    }

    if(am == A_INT){
        pushChar(store, &store_capacity, &store_index, '\0');
        TokenValue tv;
        tv.t_int = atoi(store);
        store_index = 0U;
        Token t = { T_INT, tv, p_store.line, p_store.character, position.line, position.character };
        pushToken(tokens, &tokens_capacity, &tokens_index, t);
    }

    pushToken(tokens, &tokens_capacity, &tokens_index, makeCharToken(T_END, position));

    return tokens;
}

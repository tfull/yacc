#include <stdio.h>
#include <stdlib.h>
#include "Type.h"
#include "Token.h"

extern Token *tokenize(char*);

char *getLine(void){
    unsigned int index = 0U;
    unsigned int capacity = 64U;
    char *cs;
    char c;

    cs = malloc(sizeof(char) * capacity);

    while(1){
        c = getchar();
        if(c == '\n'){
            cs[index] = '\0';
            return cs;
        }else{
            cs[index] = c;
        }

        index ++;

        if(index >= capacity){
            unsigned int i;
            char *ncs;
            capacity *= 2U;
            ncs = malloc(sizeof(char) * capacity);
            for(i = 0; i < index; i++){
                ncs[i] = cs[i];
            }
            free(cs);
            cs = ncs;
        }
    }
}

void printPosition(Token t){
    printf(" [%u:%u - %u:%u]\n", t.line_s, t.character_s, t.line_g, t.character_g);
}

void debugTokens(Token *ts){
    while(ts->tt != T_END){
        if(ts->tt == T_INT){
            printf("Integer(%d)", ts->tv.t_int);
        }else if(ts->tt == T_PLUS){
            printf("Plus");
        }else if(ts->tt == T_STAR){
            printf("Star");
        }else if(ts->tt == T_PER){
            printf("Percent");
        }else if(ts->tt == T_SLASH){
            printf("Slash");
        }else if(ts->tt == T_MINUS){
            printf("Minus");
        }else if(ts->tt == T_LPAR){
            printf("LPar");
        }else if(ts->tt == T_RPAR){
            printf("RPar");
        }else{
            printf("Error");
        }

        printPosition(*ts);
        ts ++;
    }
}

void debugTree(Tree *tree){
    if(tree->type == S_INT){
        printf("Int(%d)", tree->node.s_int);
    }else if(tree->type == S_ADD){
        printf("Add(");
        debugTree(tree->node.s_add.tree1);
        printf(",");
        debugTree(tree->node.s_add.tree2);
        printf(")");
    }else if(tree->type == S_SUB){
        printf("Sub(");
        debugTree(tree->node.s_sub.tree1);
        printf(",");
        debugTree(tree->node.s_sub.tree2);
        printf(")");
    }else if(tree->type == S_MUL){
        printf("Mul(");
        debugTree(tree->node.s_mul.tree1);
        printf(",");
        debugTree(tree->node.s_mul.tree2);
        printf(")");
    }else if(tree->type == S_DIV){
        printf("Div(");
        debugTree(tree->node.s_div.tree1);
        printf(",");
        debugTree(tree->node.s_div.tree2);
        printf(")");
    }else if(tree->type == S_MOD){
        printf("Mod(");
        debugTree(tree->node.s_mod.tree1);
        printf(",");
        debugTree(tree->node.s_mod.tree2);
        printf(")");
    }else if(tree->type == S_MINUS){
        printf("Minus(");
        debugTree(tree->node.s_minus.tree);
        printf(")");
    }
}

int main(void){
    char *cs;
    Token *ts;

    cs = getLine();
    ts = tokenize(cs);

    debugTokens(ts);

    free(cs);
    free(ts);

    return 0;
}

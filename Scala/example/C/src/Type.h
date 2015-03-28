typedef enum{
    S_ADD, S_MUL, S_SUB, S_DIV, S_MOD, S_INT, S_MINUS
}Type;

struct _Tree;

typedef struct{
    int value;
}TreeInt;

typedef struct{
    struct _Tree *tree1;
    struct _Tree *tree2;
}TreeAdd;

typedef struct{
    struct _Tree *tree1;
    struct _Tree *tree2;
}TreeSub;

typedef struct{
    struct _Tree *tree1;
    struct _Tree *tree2;
}TreeMul;

typedef struct{
    struct _Tree *tree1;
    struct _Tree *tree2;
}TreeDiv;

typedef struct{
    struct _Tree *tree1;
    struct _Tree *tree2;
}TreeMod;

typedef struct{
    struct _Tree *tree;
}TreeMinus;

typedef union{
    TreeInt s_int;
    TreeAdd s_add;
    TreeSub s_sub;
    TreeMul s_mul;
    TreeDiv s_div;
    TreeMod s_mod;
    TreeMinus s_minus;
}Node;

typedef struct _Tree{
    Type type;
    Node node;
    unsigned int line_s;
    unsigned int char_s;
    unsigned int line_g;
    unsigned int char_g;
}Tree;
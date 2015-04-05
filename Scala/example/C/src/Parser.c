#include "Type.h"
#include "Token.h"
#include <stdlib.h>
#include <stdio.h>

typedef union{
	Token token;
	Tree *tree;
}TFParserItem;

typedef struct{
	int state;
	TFParserItem item;
}TFParserStackItem;

typedef struct{
	unsigned int size;
	unsigned int capacity;
	TFParserStackItem *data;
}TFParserStack;

int tf_parser_accept[38][9] = {
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 1 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 },
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0 }
};

int tf_parser_shift[38][9] = {
	{ 4, -1, -1, -1, 1, -1, -1, 6, -1 },
	{ 16, -1, -1, -1, 13, -1, -1, 18, -1 },
	{ -1, 9, -1, 11, -1, -1, 10, -1, -1 },
	{ -1, -1, 8, -1, -1, -1, -1, 7, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ 4, -1, -1, -1, 1, -1, -1, 6, -1 },
	{ 4, -1, -1, -1, 1, -1, -1, 6, -1 },
	{ 4, -1, -1, -1, 1, -1, -1, 6, -1 },
	{ 4, -1, -1, -1, 1, -1, -1, 6, -1 },
	{ 4, -1, -1, -1, 1, -1, -1, 6, -1 },
	{ 4, -1, -1, -1, 1, -1, -1, 6, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ 16, -1, -1, -1, 13, -1, -1, 18, -1 },
	{ -1, 29, -1, 31, -1, -1, 30, -1, -1 },
	{ -1, -1, 22, -1, -1, 20, -1, 21, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ 16, -1, -1, -1, 13, -1, -1, 18, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ 16, -1, -1, -1, 13, -1, -1, 18, -1 },
	{ 16, -1, -1, -1, 13, -1, -1, 18, -1 },
	{ -1, 9, -1, 11, -1, -1, 10, -1, -1 },
	{ -1, 9, -1, 11, -1, -1, 10, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, 22, -1, -1, 34, -1, 21, -1 },
	{ 16, -1, -1, -1, 13, -1, -1, 18, -1 },
	{ 16, -1, -1, -1, 13, -1, -1, 18, -1 },
	{ 16, -1, -1, -1, 13, -1, -1, 18, -1 },
	{ -1, 29, -1, 31, -1, -1, 30, -1, -1 },
	{ -1, 29, -1, 31, -1, -1, 30, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 }
};

int tf_parser_reduce[38][9] = {
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, 3, -1, -1, -1, -1, 3, 3 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, 8, 8, 8, -1, -1, 8, 8, 8 },
	{ -1, 7, 7, 7, -1, -1, 7, 7, 7 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, 10, 10, 10, -1, -1, 10, 10, 10 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, 3, -1, -1, 3, -1, 3, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, 8, 8, 8, -1, 8, 8, 8, -1 },
	{ -1, 7, 7, 7, -1, 7, 7, 7, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, 10, 10, 10, -1, 10, 10, 10, -1 },
	{ -1, 9, 9, 9, -1, -1, 9, 9, 9 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, 2, -1, -1, -1, -1, 2, 2 },
	{ -1, -1, 1, -1, -1, -1, -1, 1, 1 },
	{ -1, 6, 6, 6, -1, -1, 6, 6, 6 },
	{ -1, 5, 5, 5, -1, -1, 5, 5, 5 },
	{ -1, 4, 4, 4, -1, -1, 4, 4, 4 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, -1, -1, -1, -1, -1, -1, -1 },
	{ -1, -1, 2, -1, -1, 2, -1, 2, -1 },
	{ -1, -1, 1, -1, -1, 1, -1, 1, -1 },
	{ -1, 9, 9, 9, -1, 9, 9, 9, -1 },
	{ -1, 4, 4, 4, -1, 4, 4, 4, -1 },
	{ -1, 5, 5, 5, -1, 5, 5, 5, -1 },
	{ -1, 6, 6, 6, -1, 6, 6, 6, -1 }
};

int tf_parser_goto[38][4] = {
	{ -1, 3, 2, 5 },
	{ -1, 15, 14, 17 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, 12 },
	{ -1, -1, 23, 5 },
	{ -1, -1, 24, 5 },
	{ -1, -1, -1, 25 },
	{ -1, -1, -1, 26 },
	{ -1, -1, -1, 27 },
	{ -1, -1, -1, -1 },
	{ -1, 28, 14, 17 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, 19 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, 32, 17 },
	{ -1, -1, 33, 17 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, 37 },
	{ -1, -1, -1, 36 },
	{ -1, -1, -1, 35 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 },
	{ -1, -1, -1, -1 }
};

Tree *tfParserReduce0(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	t0 = items[0].tree;
	return t0;
}
Tree *tfParserReduce1(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	Tree *t1;
	TreeValue s1;
	Tree *t2;
	TreeValue s2;
	TreeAdd s0_tuple;
	t1 = items[0].tree;
	s0_tuple.tree1 = t1;
	t2 = items[2].tree;
	s0_tuple.tree2 = t2;
	t0 = (Tree*)malloc(sizeof(Tree));
	t0->type = S_ADD;
	t0->node.s_add = s0_tuple;
	return t0;
}
Tree *tfParserReduce2(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	Tree *t1;
	TreeValue s1;
	Tree *t2;
	TreeValue s2;
	TreeSub s0_tuple;
	t1 = items[0].tree;
	s0_tuple.tree1 = t1;
	t2 = items[2].tree;
	s0_tuple.tree2 = t2;
	t0 = (Tree*)malloc(sizeof(Tree));
	t0->type = S_SUB;
	t0->node.s_sub = s0_tuple;
	return t0;
}
Tree *tfParserReduce3(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	t0 = items[0].tree;
	return t0;
}
Tree *tfParserReduce4(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	Tree *t1;
	TreeValue s1;
	Tree *t2;
	TreeValue s2;
	TreeMul s0_tuple;
	t1 = items[0].tree;
	s0_tuple.tree1 = t1;
	t2 = items[2].tree;
	s0_tuple.tree2 = t2;
	t0 = (Tree*)malloc(sizeof(Tree));
	t0->type = S_MUL;
	t0->node.s_mul = s0_tuple;
	return t0;
}
Tree *tfParserReduce5(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	Tree *t1;
	TreeValue s1;
	Tree *t2;
	TreeValue s2;
	TreeDiv s0_tuple;
	t1 = items[0].tree;
	s0_tuple.tree1 = t1;
	t2 = items[2].tree;
	s0_tuple.tree2 = t2;
	t0 = (Tree*)malloc(sizeof(Tree));
	t0->type = S_DIV;
	t0->node.s_div = s0_tuple;
	return t0;
}
Tree *tfParserReduce6(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	Tree *t1;
	TreeValue s1;
	Tree *t2;
	TreeValue s2;
	TreeMod s0_tuple;
	t1 = items[0].tree;
	s0_tuple.tree1 = t1;
	t2 = items[2].tree;
	s0_tuple.tree2 = t2;
	t0 = (Tree*)malloc(sizeof(Tree));
	t0->type = S_MOD;
	t0->node.s_mod = s0_tuple;
	return t0;
}
Tree *tfParserReduce7(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	t0 = items[0].tree;
	return t0;
}
Tree *tfParserReduce8(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	Tree *t1;
	TreeValue s1;
	TreeInt v;
	v.value = items[0].token.tv.t_int;
	t0 = (Tree*)malloc(sizeof(Tree));
	s0.s_int = v;
	t0->type = S_INT;
	t0->node = s0;
	return t0;
}
Tree *tfParserReduce9(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	t0 = items[1].tree;
	return t0;
}
Tree *tfParserReduce10(TFParserItem *items){
	Tree *t0;
	TreeValue s0;
	Tree *t1;
	TreeValue s1;
	t1 = items[1].tree;
	TreeMinus s0_single;
	s0_single.tree = t1;
	s0.s_minus = s0_single;
	t0 = (Tree*)malloc(sizeof(Tree));
	t0->type = S_MINUS;
	t0->node = s0;
	return t0;
}

int tf_parser_reduce_numbers[11] = {2,3,3,1,3,3,3,1,1,3,2};

Tree* (*tf_parser_reduce_functions[11])(TFParserItem*) = {
	tfParserReduce0,
	tfParserReduce1,
	tfParserReduce2,
	tfParserReduce3,
	tfParserReduce4,
	tfParserReduce5,
	tfParserReduce6,
	tfParserReduce7,
	tfParserReduce8,
	tfParserReduce9,
	tfParserReduce10
};

int tf_parser_goto_numbers[11] = {0,1,1,1,2,2,2,2,3,3,3};

TFParserStack TFParserStack_allocate(){
	TFParserStack stack;
	stack.size = 0U;
	stack.capacity = 64U;
	stack.data = (TFParserStackItem*)malloc(sizeof(TFParserStackItem) * 64U);
	return stack;
}

void TFParserStack_push(TFParserStack *stack, TFParserStackItem item){
	if(stack->size >= stack->capacity){
		unsigned int i;
		unsigned int ncap = stack->capacity * 2U;
		TFParserStackItem *ndata = (TFParserStackItem*)malloc(sizeof(TFParserStackItem) * ncap);
		for(i = 0U; i < stack->size; i++){
			ndata[i] = stack->data[i];
		}
		stack->capacity = ncap;
		free(stack->data);
		stack->data = ndata;
	}
	stack->data[stack->size] = item;
	stack->size ++;
}

TFParserStackItem TFParserStack_pop(TFParserStack *stack){
	TFParserStackItem item = stack->data[stack->size - 1U];
	stack->size --;
	return item;
}

void TFParserStack_free(TFParserStack *stack){
	free(stack->data);
}

void tfParserInitStack(TFParserStack *stack){
	TFParserStackItem item = { 0 };
	TFParserStack_push(stack, item);
}

int tfParserGetNowState(TFParserStack *stack){
	return stack->data[stack->size - 1U].state;
}

int tfParserGetTerminalID(Token t){
	if(t.tt == T_PER){
		return 1;
	}else if(t.tt == T_INT){
		return 0;
	}else if(t.tt == T_RPAR){
		return 5;
	}else if(t.tt == T_LPAR){
		return 4;
	}else if(t.tt == T_STAR){
		return 3;
	}else if(t.tt == T_PLUS){
		return 2;
	}else if(t.tt == T_MINUS){
		return 7;
	}else if(t.tt == T_SLASH){
		return 6;
	}else if(t.tt == T_END){
		return 8;
	}else{
		return -1;
	}
}

void debug(Token *ts){
	printf("[");
	while(ts->tt != T_END){
		printf("%d,", tfParserGetTerminalID(* ts));
		ts ++;
	}
	printf("]\n");
}

void tfParserRaiseParseError(Token token){
	fprintf(stderr, "parse error: token(%u:%u - %u:%u)\n", token.line_s, token.character_s, token.line_g, token.character_g);
}

Tree *parse(Token *tokens){
  	Token *ttts = tokens;
	int index = 0;
	TFParserStack stack = TFParserStack_allocate();
	tfParserInitStack(&stack);
	int state;
	while(1){
		printf("[%2d] ", index);
		debug(ttts);
		int tid = tfParserGetTerminalID(tokens[index]);
		state = tfParserGetNowState(&stack);
		printf("[%2d] state %d and tid %d (& %p)\n", index, state, tid, tokens);
		if(tf_parser_accept[state][tid]){
			TFParserStackItem s_item = TFParserStack_pop(&stack);
			TFParserStack_free(&stack);
			return s_item.item.tree;
		}else if(tf_parser_shift[state][tid] != -1){
			printf("[%2d] shift\n", index);
			TFParserItem item;
			TFParserStackItem s_item;
			item.token = tokens[index];
			s_item.state = tf_parser_shift[state][tid];
			s_item.item = item;
			TFParserStack_push(&stack, s_item);
			index ++;
		}else if(tf_parser_reduce[state][tid] != -1){
			printf("[%2d] reduce\n", index);
			TFParserItem items[3];
			TFParserItem item;
			TFParserStackItem s_item;
			int s_i;
			int r_n = tf_parser_reduce[state][tid];
			for(s_i = tf_parser_reduce_numbers[r_n] - 1; s_i >= 0; s_i--){
				TFParserStackItem s_tmp = TFParserStack_pop(&stack);
				items[s_i] = s_tmp.item;
			}
			printf("[%2d] end for r_n = %d, stack.size = %d", index, r_n, stack.size);
			debug(ttts);
			item.tree = tf_parser_reduce_functions[r_n](items);
			printf("[%2d] --", index);
			debug(ttts);
			s_item.state = tf_parser_goto[tfParserGetNowState(&stack)][tf_parser_goto_numbers[r_n]];
			s_item.item = item;
			printf("[%2d] before push\n", index);
			TFParserStack_push(&stack, s_item);
		}else{
			tfParserRaiseParseError(tokens[index]);
			return NULL;
		}
	}
}

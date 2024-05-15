/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls */
static TreeNode* stmt_sequence(void);
static TreeNode* statement(void);
static TreeNode* if_stmt(void); // if -- 修改
static TreeNode* repeat_stmt(void);
static TreeNode* assign_stmt(void); // 赋值 -- 修改
static TreeNode* read_stmt(void);
static TreeNode* write_stmt(void);
static TreeNode* exp(void);
static TreeNode* simple_exp(void);
static TreeNode* term(void); // term(算术表达式) -- 修改
static TreeNode* factor(void);

/* 新增的文法规则 */
static TreeNode* for_stmt(void); // for
static TreeNode* while_stmt(void); // while
static TreeNode* changeself(void); // 前自增、前自减（算术表达式）
static TreeNode* powernum(void); // 阶乘（算术表达式）
// 正则表达式
static TreeNode* reg(void);
static TreeNode* reg_exp1(void);
static TreeNode* reg_exp2(void);
static TreeNode* reg_exp3(void);

static void syntaxError(char* message)
{
	fprintf(listing, "\n>>> ");
	fprintf(listing, "Syntax error at line %d: %s", lineno, message);
	Error = TRUE;
}

static void match(TokenType expected)
{	
	fprintf(listing, "match:token: %d \n", token);
	if (token == expected) token = getToken();
	else {
		//fprintf(listing, "token: %d expected: %d\n", token, expected);
		syntaxError("match--unexpected token -> ");
		printToken(token, tokenString);
		fprintf(listing, "      ");
	}
}

TreeNode* stmt_sequence(void)
{
	fprintf(listing, "stmt_sequence:token: %d \n", token);
	TreeNode* t = statement();
	TreeNode* p = t;
	while ((token != ENDFILE)  &&
		(token != ELSE) && (token != UNTIL) &&
		  (token != ENDWHILE))
	{
		TreeNode* q;
		match(SEMI);
		q = statement();
		if (q != NULL) {
			if (t == NULL) t = p = q;
			else /* now p cannot be NULL either */
			{
				p->sibling = q;
				p = q;
			}
		}
	}
	return t;
}


//P394 
//lineno: 961
TreeNode* statement(void)
{
	fprintf(listing, "statement:token: %d \n", token);
	TreeNode* t = NULL;
	switch (token) {
	case IF: t = if_stmt(); break;
	case REPEAT: t = repeat_stmt(); break;
	case ID: t = assign_stmt(); break;
	case READ: t = read_stmt(); break;
	case WRITE: t = write_stmt(); break;
	case FOR: t = for_stmt(); break;
	case WHILE: t = while_stmt(); break;
	case INCREMENT: t = changeself(); break;
	case DECREMENT: t = changeself(); break;
	default: syntaxError("statmtent--unexpected token -> ");
		printToken(token, tokenString);
		token = getToken();
		break;
	} /* end case */
	return t;
}


//P394 
//lineno: 977
// if_stmt -> if(exp) stmt-sequence [else stmt-sequence]
TreeNode* if_stmt(void)
{
	fprintf(listing, "if_stmt:token: %d \n", token);
	TreeNode* t = newStmtNode(IfK);
	match(IF);
	match(LPAREN);
	if (t != NULL) t->child[0] = exp();
	match(RPAREN);
	if (t != NULL) t->child[1] = stmt_sequence();
	if (token == ELSE) {
		match(ELSE);
		if (t != NULL)
			t->child[2] = stmt_sequence();
	}
	return t;
}

//P394 
//lineno:991
TreeNode* repeat_stmt(void)
{
	fprintf(listing, "repeat_stmt:token: %d \n", token);
	TreeNode* t = newStmtNode(RepeatK);
	match(REPEAT);
	if (t != NULL) t->child[0] = stmt_sequence();
	match(UNTIL);
	if (t != NULL) t->child[1] = exp();
	return t;
}

// assign-stmt -> identifier := exp | identifier == reg
TreeNode* assign_stmt(void)
{
	fprintf(listing, "assign_stmt:token: %d \n", token);
	TreeNode* t = newStmtNode(AssignK);
	if ((t != NULL) && (token == ID))
		t->attr.name = copyString(tokenString);
	match(ID);
	
	switch (token)
	{
	case ASSIGN:
		match(ASSIGN);
		if (t != NULL) t->child[0] = exp();
		break;
	case RASSIGN:
		match(RASSIGN);
		if (t != NULL) t->child[0] = reg();
		break;
	default:
		syntaxError("assign--unexpected token -> ");
		printToken(token, tokenString);
		token = getToken();
		break;
	}

	return t;
}

TreeNode* read_stmt(void)
{
	fprintf(listing, "read_stmt:token: %d \n", token);
	TreeNode* t = newStmtNode(ReadK);
	match(READ);
	if ((t != NULL) && (token == ID))
		t->attr.name = copyString(tokenString);
	match(ID);
	return t;
}

TreeNode* write_stmt(void)
{
	fprintf(listing, "write_stmt:token: %d \n", token);
	TreeNode* t = newStmtNode(WriteK);
	match(WRITE);
	if (t != NULL) t->child[0] = exp();
	return t;
}

TreeNode* exp(void)
{
	fprintf(listing, "exp:token: %d \n", token);
	TreeNode* t = simple_exp();
	if ((token == LT) || (token == EQ) || (token == GT) || (token == GE) ||
		(token == LE) || (token == NE) ) {
		TreeNode* p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
		}
		match(token);
		if (t != NULL)
			t->child[1] = simple_exp();
	}
	return t;
}

TreeNode* simple_exp(void)
{
	fprintf(listing, "simple_exp:token: %d \n", token);
	TreeNode* t = term();
	while ((token == PLUS) || (token == MINUS))
	{
		TreeNode* p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			t->child[1] = term();
		}
	}
	return t;
}

TreeNode* term(void)
{
	fprintf(listing, "term:token: %d \n", token);
	TreeNode* t = changeself();
	while ((token == TIMES) || (token == OVER) || (token == MOD))
	{
		TreeNode* p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			p->child[1] = changeself();
		}
	}
	return t;
}

TreeNode* factor(void)
{
	fprintf(listing, "factor:token: %d \n", token);
	TreeNode* t = NULL;
	switch (token) {
	case NUM:
		t = newExpNode(ConstK);
		if ((t != NULL) && (token == NUM))
			t->attr.val = atoi(tokenString);
		match(NUM);
		break;
	case ID:
		t = newExpNode(IdK);
		if ((t != NULL) && (token == ID))
			t->attr.name = copyString(tokenString);
		match(ID);
		break;
	case LPAREN:
		match(LPAREN);
		t = exp();
		match(RPAREN);
		break;
	default:
		syntaxError("factor--unexpected token -> ");
		printToken(token, tokenString);
		token = getToken();
		break;
	}
	return t;
}

/*
* changeself -> changeop changeself | powernum
* changeop -> ++ | --
*/
TreeNode* changeself(void)
{
	fprintf(listing, "changeself:token: %d \n", token);
	TreeNode* t = NULL;
	switch (token) {
	case INCREMENT: // ++ 
		t = newExpNode(OpK);
		if (t != NULL) {
			t->attr.op = token;
			match(INCREMENT);
			/*TreeNode* p = newExpNode(IdK);
			t->child[1] = p;
			if ((p != NULL) && (token == ID))
				p->attr.name = copyString(tokenString);
			match(ID);*/

			t->child[0] = changeself();
		}
		break;
	case DECREMENT: // -- 
		t = newExpNode(OpK);
		if (t != NULL) {
			t->attr.op = token;
			match(DECREMENT);
			/*TreeNode* p = newExpNode(IdK);
			t->child[1] = p;
			if ((p != NULL) && (token == ID))
				p->attr.name = copyString(tokenString);
			match(ID);*/

			t->child[0] = changeself();
		}
		break;
	case LPAREN: // powernum
	case NUM:
	case ID:
		fprintf(listing, "changeself,goto powernum:token: %d \n", token);
		t = powernum();
		break;
	default:
		syntaxError("changeself--unexpected token -> ");
		printToken(token, tokenString);
		token = getToken();
		break;
	}
	return t;
}

/*
* powernum -> factor {powerop factor}
* powerop -> ^
*/
TreeNode* powernum(void)
{
	fprintf(listing, "powernum:token: %d \n", token);
	TreeNode* t = factor();
	while (token == POW)
	{
		TreeNode* p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			p->child[1] = factor();
		}
	}
	return t;
}

// for-stmt -> for(identifier:=simple-exp;exp;simple-exp)  stmt-sequence 
TreeNode* for_stmt(void)
{
	fprintf(listing, "for_stmt:token: %d \n", token);
	TreeNode* t = newStmtNode(ForK);
	match(FOR);
	match(LPAREN);
	if (t != NULL) t->child[0] = assign_stmt();
	match(SEMI);
	if (t != NULL) t->child[1] = exp();
	match(SEMI);
	if (t != NULL) t->child[2] = simple_exp();
	match(RPAREN);
	if (t != NULL) t->child[3] = stmt_sequence();
	return t;
}

// while-stmt -> while(exp) stmt-sequence endwhile
TreeNode* while_stmt(void)
{
	fprintf(listing, "while_stmt:token: %d \n", token);
	TreeNode* t = newStmtNode(WhileK);
	match(WHILE);
	match(LPAREN);
	if (t != NULL) t->child[0] = exp();
	match(RPAREN);
	if (t != NULL) t->child[1] = stmt_sequence();
	match(ENDWHILE);
	return t;
}

// reg -> reg-exp1{|reg}
TreeNode* reg(void)
{
	TreeNode* t = reg_exp1();
	while (token == ROR)
	{
		TreeNode* p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			p->child[1] = reg_exp1();
		}
	}
	return t;
}

// reg-exp1 -> reg-exp2{&reg-exp1}
TreeNode* reg_exp1(void)
{
	TreeNode* t = reg_exp2();
	while (token == RAND)
	{
		TreeNode* p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			p->child[1] = reg_exp2();
		}
	}
	return t;
}

// reg-exp2 -> reg-exp2# | reg-exp2? | reg-exp3
TreeNode* reg_exp2(void)
{
	TreeNode* t = reg_exp3();
	while (token == RC || token == RS)
	{
		TreeNode* p = newExpNode(OpK);
		if (p != NULL) {
			p->child[0] = t;
			p->attr.op = token;
			t = p;
			match(token);
			t->child[1] = reg_exp3();
		}
	}
	return t;
}

// reg-exp3 -> (reg) | identifier
TreeNode* reg_exp3(void)
{
	TreeNode* t = NULL;
	switch (token) {
	case LPAREN:
		match(LPAREN);
		t = reg();
		match(RPAREN);
		break;
	case ID:
		t = newExpNode(IdK);
		if ((t != NULL) && (token == ID))
			t->attr.name = copyString(tokenString);
		match(ID);
		break;
	default:
		syntaxError("reg_exp3--unexpected token -> ");
		printToken(token, tokenString);
		token = getToken();
		break;
	}
	return t;
}

/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly
 * constructed syntax tree
 */
TreeNode* parse(void)
{
	TreeNode* t;
	token = getToken();
	t = stmt_sequence();
	if (token != ENDFILE)
		syntaxError("Code ends before file\n");
	return t;
}

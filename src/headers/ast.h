#ifndef AST_h
#define AST_h

#include "token.h"
#include "common.h"
#include "type.h"
#include "table.h"

struct Parser;
struct Ast;
struct Name; // context.h
struct Context;
union Register;

typedef enum {
    Decl_PROC,
    Decl_VAR,
    Decl_TYPEDEF,
} DeclType;

typedef enum {
    Expr_LITERALS_START = 0,
    Expr_INT,
    Expr_FLOAT,
    Expr_STRING,
    Expr_BOOL,
    Expr_NULL,
    Expr_ARRAY,
    Expr_LITERALS_END,

    Expr_NAME,
    Expr_BINARY,
    Expr_UNARY,
    Expr_SELECTOR,
    Expr_PAREN,
    Expr_CAST,
    Expr_CALL,
    Expr_INDEX,
    Expr_VAR_ARGS_EXPAND,
    Expr_IMPORT,
} ExprType;

typedef enum {
    Stmt_ASSIGN = 0,
    Stmt_BLOCK,
    Stmt_IF,
    Stmt_FOR,
    Stmt_WHILE,
    Stmt_STRUCT,
    Stmt_ENUM,
    Stmt_RETURN,
    Stmt_DEFER,
    Stmt_CALL,
    Stmt_USING,
} StmtType;

typedef enum {
    Node_ZERO = 0, // nothing, just a sentinal value

    Node_TYPENAME,

    Node_EXPRESSIONS_START, // sentinal expressions start

        Node_LITERALS_START,
            Node_INT_LIT,
            Node_FLOAT_LIT,
            Node_STRING_LIT,
            Node_BOOL_LIT,
            Node_NIL,
            Node_ARRAY_LIT,
        Node_LITERALS_END,

        Node_IDENT,
        Node_BINARY,
        Node_UNARY,
        Node_SELECTOR,
        Node_PAREN,
        Node_CAST,
        Node_CALL,
        Node_INDEX,
        Node_VAR_ARGS_EXPAND,
		Node_IMPORT,
        //Node_INITIALIZER,

    Node_EXPRESSIONS_END, // sentinal expressions end

    Node_STATEMENTS_START, // sentinal statements start

        Node_DECLS_START, // declarations

            Node_PROCEDURE,
            Node_TYPEDEF,
            Node_VAR,

        Node_DECLS_END, // decls end

        // More statements
        Node_ASSIGN,
        Node_BLOCK,
        Node_IF,
        Node_FOR,
        Node_WHILE,
        Node_STRUCT,
        Node_ENUM,
        Node_RETURN,
        Node_DEFER,
        Node_USING,

    Node_STATEMENTS_END, // sentinal expressions end
} AstNodeType;

enum {
    VAR_IS_INFERRED = 1 << 0,
    VAR_IS_INITED   = 1 << 1,
    VAR_IS_VARARGS  = 1 << 2,
    VAR_IS_CONST    = 1 << 3,
};

struct AstExpr;
struct AstStmt;
struct AstDecl;

typedef struct AstVar {
    struct Name *name;
    struct AstExpr *value;
    struct AstNode *typename;
    int flags;
} AstVar;

enum {
    PROC_IS_FOREIGN        = 1 << 0,
    PROC_RET_VALUE_CHECKED = 1 << 1,
};
typedef struct AstProcedure {
    struct Ast *params;
    struct AstStmt *block;
    struct Name *name;
    struct AstNode *return_type;
    struct AstVar *var_args;
    int flags;
    struct AstExpr *foreign_link_name;
} AstProcedure;

typedef struct {
    struct AstStmt *members;
} AstStruct;

typedef struct {
    Table fields;
    Type *base_type;
} AstEnum;

typedef struct {
    struct AstExpr *name;
    struct AstExpr *value;
} AstAssignment;

typedef struct {
    struct AstExpr *condition;
    struct AstStmt *block_or_stmt;

    // This will be an AstBlock for 'else' or
    // AstIf for an else if.
    struct AstStmt *other_branch;
} AstIf;

typedef struct {
    char *path;
} AstImport;

typedef struct {
    struct AstNode *condition;
    struct AstNode *block;
} AstFor;

typedef struct {
    struct AstExpr *condition;
    struct AstStmt *block;
} AstWhile;

typedef struct {
    struct AstExpr *expr;
    struct AstBlock *owning;
} AstReturn;

typedef struct AstUsing {
    struct Name *what;
} AstUsing;

typedef struct AstLiteral {
    union {
        s64 integer;
        char *string;
        struct Ast *array;
        double floating;
        bool boolean;
    } data;
} AstLiteral;

enum {
    CALL_MOD_NONE = 1 << 0,
    CALL_MOD_INLINE = 1 << 1,
};

typedef struct {
    struct AstExpr *name;
    struct Ast *params;
    AstProcedure *calling; // filled out in resolve.c
    int flags;
} AstCall;

typedef struct {
    struct AstExpr *left;
    struct AstExpr *right;
    TokenType op;
} AstBinary;

typedef struct {
    TokenType op;
    struct AstExpr *expr;
} AstUnary;

typedef struct {
    struct AstExpr *left;
    struct AstExpr *right;
} AstSelector;

typedef struct {
    struct AstExpr *sub_expr;
} AstParen;

typedef struct {
    //Type *type;
    struct AstNode *typename;
    struct AstExpr *expr;
} AstCast;

typedef struct {
    struct AstExpr *name;
    struct AstExpr *index;
} AstArrayIndex;

typedef struct {
    struct AstStmt *statement;
} AstDefer;

typedef struct {
    struct Name *name;
} AstVarArgsExpand;

typedef struct AstBlock {
    struct Ast *statements;
    struct AstBlock *parent;
    struct Ast *deferred;
} AstBlock;

typedef struct AstExpr {
    union {
        struct Name *name;
        AstVarArgsExpand var_args_expand;
        AstLiteral literal;
        AstCast cast;
        AstBinary binary;
        AstUnary unary;
        AstSelector selector;
        AstParen paren;
        AstCall call;
        AstArrayIndex index;
        AstImport import;
    } as;
    Type *resolved_type;
    ExprType tag;
} AstExpr;

typedef struct AstStmt {
    union {
        AstExpr assign;
        AstBlock block;
        AstIf _if;
        AstWhile _while;
        AstStruct _struct;
        AstEnum _enum;
        AstReturn _return;
        AstDefer defer;
        AstCall call;
        AstUsing using;
    } as;
    StmtType tag;
} AstStmt;

enum {
    DECL_IS_TOP_LEVEL     = 1 << 0,
    DECL_IS_PROC_ARGUMENT = 1 << 1,
    DECL_IS_EXPORTED      = 1 << 2,
	DECL_IS_STRUCT_FIELD  = 1 << 3,
};

typedef struct AstDecl {
    union {
        AstProcedure proc;
        AstVar var;
        Type *type;
    } as;
    struct Name *name;
    DeclType tag;
    int flags;
    enum {
        Status_RESOLVED,
        Status_RESOLVING,
        Status_UNRESOLVED,
    } status;
} AstDecl;

typedef struct AstNode {
    union {
        AstExpr expr;
        AstDecl decl;
        AstStmt stmt;
        Type *type;
    } as;
    AstNodeType tag;
    Token token;
} AstNode;

typedef struct Ast {
    AstNode **nodes;
    u64 cap;
    u64 len;
} Ast;

void ast_init(Ast *, int);
void ast_free(Ast *);
void ast_add(Ast *, AstNode *);

AstNode *ast_node(struct Context *, AstNodeType tag, Token);
AstNode *ast_decl(struct Context *, AstNodeType tag, Token, const AstDecl *);
AstNode *ast_expr(struct Context *, AstNodeType tag, Token, const AstExpr *);
AstNode *ast_stmt(struct Context *, AstNodeType tag, Token, const AstStmt *);

AstExpr *ast_name(struct Context *, Token t);
AstExpr *ast_binary(struct Context *, Token t, const AstBinary *binary);
AstExpr *ast_unary(struct Context *, Token t, const AstUnary *unary);
AstExpr *ast_selector(struct Context *, Token t, const AstSelector *sel);
AstExpr *ast_paren(struct Context *, Token t, const AstParen *paren);
AstExpr *ast_cast(struct Context *, Token t, const AstCast *cast);
AstExpr *ast_index(struct Context *, Token t, const AstArrayIndex *index);
AstExpr *ast_var_args_expand(struct Context *, Token t, const AstVarArgsExpand *expand);
AstExpr *ast_import(struct Context *, Token t, const AstImport *imp);

AstDecl *ast_proc(struct Context *, Token t, struct Name *name, const AstProcedure *proc);
AstDecl *ast_var(struct Context *, Token t, struct Name *name, const AstVar *var);
// AstDecl *ast_typedefi(struct Context *, Token t, struct Name *name, const AstTypedef *td);
AstDecl *ast_typedefi(struct Context *c, Token t, struct Name *name, Type *type);

AstStmt *ast_assignment(struct Context *, Token t, const AstExpr *ass);
AstStmt *ast_block(struct Context *, Token t, const AstBlock *blk);
AstStmt *ast_if(struct Context *, Token t, const AstIf *i);
AstStmt *ast_while(struct Context *, Token t, const AstWhile *w);
AstStmt *ast_struct(struct Context *, Token t, const AstStruct *s);
AstStmt *ast_enum(struct Context *, Token t, const AstEnum *e);
AstStmt *ast_return(struct Context *, Token t, const AstReturn *r);
AstStmt *ast_defer(struct Context *, Token t, const AstDefer *d);
AstStmt *ast_using(struct Context *, Token t, const AstUsing *u);

AstExpr *ast_call_expr(struct Context *, Token t, const AstCall *call);
AstStmt *ast_call_stmt(struct Context *, Token t, const AstCall *call);

struct Name *get_decl_name(AstNode *);

bool is_decl(AstNode *);
bool is_assignment(AstBinary);
bool is_binary_comparison(AstBinary);
bool is_literal(AstNode *);

Token decl_tok(AstDecl *);
Token expr_tok(AstExpr *);
Token stmt_tok(AstStmt *);

#endif

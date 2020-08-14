#ifndef AST_h
#define AST_h

#include "token.h"
#include "common.h"
#include "type.h"

struct Parser;
struct Ast;
struct Name; // context.h
struct Context;

typedef enum {
    Node_ZERO, // nothing, just a sentinal value

    Node_ERROR,

    Node_TYPENAME,
    Node_PROC_MOD,

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
        Node_IMPORT,
        Node_BLOCK,
        Node_IF,
        Node_FOR,
        Node_WHILE,
        Node_STRUCT,
        Node_RETURN,
        Node_DEFER,

    Node_STATEMENTS_END, // sentinal expressions end
} AstNodeType;

enum {
    VAR_IS_INFERRED         = 1 << 0,
    VAR_TYPE_IS_ANON_STRUCT = 1 << 1,
    VAR_IS_INITED           = 1 << 2,
};

typedef struct {
    struct AstNode *name;
    struct AstNode *value;
    struct AstNode *typename;
    int flags;
} AstVar;

enum {
    PROC_MOD_FOREIGN = 1 << 0,
};
typedef struct {
    struct SymbolTable *params;
    struct AstNode *block;
    struct AstNode *name;
    struct AstNode *return_type;
    int flags;
} AstProcedure;

typedef struct {
    struct AstNode *members;
} AstStruct;

typedef struct {
    struct AstNode *name;
    struct AstNode *value;
} AstAssignment;

typedef struct {
    struct AstNode *condition;
    struct AstNode *block_or_stmt;

    // This will be an AstBlock for 'else' or
    // AstIf for an else if.
    struct AstNode *other_branch;
} AstIf;

typedef struct {
    struct AstNode *of;
    struct AstNode *name;
} AstTypedef;

typedef struct {
    char *path;
} AstImport;

typedef struct {
    struct AstNode *condition;
    struct AstNode *block;
} AstFor;

typedef struct {
    struct AstNode *condition;
    struct AstNode *block;
} AstWhile;

typedef struct {
    struct AstNode *expr;
} AstReturn;

typedef struct {
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
    struct AstNode *name;
    struct Ast *params;
    AstProcedure *calling; // filled out in resolve.c
    int flags;
} AstCall;

typedef struct {
    struct AstNode *left;
    struct AstNode *right;
    TokenType op;
} AstBinary;

typedef struct {
    TokenType op;
    struct AstNode *expr;
} AstUnary;

typedef struct {
    struct AstNode *left;
    struct AstNode *right;
} AstSelector;

// typedef struct {
//     Type *real_type;
// } AstTypename;

typedef struct {
    struct AstNode *sub_expr;
} AstParen;

typedef struct {
    //Type *type;
    struct AstNode *typename;
    struct AstNode *expr;
} AstCast;

typedef struct {
    struct AstNode *name;
    struct AstNode *index;
} AstArrayIndex;

typedef struct {
    struct AstNode *statement;
} AstDefer;

struct SymbolTable;

typedef struct AstBlock {
    // These overlap, meaning, declarations that are also 
    // statements will be added to statements and decls.
    // Initialized AstBlocks can be casted to an Ast.
    struct Ast *statements;
    struct SymbolTable *symbols;
    struct AstBlock *parent;
} AstBlock;

typedef struct AstStmt {
    union {
        //AstAssignment assign;
        AstBinary binary; // TODO replace with real assignment
        AstImport _import;
        AstBlock block;
        AstIf _if;
        AstWhile _while;
        AstStruct _struct;
        AstReturn _return;
        AstDefer defer;
        AstCall call;
    } as;
} AstStmt;

typedef struct AstExpr {
    union {
        struct Name *name;
        AstLiteral literal;
        AstCast cast;
        AstBinary binary;
        AstUnary unary;
        AstSelector selector;
        AstParen paren;
        AstCall call;
        AstArrayIndex index;
    } as;
    Type *resolved_type;
} AstExpr;

typedef struct AstDecl {
    union {
        AstProcedure proc;
        AstVar var;
        AstTypedef typedefi;
    } as;
    struct Name *name;
} AstDecl;

typedef struct AstError {
    char *msg;
    u64 line;
    s32 column;
} AstError;

typedef struct AstNode {
    union {
        AstError error;
        AstExpr expr;
        AstDecl decl;
        AstStmt stmt;
        AstCall call;
        struct Name *ident;
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

AstNode *ast_node(struct Parser *, AstNodeType tag, Token);
AstNode *ast_decl(struct Parser *, AstNodeType tag, Token, const AstDecl *);
AstNode *ast_expr(struct Parser *, AstNodeType tag, Token, const AstExpr *);
AstNode *ast_stmt(struct Parser *, AstNodeType tag, Token, const AstStmt *);

AstNode *ast_name(struct Parser *p, Token t, struct Name *name);
AstNode *ast_binary(struct Parser *p, Token t, const AstBinary *binary);
AstNode *ast_unary(struct Parser *p, Token t, const AstUnary *unary);
AstNode *ast_selector(struct Parser *p, Token t, const AstSelector *sel);
AstNode *ast_paren(struct Parser *p, Token t, const AstParen *paren);
AstNode *ast_cast(struct Parser *p, Token t, const AstCast *cast);
AstNode *ast_index(struct Parser *p, Token t, const AstArrayIndex *index);
AstNode *ast_proc(struct Parser *p, Token t, struct Name *name, const AstProcedure *proc);
AstNode *ast_var(struct Parser *p, Token t, struct Name *name, const AstVar *var);
AstNode *ast_typedefi(struct Parser *p, Token t, struct Name *name, const AstTypedef *td);
AstNode *ast_assignment(struct Parser *p, Token t, const AstBinary *ass);
AstNode *ast_import(struct Parser *p, Token t, const AstImport *imp);
AstNode *ast_block(struct Parser *p, Token t, const AstBlock *blk);
AstNode *ast_if(struct Parser *p, Token t, const AstIf *i);
AstNode *ast_while(struct Parser *p, Token t, const AstWhile *w);
AstNode *ast_struct(struct Parser *p, Token t, const AstStruct *s);
AstNode *ast_return(struct Parser *p, Token t, const AstReturn *r);
AstNode *ast_defer(struct Parser *p, Token t, const AstDefer *d);
AstNode *ast_call(struct Parser *p, Token t, const AstCall *call);

AstNode *make_error_node(struct Parser *, Token, const char *msg);
AstNode *make_ident_node(struct Context *, Token);

struct Name *get_decl_name(AstNode *);

bool is_decl(AstNode *);
bool is_assignment(AstBinary);
bool is_binary_comparison(AstBinary);
bool is_literal(AstNode *);

#endif

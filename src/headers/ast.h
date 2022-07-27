#ifndef AST_h
#define AST_h

#include "token.h"
#include "common.h"

struct Ast;
struct AstExpr;
struct AstStmt;
struct AstDecl;
struct AstTypeDecl;
struct AstStruct;
struct AstEnum;
struct AstBinary;
struct AstIdent;
struct AstTypename;
struct AstBlock;
struct AstUnary;

struct Name; // context.h
struct Parser;
struct Context;
struct ProcedureType;

enum TypeDeclExprType {
    TypeDecl_PLACEHOLDER,

    TypeDecl_BUILTIN_START,

    TypeDecl_STRING,
    TypeDecl_INTEGER,
    TypeDecl_FLOAT,
    TypeDecl_BOOL,
    TypeDecl_VOID,
    TypeDecl_ANY,
    TypeDecl_TYPE, // the type of 'Type'

    TypeDecl_BUILTIN_END,

    TypeDecl_POINTER,
    TypeDecl_ARRAY,

    TypeDecl_STRUCT,
    TypeDecl_ENUM,
    TypeDecl_PROCEDURE,

    TypeDecl_ALIAS,
};

enum DeclStatus {
    Status_UNRESOLVED,
    Status_RESOLVED,
    Status_RESOLVING,
};

enum {
    DECL_IS_TOP_LEVEL     = 1 << 0,
    DECL_IS_PROC_ARGUMENT = 1 << 1,
    DECL_IS_EXPORTED      = 1 << 2,
    DECL_IS_STRUCT_FIELD  = 1 << 3,
    DECL_IS_CONST         = 1 << 4,
    DECL_IS_INFERRED      = 1 << 5,
    DECL_IS_VAR_ARGS      = 1 << 6,
};

enum {
    TYPE_DECL_IS_SIGNED_INTEGER = 1 << 6,
};

enum {
    PROC_IS_FOREIGN        = 1 << 0,
    PROC_RET_VALUE_CHECKED = 1 << 1,
};

enum {
    CALL_MOD_NONE = 1 << 0,
    CALL_MOD_INLINE = 1 << 1,
};

enum AstNodeType {
    Node_ZERO = 0, // nothing, just a sentinal value

    Node_EXPRESSIONS_START, // sentinal expressions start

        Node_LITERALS_START,
            Node_INT_LIT,
            Node_FLOAT_LIT,
            Node_STRING_LIT,
            Node_BOOL_LIT,
            Node_NULL_LIT,
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
        Node_LIBRARY,
        Node_PROCEDURE,

        Node_TYPENAME,
        Node_STRUCT,
        Node_ENUM,

    Node_EXPRESSIONS_END, // sentinal expressions end

    Node_STATEMENTS_START, // sentinal statements start

        Node_DECL,
        Node_TYPE_DECL,

        Node_ASSIGN,
        Node_BLOCK,
        Node_IF,
        Node_FOR,
        Node_WHILE,
        Node_RETURN,
        Node_DEFER,
        Node_USING,

    Node_STATEMENTS_END, // sentinal statements end
};

struct AstNode {
    AstNodeType tag;
    Token       token;
};

struct Ast {
    AstNode **nodes;
    u64 cap;
    u64 len;
};

struct AstStmt: public AstNode {

};


//
// Declarations
//
struct AstDecl: public AstStmt {
    Name        *name       = NULL;
    AstExpr     *expr       = NULL;
    AstTypename *given_type = NULL;
    DeclStatus   status     = Status_UNRESOLVED;
    int          flags      = 0;
};

struct AstTypeDecl: public AstDecl {
    u64 size = 0;
    union {
        AstStruct   *struct_;
        AstEnum     *enum_;
        AstTypeDecl *base_type;
        AstTypename *alias;
        ProcedureType *proc;
    };
    TypeDeclExprType expr_type;
};

struct AstUsing: public AstDecl {
    AstNode  *what           = NULL;
    AstBlock *resolved_scope = NULL;
};


//
// Expressions
//
struct AstExpr: public AstNode {
    AstTypeDecl *resolved_type = NULL;
};

struct AstLibrary: public AstExpr {
    char *library;
    bool is_static;
};

struct AstProcedure: public AstExpr {
    Ast         *params;
    AstStmt     *block;
    AstTypename *return_type;
    AstExpr     *foreign_link_name;
    Name        *library_name;
    int          var_args_index;
    int          flags;
};

struct AstIdent: public AstExpr {
    Name    *name          = NULL;
    AstDecl *resolved_decl = NULL;
};

struct AstTypename: public AstExpr {
    // Only one of these is set at a time
    // When I used a union, it didn't work though.
    // I need them both initialized to NULL at first.
    Name      *name        = NULL;
    AstBinary *selector    = NULL;
    // AstUnary  *ptr         = NULL;
    AstTypename *ptr = NULL;
    AstExpr   *array_count = NULL;
};

struct AstStruct: public AstExpr {
    AstStmt *members;
};

struct AstEnum: public AstExpr {
    AstBlock    *constants;
    AstTypename *base_type;
};

struct AstImport: public AstExpr {
    char *path;
};

struct AstVarArgsExpand: public AstExpr {
    AstIdent *name;
};

struct AstLiteral: public AstExpr {
    union {
        s64 integer;
        char *string;
        Ast *array;
        double floating;
        bool boolean;
    };
};

struct AstBinary: public AstExpr {
    AstExpr *left;
    AstExpr *right;
    TokenType op;
};

struct AstUnary: public AstExpr {
    TokenType op;
    AstExpr *expr;
};

struct AstParen: public AstExpr {
    AstExpr *sub_expr;
};

struct AstCast: public AstExpr {
    AstTypename *type;
    AstExpr *expr;
};

struct AstArrayIndex: public AstExpr {
    AstExpr *name;
    AstExpr *index;
};


//
// Statements
//
struct AstIf: public AstStmt {
    AstExpr *condition;
    AstStmt *block_or_stmt;

    // This will be an AstBlock for 'else' or
    // AstIf for an else if.
    AstStmt *other_branch;
};

struct AstFor: public AstStmt {
    AstNode *condition;
    AstNode *block;
};

struct AstWhile: public AstStmt {
    AstExpr *condition;
    AstStmt *block;
};

struct AstReturn: public AstStmt {
    AstExpr *expr;
    AstBlock *owning;
};

struct AstDefer: public AstStmt {
    AstStmt *statement;
};

struct AstAssignment: public AstStmt {
    AstExpr *expr;
};

struct AstBlock: public AstStmt {
    Ast *statements;
    AstBlock *parent;
    Ast *deferred;
    Ast usings;
};


// A call can be an expression, or a statement.
struct AstCall: public AstExpr {
    AstExpr      *name;
    Ast          *params;
    AstProcedure *calling; // filled out in resolve.c
    int           flags;
};

struct AstCallStmt: public AstStmt {
    AstCall *expr;
};


#define NODEBLOCK_ARRAY_SIZE 64
struct NodeBlock {
    AstNode    nodes[NODEBLOCK_ARRAY_SIZE];
    u64        nodes_count;
    NodeBlock *next_block;
};

struct NodeArena {
    NodeBlock *first_block;
    NodeBlock *current_block;
};

void ast_init(Ast *, int);
void ast_free(Ast *);
void ast_add(Ast *, AstNode *);

Ast *make_subtree();

bool node_arena_init(NodeArena *);
void node_arena_free(NodeArena *);
AstNode *node_arena_alloc(NodeArena *);

AstNode *ast_node(struct Context *, AstNodeType tag, Token);
AstNode *ast_expr(struct Context *, AstNodeType tag, Token, const AstExpr *);
AstNode *ast_stmt(struct Context *, AstNodeType tag, Token, const AstStmt *);

AstDecl *ast_decl(struct Context *c, Token t, const AstDecl *decl);

AstIdent *ast_name(struct Context *, Token t);
AstBinary *ast_binary(struct Context *, Token t, const AstBinary *binary);
AstUnary *ast_unary(struct Context *, Token t, const AstUnary *unary);
AstParen *ast_paren(struct Context *, Token t, const AstParen *paren);
AstCast *ast_cast(struct Context *, Token t, const AstCast *cast);
AstArrayIndex *ast_index(struct Context *, Token t, const AstArrayIndex *index);
AstVarArgsExpand *ast_var_args_expand(struct Context *, Token t, const AstVarArgsExpand *expand);
AstImport *ast_import(struct Context *, Token t, const AstImport *imp);
AstLibrary *ast_library(struct Context *, Token t, char *library, bool is_static);
AstProcedure *ast_procedure(struct Context *, Token t, const AstProcedure *imp);
AstStruct *ast_struct(struct Context *, Token t, const AstStruct *s);
AstEnum *ast_enum(struct Context *, Token t, const AstEnum *e);

AstAssignment *ast_assignment(struct Context *, Token t, AstExpr *ass);
AstBlock *ast_block(struct Context *, Token t);
AstIf *ast_if(struct Context *, Token t, const AstIf *i);
AstWhile *ast_while(struct Context *, Token t, const AstWhile *w);
AstReturn *ast_return(struct Context *, Token t, const AstReturn *r);
AstDefer *ast_defer(struct Context *, Token t, const AstDefer *d);
AstUsing *ast_using(struct Context *, Token t, const AstUsing *u);

AstCall *ast_call_expr(struct Context *, Token t, const AstCall *call);
AstCallStmt *ast_call_stmt(struct Context *, Token t, AstCall *expr);

struct Name *get_decl_name(AstNode *);

bool is_assignment(AstBinary);
bool is_binary_comparison(AstBinary);
bool is_literal(AstNode *);

#endif

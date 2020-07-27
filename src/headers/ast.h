#ifndef AST_h
#define AST_h

#include "token.h"
#include "common.h"
#include "type.h"

struct Parser;
struct Ast;

typedef enum ExpressionType {
    Node_ZERO, // nothing, just a sentinal value

    Node_ERROR,

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
        Node_ACCESSOR,
        Node_ENCLOSED,
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

typedef struct AstError {
    char *msg;
    u64 line;
    s32 column;
} AstError;

static const u8 VAR_IS_INFERRED         = 1 << 0;
static const u8 VAR_TYPE_IS_ANON_STRUCT = 1 << 1;
static const u8 VAR_IS_INITED           = 1 << 2;
typedef struct {
    struct AstNode *name;
    struct AstNode *value;
    struct AstNode *typename;
    int flags;
} AstVar;

typedef struct {
    struct Ast *params;
    struct AstNode *block;
    struct AstNode *identifier;
    struct AstNode *return_type;
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
    char *name;
} AstIdent;

typedef struct {
    union {
        s64 integer;
        char *string;
        struct Ast *array;
        double floating;
        bool boolean;
    } data;
} AstLiteral;

typedef struct {
    struct AstNode *name;
    struct Ast *params;
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
} AstAccessor;

// typedef struct {
//     Type *real_type;
// } AstTypename;

typedef struct {
    struct AstNode *sub_expr;
} AstEnclosed;

typedef struct {
    //Type *type;
    struct AstNode *typename;
    struct AstNode *expr;
} AstCast;

typedef struct {
    struct AstNode *array;
    struct AstNode *index;
} AstArrayIndex;

typedef struct {
    struct AstNode *statement;
} AstDefer;

typedef struct {
    // These overlap, meaning, declarations that are also 
    // statements will be added to statements and decls.
    // Initialized AstBlocks can be casted to an Ast.
    struct Ast *statements;
    struct Ast *decls;
} AstBlock;

typedef struct AstNode {
    union {
        AstProcedure procedure;
        AstStruct struct_;
        AstVar var;
        AstAssignment assignment;
        AstIf if_;
        AstTypedef typedef_;
        AstError error;
        AstImport import;
        AstFor for_;
        AstWhile while_;
        AstReturn return_;
        AstIdent ident;
        AstLiteral literal;
        AstCall function_call;
        AstBinary binary;
        AstUnary unary;
        AstAccessor accessor; /* e.g: thing_instance.member */
        AstEnclosed enclosed; /* e.g: (10 * 3) */
        AstCast cast;
        AstArrayIndex array_index;
        AstDefer defer;
        AstBlock block;

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

AstNode *ast_node(struct Parser *p, AstNodeType tag, Token t);

AstNode *make_error_node(struct Parser *p, Token tok, const char *msg);
AstNode *make_ident_node(struct Parser *p, Token tok);

bool is_assignment(AstBinary);
bool is_binary_comparison(AstBinary);

#endif

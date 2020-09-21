// Code to enforce the language grammar while
// transforming the program text into an
// abstract syntax tree.
#include "headers/common.h"
#include "headers/parser.h"
#include "headers/token.h"
#include "headers/ast.h"
#include "headers/arena.h"
#include "headers/context.h"

// Just for arrlen
#include "headers/stb/stretchy_buffer.h"
#include "headers/stb/stb_ds.h"

#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>

static bool consume(Parser *, TokenType);
static Ast *make_subtree(Parser *);
static void parser_recover_to_declaration(Parser *);
static void parser_recover(Parser *parser, TokenType tt);

static AstCall  parse_call(Context *, Parser *, AstExpr *name);
static AstExpr *parse_expression(Context *, Parser *, int min_precedence);
static AstNode *parse_statement(Context *, Parser *);
static AstNode *parse_proc(Context *ctx, Parser *, bool in_typedef);
static AstNode *parse_var_as_decl(Context *ctx, Parser *, bool top_level, bool is_const);
static AstNode *parse_typename(Context *, Parser *);
static AstNode *parse_top_level(Context *, Parser *);
static bool parse_var(Context *, Parser *, bool top_level, bool is_const, AstVar *out);

// These are implemented at the very bottom of the file
static inline AstExpr *int_literal(Context *, Parser *);
static inline AstExpr *string_literal(Context *, Parser *);
static inline AstExpr *float_literal(Context *, Parser *);
static inline AstExpr *false_literal(Context *, Parser *);
static inline AstExpr *true_literal(Context *, Parser *);
static inline AstExpr *null_literal(Context *, Parser *);

static inline void parser_next(Parser *parser) {
    parser->prev = parser->curr++;
}

typedef struct {
    u8 prec;
    bool left_assoc;
} BinaryOperator;

static inline BinaryOperator get_binary_op_info(TokenType tt) {
    switch (tt) {
    case Token_PLUS:
    case Token_MINUS:
        return (BinaryOperator){.prec=1, .left_assoc=true};

    case Token_STAR:
    case Token_SLASH:
        return (BinaryOperator){.prec=2, .left_assoc=true};

    case Token_DOT:
        return (BinaryOperator){.prec=10, .left_assoc=true};

    case Token_EQUAL:
    case Token_MINUS_EQUAL:
    case Token_PLUS_EQUAL:
    case Token_STAR_EQUAL:
    case Token_SLASH_EQUAL:
        return (BinaryOperator){.prec=1, .left_assoc=false};

    case Token_AMP_AMP:
    case Token_BAR_BAR:
        return (BinaryOperator){.prec=2, .left_assoc=true};

    case Token_GREATER:
    case Token_LESS:
    case Token_BANG_EQUAL:
    case Token_GREATER_EQUAL:
    case Token_LESS_EQUAL:
    case Token_EQUAL_EQUAL:
        return (BinaryOperator){.prec=1, .left_assoc=true};

    // Postfix operators are just binary ops with no right hand side
    case Token_OPEN_PAREN:
        return (BinaryOperator){.prec=9, .left_assoc=true};
    case Token_OPEN_BRACKET:
        return (BinaryOperator){.prec=9, .left_assoc=true};

    default: return (BinaryOperator){.prec=0};
    }
}

static AstExpr *parse_simple_expr(Context *ctx, Parser *parser) {
    switch (parser->curr->type) {
    case Token_OPEN_PAREN: {
        Token start = *parser->curr;
        parser_next(parser);

        AstExpr *sub_expr = parse_expression(ctx, parser, 1);
        if (!sub_expr) {
            parser_recover(parser, Token_SEMI_COLON);
            return NULL;
        }
        consume(parser, Token_CLOSE_PAREN);

        AstParen paren;
        paren.sub_expr = sub_expr;

        return ast_paren(ctx, start, &paren);
    }

    case Token_DOT_DOT: {
        Token dots = *parser->curr;
        parser_next(parser);

        AstVarArgsExpand ve;
        if (!consume(parser, Token_IDENT)) {
            compile_error(ctx, dots, "var-args expansion expects a name");
            return NULL;
        }
        ve.name = make_name(ctx, *parser->prev);
        return ast_var_args_expand(ctx, dots, &ve);
    }

    case Token_CARAT:
    case Token_MINUS:
    case Token_STAR:
    case Token_BANG: {
        Token start = *parser->curr;
        parser_next(parser);

        AstUnary unary;
        unary.expr = parse_expression(ctx, parser, 1);
        unary.op = start.type;

        return ast_unary(ctx, start, &unary);
    }

    case Token_CAST: {
        Token start = *parser->curr;
        parser_next(parser);

        if (!consume(parser, Token_OPEN_PAREN)) {
            compile_error(ctx, *parser->curr, "incorrect cast syntax (missing an opening parenthese)");
            parser_recover(parser, Token_SEMI_COLON);
            return NULL;
        }

        AstCast cast;
        cast.typename = parse_typename(ctx, parser);

        if (!consume(parser, Token_CLOSE_PAREN)) {
            compile_error(ctx, *parser->curr, "incorrect cast syntax (missing a closing parenthese)");
            parser_recover(parser, Token_SEMI_COLON);
            return NULL;
        }

        cast.expr = parse_expression(ctx, parser, 1);

        return ast_cast(ctx, start, &cast);
    }

    case Token_INT_LIT:    return int_literal(ctx, parser);
    case Token_STRING_LIT: return string_literal(ctx, parser);
    case Token_FLOAT_LIT:  return float_literal(ctx, parser);
    case Token_TRUE:       return true_literal(ctx, parser);
    case Token_FALSE:      return false_literal(ctx, parser);
    case Token_NIL:        return null_literal(ctx, parser);

    case Token_IDENT: {
        if (parser->curr[1].type == Token_COLON) {
            compile_error(ctx, *parser->curr, "expected an expression, got a variable declaration");
            return NULL;
        }

        parser_next(parser);
        return ast_name(ctx, *parser->prev);
    }
    }
    compile_error(ctx, *parser->curr, "\"%s\" is not an expression", parser->curr->text);
    return NULL;
}

static AstExpr *parse_postfix_expr(Context *ctx, Parser *parser, AstExpr *left) {
    Token start = *parser->curr;

    switch (parser->curr->type) {
    case Token_OPEN_PAREN: {
        Token start = *parser->curr;
        AstCall call = parse_call(ctx, parser, left);
        return ast_call_expr(ctx, start, &call);
    } break;
    case Token_OPEN_BRACKET: {
        parser_next(parser);
        AstArrayIndex index;
        index.name = left;
        index.index = parse_expression(ctx, parser, 1);
        if (!consume(parser, Token_CLOSE_BRACKET)) {
            compile_error(ctx, *parser->curr, "uneven brackets on array index");
            return NULL;
        }
        return ast_index(ctx, start, &index);
    } break;
    }
    return NULL;
}

static AstExpr *maybe_parse_array_assignment(Context *ctx, Parser *parser, AstExpr *index) {
    if (!(parser->curr->type > Token_ASSIGNMENTS_START && parser->curr->type < Token_ASSIGNMENTS_END)) {
        // Not an assignment
        return index;
    }

    Token op = *parser->curr;
    parser_next(parser);

    AstBinary binary;

    binary.right = parse_expression(ctx, parser, 1);
    binary.left = index;
    binary.op = op.type;
    return ast_binary(ctx, op, &binary);
}

static AstExpr *parse_expression(Context *ctx, Parser *parser, int min_prec) {
    Token start = *parser->curr;

    AstExpr *left = parse_simple_expr(ctx, parser);
    if (!left) {
        parser_recover_to_declaration(parser);
        return NULL;
    }

    while (true) {
        TokenType op = parser->curr->type;
        BinaryOperator info = get_binary_op_info(op);

        if ((op < Token_BINOP_START && op > Token_BINOP_END) || info.prec < min_prec) {
            break;
        }

        int next_min_prec = info.prec;
        if (info.left_assoc) next_min_prec++;

        AstExpr *right = NULL;
        AstExpr *postfix = parse_postfix_expr(ctx, parser, left);
        if (postfix) {
            if (postfix->tag == Expr_CALL && parser->curr->type != Token_DOT)
                return postfix;
            if (postfix->tag == Expr_INDEX && parser->curr->type != Token_DOT)
                return maybe_parse_array_assignment(ctx, parser, postfix);
    
            left = postfix;
            right = postfix;
        } else {
            parser_next(parser);
            right = parse_expression(ctx, parser, next_min_prec);
        }

        AstBinary binary;
        binary.left  = left;
        binary.op    = op;
        binary.right = right;

        AstExpr *new_left = ast_binary(ctx, start, &binary);

        left = new_left;
    }
    return left;
}

static AstCall parse_call(Context *ctx, Parser *parser, AstExpr *name) {
    parser_next(parser); // (

    AstCall call;
    call.name = name;
    call.flags = 0;
    call.params = NULL;

    if (consume(parser, Token_CLOSE_PAREN)) {
        return call;
    }

    Ast *params = make_subtree(parser);
    while (!consume(parser, Token_CLOSE_PAREN) && parser->curr->type != Token_SEMI_COLON && parser->curr->type != Token_EOF) {
        AstExpr *arg = parse_expression(ctx, parser, 1);
        if (!arg) {
            parser_recover(parser, Token_SEMI_COLON);
        }

        // Expect the current token to be a comma to separate each argument
        // and give an error if it is not, unless the next token is a ')'
        //
        // This allows for a dangling comma on the end of calls: e.g
        //
        // a_call(
        //     10,
        //     "an expr",
        // );
        //
        if (!consume(parser, Token_COMMA) && parser->curr->type != Token_EOF) {
            if (parser->curr->type != Token_CLOSE_PAREN) {
                compile_error(ctx, *parser->curr, "expected comma or end of argument list");
                parser_recover_to_declaration(parser);
            }
        }

        ast_add(params, (AstNode *)arg);
    }
    call.params = params;
    return call;
}

// You need to consume the opening token of the brace
// before you call this.
static AstStmt *parse_block(Context *ctx, Parser *parser) {
    Token open = *parser->prev;

    AstBlock *parent = parser->current_scope;

    AstNode *blocknode = ast_node(ctx, Node_BLOCK, open);
    blocknode->as.stmt.tag = Stmt_BLOCK;
    AstBlock *block = &blocknode->as.stmt.as.block;
    block->deferred = make_subtree(parser);
    sh_new_arena(block->symbols);

    parser->current_scope = block;

    Ast *stmts = make_subtree(parser);

    while (!consume(parser, Token_CLOSE_BRACE) && parser->curr->type != Token_EOF) {
        if (parser->curr->type == Token_EOF) {
            //compile_error(ctx, *parser->curr, "unclosed block (missing a '}')");
            return NULL;
        }
        AstNode *statement = parse_statement(ctx, parser);
        if (!statement) {
            parser_recover(parser, Token_CLOSE_BRACE);
            continue;
        }
        consume(parser, Token_SEMI_COLON);
        if (parser->curr->type == Token_EOF) {
            //compile_error(ctx, *parser->curr, "unclosed block (missing a '}')");
            return NULL;
        }
        if (is_decl(statement)) {
            AstDecl *decl = (AstDecl *)statement;
            if (shgeti(block->symbols, decl->name->text) != -1) {
                compile_error(ctx, *parser->curr, "redeclaration of local \"%s\"", decl->name->text);
            }
            shput(block->symbols, decl->name->text, decl);
            consume(parser, Token_SEMI_COLON);
        }

        // Declarations still get added to the statements list so that we can resolve them
        // properly later on.
        ast_add(stmts, statement);
    }

    block->statements = stmts;
    block->parent = parent;

    return &blocknode->as.stmt;
}

static AstStmt *parse_return(Context *ctx, Parser *parser) {
    parser_next(parser);

    Token start = *parser->curr;
    AstReturn ret;
    ret.owning = NULL; // filled in by resolve.ctx

    if (parser->curr->type == Token_SEMI_COLON) {
        ret.expr = NULL;
    } else {
        ret.expr = parse_expression(ctx, parser, 1);
    }

    return ast_return(ctx, start, &ret);
}

static AstStmt *parse_defer(Context *ctx, Parser *parser) {
    
    parser_next(parser);

    Token start = *parser->curr;
    AstDefer defer;

    AstNode *sub_stmt = parse_statement(ctx, parser);
    if (sub_stmt->tag == Node_DEFER) {
        compile_error(ctx, *parser->curr, "you can't defer a defer statement");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    defer.statement = (AstStmt *)sub_stmt;
    return ast_defer(ctx, start, &defer);
}

static AstStmt *parse_struct(Context *ctx, Parser *parser) {
    
    if (!consume(parser, Token_STRUCT)) {
        compile_error(ctx, *parser->curr, "expected a struct declaration");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    Token start = *parser->curr;
    AstStruct s;

    if (!consume(parser, Token_OPEN_BRACE)) {
        compile_error(ctx, *parser->curr, "expected a struct body");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    s.members = parse_block(ctx, parser);

    return ast_struct(ctx, start, &s);
}

static AstStmt *parse_enum(Context *ctx, Parser *parser) {
    if (!consume(parser, Token_ENUM)) {
        compile_error(ctx, *parser->curr, "expected an enum declaration");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    Token start = *parser->curr;
    AstEnum e;
    e.fields = NULL;
    e.base_type = ctx->type_int;

    if (!consume(parser, Token_OPEN_BRACE)) {
        compile_error(ctx, *parser->curr, "expected a list of enum members");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    sh_new_arena(e.fields);

    while (parser->curr->type == Token_IDENT) {
        Token start = *parser->curr;
        AstExpr *expr = parse_expression(ctx, parser, 1);
        Name *decl_name = NULL;
        AstExpr *value = NULL;
        if (expr->tag == Expr_NAME) {
            decl_name = expr->as.name;
            value = expr;
        }
        else if (expr->tag == Expr_BINARY) {
            AstBinary *binary = (AstBinary *)expr;
            if (binary->left->tag != Expr_NAME || binary->op != Token_EQUAL) {
                compile_error(ctx, *parser->curr, "enum declarations can only contain assignments");
                parser_recover(parser, Token_CLOSE_BRACE);
                return NULL;
            }
            decl_name = binary->left->as.name;
            if (binary->right->tag != Expr_NAME && binary->right->tag != Expr_INT) {
                compile_error(ctx, *parser->curr, "enum field can only be an int or other field");
                parser_recover(parser, Token_CLOSE_BRACE);
                return NULL;
            }
            value = binary->right;
        } else {
            compile_error(ctx, *parser->curr, "enum declarations can only contain assignments");
            parser_recover(parser, Token_CLOSE_BRACE);
            return NULL;
        }

        AstVar var;
        var.name = decl_name;
        var.typename = NULL;
        var.value = value;
        var.flags = 0;
        AstDecl *decl = ast_var(ctx, start, decl_name, &var);
        shput(e.fields, decl_name->text, decl);
        consume(parser, Token_COMMA);
    }

    if (!consume(parser, Token_CLOSE_BRACE)) {
        compile_error(ctx, *parser->curr, "expected a close brace");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    return ast_enum(ctx, start, &e);
}

static AstNode *parse_typedef(Context *ctx, Parser *parser) {
    parser_next(parser);

    Token name = *parser->curr;

    if (!consume(parser, Token_IDENT)) {
        compile_error(ctx, name, "typedefs must be given a name");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    AstTypedef td;

    AstExpr *namenode = ast_name(ctx, name);

    if (!consume(parser, Token_COLON)) {
        compile_error(ctx, *parser->curr, "expected a ':' in type definition");
        return NULL;
    }

    u64 type_i = shgeti(ctx->current_module->type_table, name.text); // hopefully -1
    if (type_i != -1) {
        Type *existing = ctx->current_module->type_table[type_i].value;
        // AstDecl *existing_decl = shget(ctx->current_module->symbols, existing->name);
        // assert(existing_decl);
        AstStmt *user = existing->data.user;
        assert(user);
        compile_error_start(ctx, name, "type \"%s\" was declared more than once; first declared here:", name.text);
        compile_error_add_line(ctx, "\t%s:%lu", ctx->current_module->name->text, stmt_tok(user).line);
        compile_error_end();
        return NULL;
    }

    AstNode *decl = NULL;
    Type *type = make_type(0, name.text, 0); // NOTE size is wrong lol

    TypeTable *table = ctx->current_module->type_table;

    switch (parser->curr->type) {
    case Token_STRUCT:
        decl = (AstNode *)parse_struct(ctx, parser);
        type->kind      = Type_STRUCT;
        type->data.user = (AstStmt *)decl;
        break;
    case Token_ENUM:
        decl = (AstNode *)parse_enum(ctx, parser);
        type->kind = Type_ENUM;
        type->data.user = (AstStmt *)decl;
        break;
    case Token_PROC:
        decl = parse_proc(ctx, parser, true);
        break;

    // These should be equivalent to the tokens we check for in parse_typename.
    case Token_RESERVED_TYPE:
        table = ctx->builtin_type_table;
    case Token_IDENT:
    case Token_CARAT:
    case Token_OPEN_BRACKET:
        decl = parse_typename(ctx, parser);
        type->kind = Type_ALIAS;
        type->data.alias_of = decl->as.type;
        break;
    default:
        compile_error(ctx, *parser->curr, "expected a declaration");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    shput(table, name.text, type);
    td.of = decl;
    td.name = namenode;

    AstDecl *n = ast_typedefi(ctx, name, namenode->as.name, &td);

    add_symbol(ctx, n, name.text);

    return (AstNode *)n;
}

static AstNode *parse_typename(Context *ctx, Parser *parser) {
    Token t = *parser->curr;

    AstNode *type_node = ast_node(ctx, Node_TYPENAME, t);

    switch (t.type) {
    case Token_RESERVED_TYPE: {
        parser_next(parser);
        u64 i = shgeti(ctx->builtin_type_table, t.text);
        type_node->as.type = ctx->builtin_type_table[i].value;
        assert(type_node->as.type);
        return type_node;
    } break;
    case Token_STRUCT: {
        type_node->as.type = make_type(Type_ANON_STRUCT, "anonymous struct", 0);
        type_node->as.type->data.user = parse_struct(ctx, parser);
        return type_node;
    } break;
    case Token_IDENT: {
        parser_next(parser);

        if (shgeti(ctx->current_module->type_table, t.text) == -1) { // type doesn't exist (or not appeared in program text yet)
            type_node->as.type = make_type(
                Type_UNRESOLVED,
                t.text,
                0
            );
            return type_node;
        }
        type_node->as.type = shget(ctx->current_module->type_table, t.text);
        return type_node;
    } break;

    //case Token_STAR: {
    case Token_CARAT: {
        parser_next(parser);

        // parse the base type
        // make the pointer type
        // assign the base type to the pointer type

        AstNode *base = parse_typename(ctx, parser);
        if (!base) return NULL;

        type_node->as.type = make_pointer_type(base->as.type);

        return type_node;
    } break;
    case Token_OPEN_BRACKET: {
        parser_next(parser); // [
        if (!consume(parser, Token_CLOSE_BRACKET)) {
            compile_error(ctx, t, "array type has no closing ']'");
            parser_recover_to_declaration(parser);
            return NULL;
        }
        AstNode *base = parse_typename(ctx, parser);
        if (!base) return NULL;

        // TODO array size

        type_node->as.type = make_type(Type_ARRAY, "[]", sizeof(ArrayType));
        type_node->as.type->data.base = base->as.type;

        return type_node;
    } break;
    }

    compile_error(ctx, *parser->prev, "expected a type name, got \"%s\"", parser->curr->text);
    parser_recover(parser, Token_SEMI_COLON);
    return NULL;
}

static AstStmt *parse_if(Context *ctx, Parser *parser) {
    

    Token start = *parser->curr;
    parser_next(parser); // skip keyword

    AstIf _if = (AstIf){0};

    _if.condition = parse_expression(ctx, parser, 1);

    if (consume(parser, Token_THEN)) {
        AstNode *stmt = parse_statement(ctx, parser);
        if (!stmt) {
            parser_recover_to_declaration(parser);
            return NULL;
        }

        if (is_decl(stmt)) {
            compile_error(ctx, stmt->token, "\"then\" clause cannot be a declaration")
            ;
            parser_recover_to_declaration(parser);
            return NULL;
        }

        _if.block_or_stmt = (AstStmt *)stmt;

    } else if (consume(parser, Token_OPEN_BRACE)) {
        _if.block_or_stmt = parse_block(ctx, parser);
    } else {
        compile_error(ctx, *parser->curr, "expected an open brace or 'then' on 'if' statement");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    // Skip the inserted semi-colon after close braces
    consume(parser, Token_SEMI_COLON);

    if (consume(parser, Token_ELSE)) {
        if (parser->curr->type == Token_IF)         _if.other_branch = parse_if(ctx, parser);
        else if (consume(parser, Token_OPEN_BRACE)) _if.other_branch = parse_block(ctx, parser);
        else {
            compile_error(ctx, *parser->curr, "expected either: body of else statement ,, or, else-if");
            parser_recover(parser, Token_CLOSE_BRACE);
            return NULL;
        }
    }

    return ast_if(ctx, start, &_if);
}

static AstStmt *parse_while(Context *ctx, Parser *parser) {
    Token start = *parser->curr;
    parser_next(parser);

    AstWhile w;

    w.condition = parse_expression(ctx, parser, 1);

    if (!consume(parser, Token_OPEN_BRACE)) {
        parser_recover(parser, Token_SEMI_COLON);
        compile_error(ctx, *parser->curr, "expected a block on while loop");
        return NULL;
    }

    w.block = parse_block(ctx, parser);

    return ast_while(ctx, start, &w);
}

static AstNode *parse_var_as_decl(Context *ctx, Parser *parser, bool top_level, bool is_const) {
    Token start = *parser->curr;

    AstVar var;
    if (!parse_var(ctx, parser, top_level, is_const, &var)) {
        return NULL;
    }
    AstDecl *decl = ast_var(ctx, start, var.name, &var);
    if (top_level) {
        decl->flags |= DECL_IS_TOP_LEVEL;
        add_symbol(ctx, decl, decl->name->text);
    }
    return (AstNode *)decl;
}

static bool parse_var(Context *ctx, Parser *parser, bool top_level, bool is_const, AstVar *out) {
    if (parser->curr->type != Token_IDENT) {
        parser_recover(parser, Token_SEMI_COLON);
        compile_error(ctx, *parser->curr, "expected a name on variable declaration");
        return false;
    }

    Token name = *parser->curr;
    parser_next(parser);

    AstVar var;
    var.flags    = 0;
    var.name     = make_name(ctx, name); // probs remove AstDecl names from their actual nodes
    var.typename = ast_node(ctx, Node_TYPENAME, *parser->curr);

    if (is_const) {
        var.flags |= VAR_IS_CONST;
    }

    if (!consume(parser, Token_COLON)) {
        parser_recover(parser, Token_SEMI_COLON);
        compile_error(ctx, name, "expected a colon as type specifer or inference assignment");
        return false;
    }

    // Assignment on inferred decls:
    //     name := value
    if (consume(parser, Token_EQUAL)) {
        AstExpr *value = parse_expression(ctx, parser, 1);
        var.flags |= VAR_IS_INITED;
        if (!value) {
            parser_recover_to_declaration(parser);
            return false;
        }
        var.flags |= VAR_IS_INFERRED;
        var.value = value;
    } else { // explicit type
        AstNode *typename = parse_typename(ctx, parser);
        if (!typename) {
            return false;
        }

        var.typename = typename;

        // Assignment on explicitly-typed decls:
        //     name: Type = value
        if (consume(parser, Token_EQUAL)) {
            AstExpr *value = parse_expression(ctx, parser, 1);
            var.flags |= VAR_IS_INITED;
            if (!value) {
                parser_recover_to_declaration(parser);
                return false;
            }
            var.value = value;
        }
    }

    *out = var;
    return true;
}

static int parse_proc_mod(Context *ctx, Parser *parser, AstExpr **out_maybe_foreign_link_name) {
    
    if (strcmp(parser->curr->text, "foreign")==0) {
        parser_next(parser);
        if (parser->curr->type == Token_STRING_LIT) {
            *out_maybe_foreign_link_name = string_literal(ctx, parser);
        }
        return PROC_IS_FOREIGN;
    }
    return -1;
}

// Parse a procuedure declaration.
static AstNode *parse_proc(Context *ctx, Parser *parser, bool in_typedef) {
    
    Token start = *parser->curr;

    AstProcedure proc;
    proc.params = NULL;
    proc.var_args = NULL;

    // Skip the "proc" keyword.
    if (!consume(parser, Token_PROC)) {
        compile_error(ctx, *parser->curr, "expected procedure declaration");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    Token name = *parser->curr;
    if (!consume(parser, Token_IDENT) && !in_typedef) {
        compile_error(ctx, *parser->curr, "expected name on procedure declaration");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    Name *ident = make_name(ctx, *parser->prev);

    proc.name = ident;

    if (!consume(parser, Token_OPEN_PAREN)) {
        compile_error(ctx, *parser->curr, "expected parameter list (even if it's empty) after procedure name");
        return NULL;
    }

    // If the argument list isn't empty.
    if (!consume(parser, Token_CLOSE_PAREN)) {
        proc.params = make_subtree(parser);
        bool got_var_args = false;
        while (!consume(parser, Token_CLOSE_PAREN)) {
            if (got_var_args) {
                compile_error(ctx, *parser->curr, "var-args argument must be the last one");
                parser_recover(parser, Token_CLOSE_PAREN);
                break;
            }
            if (parser->curr->type == Token_OPEN_BRACE || 
                parser->curr->type == Token_SEMI_COLON ||
                parser->curr->type == Token_EOF        ||
                parser->curr->type == Token_COLON)
            {
                parser_recover_to_declaration(parser);
                compile_error(ctx, start, "unclosed parameter list.");
                return NULL;
            }

            if (parser->curr->type != Token_IDENT && parser->curr->type != Token_CONST) {
                Token t = *parser->curr;
                compile_error(ctx, t, "procedure parameter list must only contain variable declarations");
                parser_recover_to_declaration(parser);
                return NULL;
            }

            bool is_const = consume(parser, Token_CONST);

            // Each parameter is just a variable declaration.
            AstNode *arg = parse_var_as_decl(ctx, parser, /*top_level=*/false, is_const);
            if (!arg) {
                parser_recover(parser, Token_CLOSE_PAREN);
                return NULL;
            }
            AstDecl *decl = &arg->as.decl;
            if (consume(parser, Token_DOT_DOT)) {
                AstVar *var = (AstVar *)decl;
                if (got_var_args) {
                    compile_error(ctx, arg->token, "multiple var-args arguments not allowed");
                    parser_recover(parser, Token_CLOSE_PAREN);
                } else {
                    if (var->typename->as.type->kind != Type_ARRAY) {
                        compile_error(ctx, arg->token, "argument specified as var-args, but it's not an array");
                        parser_recover(parser, Token_CLOSE_PAREN);
                    } else {
                        var->flags |= VAR_IS_VARARGS;
                        proc.var_args = &decl->as.var;
                        got_var_args = true;
                    }
                }
            } else {
                ast_add(proc.params, arg);
            }

            consume(parser, Token_COMMA);
        }
    }

    proc.flags = 0;
    proc.block = NULL;
    proc.params = proc.params;
    proc.foreign_link_name = NULL;

    AstNode *return_type = ast_node(ctx, Node_TYPENAME, *parser->curr);
    return_type->as.type = ctx->type_void;
    if (consume(parser, Token_COLON)) {
        return_type = parse_typename(ctx, parser);
        if (!return_type) {
            parser_recover_to_declaration(parser);
            return NULL;
        }
    }
    proc.return_type = return_type;

    if (parser->curr->type == Token_HASH) {
        while (consume(parser, Token_HASH)) {
            AstExpr *link_name = NULL;
            int mod = parse_proc_mod(ctx, parser, &link_name);
            if (mod == PROC_IS_FOREIGN) proc.foreign_link_name = link_name;
            proc.flags |= mod;
        }
    }

    if (in_typedef) {
        if (parser->curr->type == Token_OPEN_BRACE) {
            compile_error(ctx, *parser->prev, "a typedef'd procedure must not have body");
            parser_recover_to_declaration(parser);
            return NULL;
        }
        return (AstNode *)ast_proc(ctx, start, ident, &proc);
    }

    if (proc.flags & PROC_IS_FOREIGN) {
        if (consume(parser, Token_OPEN_BRACE)) {
            compile_error(ctx, *parser->curr, "procedure marked as foreign should not have a body");
            parser_recover_to_declaration(parser);
            return NULL;
        }
    } else if (!consume(parser, Token_OPEN_BRACE)) {
        compile_error(ctx, *parser->curr, "expected a block on procedure declaration");
        parser_recover_to_declaration(parser);
        return NULL;
    } else {
        proc.block = parse_block(ctx, parser);
    }

    AstDecl *procnode = ast_proc(ctx, start, ident, &proc);
    if (ident == make_namet(ctx, "main")) {
        ctx->decl_for_main = procnode;
    }
    add_symbol(ctx, procnode, name.text);

    return (AstNode *)procnode;
}

static AstStmt *parse_using(Context *ctx, Parser *parser) {
    
    Token start = *parser->curr;
    parser_next(parser);

    if (!consume(parser, Token_IDENT)) {
        compile_error(ctx, *parser->curr, "expected a name after 'using'");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    AstUsing using;
    using.what = make_name(ctx, *parser->prev);
    return ast_using(ctx, start, &using);
}

static AstNode *parse_top_level_directive(Context *ctx, Parser *parser) {
    
    parser_next(parser); // #
    if (parser->curr->type != Token_IDENT && parser->curr->type != Token_IMPORT) {
        compile_error(ctx, *parser->curr, "expected an identifier after #");
        parser_recover_to_declaration(parser);
        return NULL;
    }
    // Name *name = make_name(ctx, *parser->curr);
    //parser_next(parser);
    if (/*name == make_namet(ctx, "import")*/consume(parser, Token_IMPORT)) {
        if (!consume(parser, Token_STRING_LIT)) {
            compile_error(ctx, *parser->curr, "expected a path in the form of a string literal after import");
            return NULL;
        }

        char *path_buffer = malloc(parser->prev->length);
        strncpy(path_buffer, parser->prev->text+1, parser->prev->length);

        AstImport import;
        import.path = path_buffer;

        AstStmt *node = ast_import(ctx, *parser->prev, &import);
        //stbds_arrpush(ctx->current_module->imports, (AstImport *)node);
        return (AstNode *)node;
    }
    return NULL;
}

static AstNode *parse_top_level(Context *ctx, Parser *parser) {
    
    switch (parser->curr->type) {
    case Token_PROC: return parse_proc(ctx, parser, false);
    case Token_IDENT: return parse_var_as_decl(ctx, parser, /*top_level=*/true, /*is_const=*/false);
    case Token_CONST: {
        parser_next(parser);
        return parse_var_as_decl(ctx, parser, true, true);
    }
    case Token_TYPEDEF: return parse_typedef(ctx, parser);
    case Token_HASH: return parse_top_level_directive(ctx, parser);
    }
    compile_error(ctx, *parser->curr, "unknown top level statement");
    parser_recover_to_declaration(parser);
    return NULL;
}

static AstNode *parse_statement(Context *ctx, Parser *parser) {
    
    Token start = *parser->curr;
    switch (start.type) {
    case Token_IF:         return (AstNode *)parse_if(ctx, parser);
    case Token_WHILE:      return (AstNode *)parse_while(ctx, parser);
    case Token_RETURN:     return (AstNode *)parse_return(ctx, parser);
    case Token_DEFER:      return (AstNode *)parse_defer(ctx, parser);
    case Token_OPEN_BRACE: return (AstNode *)parse_block(ctx, parser);
    case Token_USING:      return (AstNode *)parse_using(ctx, parser);
    case Token_CONST: {
        parser_next(parser);
        return parse_var_as_decl(ctx, parser, false, true);
    }

    case Token_IDENT: {
        // Function call
        if (parser->curr[1].type == Token_OPEN_PAREN) {
            AstExpr *call_name = ast_name(ctx, start);
            parser_next(parser);
            AstCall call = parse_call(ctx, parser, call_name);
            return (AstNode *)ast_call_stmt(ctx, start, &call);
        }

        // Variable declaration
        if (parser->curr[1].type == Token_COLON) {
            return parse_var_as_decl(ctx, parser, /*top_level=*/false, /*is_const=*/false);
        }
    }
    }

    AstExpr *expr_left = parse_expression(ctx, parser, 1);
    if (!expr_left) {
        return NULL;
    }
    if (expr_left->tag == Expr_NAME) {
        compile_error(ctx, expr_tok(expr_left), "unexpected identifier");
        return NULL;
    }
    if (expr_left->tag != Expr_BINARY && expr_left->tag != Expr_UNARY) {
        compile_error(ctx, *parser->curr, "only assignments are allowed as statements");
        return NULL;
    }
    return (AstNode *)ast_assignment(ctx, start, expr_left);
}

Ast parse(Context *ctx, Parser *parser) {
    Ast nodes;
    ast_init(&nodes, 100);

    for (;;) {
        arena_clear(&ctx->scratch); // clear the temp allocator
        Token curr = *parser->curr;
        if (curr.type == Token_EOF) break;
        parser->current_scope = NULL;
        AstNode *node = parse_top_level(ctx, parser);
        consume(parser, Token_SEMI_COLON);
        ast_add(&nodes, node);
    }

    return nodes;
}

// If a token of type `tt` is present, eat it.
static bool consume(Parser *parser, TokenType tt) {
    if (parser->curr->type == tt) {
        parser_next(parser);
        return true;
    }
    return false;
}

// Allocates an AST on the heap using an arena so that it may persist between parse states,
// and then freed neatly at the end of parsing.
static Ast *make_subtree(Parser *parser) {
    Ast *ast = arena_alloc(&parser->tree_allocator, sizeof(Ast));
    ast_init(ast, 16);
    return ast;
}

// Used to recover from errors by skipping to a token of type `tt`.
static void parser_recover(Parser *parser, TokenType tt) {
    while (parser->curr->type != tt && parser->curr->type != Token_EOF) {
        parser_next(parser);
    }
}

// NOTE if new top levels are added then they need to be on here
static void parser_recover_to_declaration(Parser *parser) {
    while (parser->curr->type != Token_EOF &&
           parser->curr->type != Token_TYPEDEF &&
           parser->curr->type != Token_PROC) {
        parser_next(parser);
    }
}

void parser_init(Parser *parser, const TokenList *l, const SourceStats *stats) {
    /* Initialize the persistent storage for subtrees */
    /* blocks * 2 because there's two ASTs for each block */
    u64 num_trees = (stats->blocks * 2) + stats->argument_lists;
    arena_init(&parser->tree_allocator, num_trees, sizeof(Ast), 8);

    parser->curr = l->tokens;
    parser->prev = l->tokens;
    parser->current_scope = NULL;
}

// Free all resources held by the parser.
void parser_free(Parser *parser, Ast *a) {
    arena_free(&parser->tree_allocator);
    ast_free(a);
}

static inline AstExpr *int_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
    AstNode *l = ast_node(ctx, Node_INT_LIT, *parser->prev);
    l->as.expr.tag = Expr_INT;
    l->as.expr.as.literal.data.integer = atoi(parser->prev->text);
    return &l->as.expr;
}

static inline AstExpr *string_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
    u64 str_index = shgeti(ctx->string_literal_pool, parser->prev->text);
    if (str_index == -1) {
        AstNode *l = ast_node(ctx, Node_STRING_LIT, *parser->prev);
        l->as.expr.tag = Expr_STRING;
        l->as.expr.as.literal.data.string = parser->prev->text;
        shput(ctx->string_literal_pool, parser->prev->text, (AstLiteral *)l);
        return &l->as.expr;
    }
    return (AstExpr *)ctx->string_literal_pool[str_index].value;
}

static inline AstExpr *float_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
    AstNode *l = ast_node(ctx, Node_FLOAT_LIT, *parser->prev);
    l->as.expr.tag = Expr_FLOAT;
    l->as.expr.as.literal.data.floating = strtod(parser->prev->text, NULL);
    return &l->as.expr;
}

static inline AstExpr *false_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
    AstNode *l = ast_node(ctx, Node_BOOL_LIT, *parser->prev);
    l->as.expr.tag = Expr_BOOL;
    l->as.expr.as.literal.data.boolean = false;
    return &l->as.expr;
}

static inline AstExpr *true_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
    AstNode *l = ast_node(ctx, Node_BOOL_LIT, *parser->prev);
    l->as.expr.tag = Expr_BOOL;
    l->as.expr.as.literal.data.boolean = true;
    return &l->as.expr;
}

static inline AstExpr *null_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
    AstNode *l = ast_node(ctx, Node_NIL, *parser->prev);
    l->as.expr.tag = Expr_NULL;
    return &l->as.expr;
}

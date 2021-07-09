// Code to enforce the language grammar while transforming the program text into an abstract syntax tree.
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
static Ast *make_subtree();
static void parser_recover_to_declaration(Parser *);
static void parser_recover(Parser *parser, TokenType tt);

static AstCall  parse_call(Context *, Parser *, AstExpr *name);
static AstExpr *parse_expression(Context *, Parser *, int min_precedence);
static AstStmt *parse_statement(Context *, Parser *);
static AstExpr *parse_proc(Context *ctx, Parser *, bool in_typedef);
static AstTypename *parse_typename(Context *ctx, Parser *parser);
static AstNode *parse_top_level(Context *, Parser *);
static AstNode *parse_top_level_directive(Context *ctx, Parser *parser);
static AstExpr *parse_struct(Context *ctx, Parser *parser);
static AstDecl *parse_declaration(Context *ctx, Parser *parser);

// These are implemented at the very bottom of the file
static inline AstLiteral *int_literal(Context *, Parser *);
static inline AstLiteral *string_literal(Context *, Parser *);
static inline AstLiteral *float_literal(Context *, Parser *);
static inline AstLiteral *false_literal(Context *, Parser *);
static inline AstLiteral *true_literal(Context *, Parser *);
static inline AstLiteral *null_literal(Context *, Parser *);

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
        return BinaryOperator{1, true};

    case Token_STAR:
    case Token_SLASH:
        return BinaryOperator{2, true};

    case Token_DOT:
        return BinaryOperator{10, true};

    case Token_EQUAL:
    case Token_MINUS_EQUAL:
    case Token_PLUS_EQUAL:
    case Token_STAR_EQUAL:
    case Token_SLASH_EQUAL:
        return BinaryOperator{1, false};

    case Token_AMP_AMP:
    case Token_BAR_BAR:
        return BinaryOperator{2, true};

    case Token_GREATER:
    case Token_LESS:
    case Token_BANG_EQUAL:
    case Token_GREATER_EQUAL:
    case Token_LESS_EQUAL:
    case Token_EQUAL_EQUAL:
        return BinaryOperator{1, true};

    // Postfix operators are just binary ops with no right hand side
    case Token_OPEN_PAREN:
        return BinaryOperator{9, true};
    case Token_OPEN_BRACKET:
        return BinaryOperator{9, true};

    default: return BinaryOperator{.prec=0};
    }
}

static AstExpr *parse_simple_expr(Context *ctx, Parser *parser) {
    switch (parser->curr->type) {
    case Token_PROC: {
        return parse_proc(ctx, parser, false);
    } break;

    /*
    case Token_IDENT:
    case Token_CARAT:
    case Token_OPEN_BRACKET:
    */
    case Token_RESERVED_TYPE: {
        return parse_typename(ctx, parser);
    } break;

    case Token_STRUCT: {
        return parse_struct(ctx, parser);
    } break;

    case Token_OPEN_PAREN: {
        Token start = *parser->curr;
        parser_next(parser);

        AstExpr *sub_expr = parse_expression(ctx, parser, 1);
        if (!consume(parser, Token_CLOSE_PAREN)) {
            compile_error(ctx, start, "unclosed parentheses");
            parser_recover(parser, Token_SEMI_COLON);
            return NULL;
        }

        AstParen paren;
        paren.sub_expr = sub_expr;

        return ast_paren(ctx, start, &paren);
    } break;

    case Token_DOT_DOT: {
        Token dots = *parser->curr;
        parser_next(parser);

        AstVarArgsExpand ve;
        if (!consume(parser, Token_IDENT)) {
            compile_error(ctx, dots, "var-args expansion expects a name");
            return NULL;
        }
        ve.name = ast_name(ctx, *parser->prev);
        return ast_var_args_expand(ctx, dots, &ve);
    } break;

    case Token_HASH: {
        parser_next(parser); // #
        if (parser->curr->type != Token_IDENT && parser->curr->type != Token_IMPORT) {
            compile_error(ctx, *parser->curr, "expected an identifier after #");
            parser_recover_to_declaration(parser);
            return NULL;
        }

        if (consume(parser, Token_IMPORT)) {
            if (!consume(parser, Token_STRING_LIT)) {
                compile_error(ctx, *parser->curr, "expected a path in the form of a string literal after import");
                return NULL;
            }

            char *path_buffer = (char *)malloc(parser->prev->length);
            strncpy(path_buffer, parser->prev->text, parser->prev->length);

            AstImport import;
            import.path = path_buffer;

            return ast_import(ctx, *parser->prev, &import); 
        }

        if (consume(parser, Token_IDENT)) {
            bool is_static;
            if (strcmp(parser->prev->text, "static_library") == 0) {
                is_static = true;
            } else if (strcmp(parser->prev->text, "dynamic_library") == 0) {
                is_static = false;
            } else {
                compile_error(ctx, *parser->curr, "unknown compiler directive");
                parser_recover(parser, Token_SEMI_COLON);
                return NULL;
            }
            if (!consume(parser, Token_STRING_LIT)) {
                compile_error(ctx, *parser->curr, "expected name of library, a string");
                parser_recover(parser, Token_SEMI_COLON);
                return NULL;
            }
            return ast_library(ctx, *parser->prev, parser->prev->text, is_static);
        }

        return NULL;
    } break;

    case Token_CARAT:
    case Token_MINUS:
    case Token_STAR:
    case Token_BANG: {
        Token start = *parser->curr;
        parser_next(parser);

        AstExpr *expr = parse_expression(ctx, parser, 1);
        if (!expr) {
            compile_error(ctx, *parser->curr, "expected an expression after unary operator");
            parser_recover(parser, Token_SEMI_COLON);
            return NULL;
        }

        AstUnary unary;
        unary.expr = expr;
        unary.op = start.type;
        return ast_unary(ctx, start, &unary);
    } break;

    case Token_CAST: {
        Token start = *parser->curr;
        parser_next(parser);

        if (!consume(parser, Token_OPEN_PAREN)) {
            compile_error(ctx, *parser->curr, "incorrect cast syntax (missing an opening parenthese)");
            parser_recover(parser, Token_SEMI_COLON);
            return NULL;
        }

        AstCast cast;
        cast.type = parse_typename(ctx, parser);

        if (!consume(parser, Token_CLOSE_PAREN)) {
            compile_error(ctx, *parser->curr, "incorrect cast syntax (missing a closing parenthese)");
            parser_recover(parser, Token_SEMI_COLON);
            return NULL;
        }

        AstExpr *expr = parse_expression(ctx, parser, 1);
        if (!expr) {
            compile_error(ctx, *parser->curr, "expected an expression on the cast");
            parser_recover(parser, Token_SEMI_COLON);
            return NULL;
        }
        cast.expr = expr;
        return ast_cast(ctx, start, &cast);
    } break;

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
    } break;
    }
    return NULL;
}

static AstExpr *parse_postfix_expr(Context *ctx, Parser *parser, AstExpr *left) {
    Token start = *parser->curr;

    switch (parser->curr->type) {
    case Token_OPEN_PAREN: { // function call
        Token start = *parser->curr;
        AstCall call = parse_call(ctx, parser, left);
        return ast_call_expr(ctx, start, &call);
    } break;

    case Token_OPEN_BRACKET: { // array index
        parser_next(parser);
        AstArrayIndex index;
        index.name = left;
        index.index = parse_expression(ctx, parser, 1);
        if (!index.index) {
            compile_error(ctx, *parser->curr, "invalid expression as array index");
            parser_recover_to_declaration(parser);
            return NULL;
        }
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
    if (!binary.right) {
        compile_error(ctx, *parser->curr, "invalid right-hand-side of assignment");
        parser_recover_to_declaration(parser);
        return NULL;
    }
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

        if (!(op > Token_BINOP_START && op < Token_BINOP_END) || info.prec < min_prec) {
            break;
        }

        int next_min_prec = info.prec;
        if (info.left_assoc) next_min_prec++;

        AstExpr *right = NULL;
        AstExpr *postfix = parse_postfix_expr(ctx, parser, left);
        if (postfix) {
            if (postfix->tag == Node_CALL && parser->curr->type != Token_DOT)
                return postfix;
            if (postfix->tag == Node_INDEX && parser->curr->type != Token_DOT)
                return maybe_parse_array_assignment(ctx, parser, postfix);
    
            left = postfix;
            right = postfix;
        } else {
            parser_next(parser);
            AstExpr *r = parse_expression(ctx, parser, next_min_prec);
            if (!r) {
                compile_error(ctx, *parser->curr, "invalid expression");
            }
            right = r;
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
    Token loc = *parser->curr;
    parser_next(parser); // (

    AstCall call;
    call.name = name;
    call.flags = 0;
    call.params = NULL;

    if (consume(parser, Token_CLOSE_PAREN)) {
        return call;
    }

    Ast *params = make_subtree();
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
                compile_error(ctx, loc, "expected a comma, or the end of the argument list");
                parser_recover(parser, Token_SEMI_COLON);
            }
        }

        ast_add(params, (AstNode *)arg);
    }
    call.params = params;
    return call;
}

// You need to consume the opening token of the brace
// before you call this.
static AstBlock *parse_block(Context *ctx, Parser *parser) {
    Token open = *parser->prev;

    AstBlock *block = (AstBlock *)malloc(sizeof(AstBlock));
	block->tag = Node_BLOCK;
	block->token = open;
    block->deferred = make_subtree(); 
    block->statements = make_subtree();
	block->parent = block_stack_top(ctx->block_stack);

    block_stack_push(&ctx->block_stack, block);

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

        // TODO: redeclaration errors
        /*
        if (is_decl(statement)) {
            AstDecl *decl = (AstDecl *)statement;
            if (table_get(&block->symbols, decl->name->text)) {
                compile_error(ctx, *parser->curr, "redeclaration of local \"%s\"", decl->name->text);
            }
            assert(table_add(&block->symbols, decl->name->text, decl));
            consume(parser, Token_SEMI_COLON);
        }
        */

        // Declarations still get added to the statements list so that we can resolve them
        // properly later on.
        ast_add(block->statements, statement);
    }

    block_stack_pop(&ctx->block_stack);
    return block;
}

static AstStmt *parse_return(Context *ctx, Parser *parser) {
    parser_next(parser);

    Token start = *parser->curr;
    AstReturn ret;
    ret.owning = NULL; // filled in by resolve.c

    if (parser->curr->type == Token_SEMI_COLON) {
        ret.expr = NULL;
    } else {
        AstExpr *expr = parse_expression(ctx, parser, 1);
        if (!expr) {
            compile_error(ctx, *parser->curr, "invalid expression");
            parser_recover_to_declaration(parser);
            return NULL;
        }
        ret.expr = expr;
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

static AstExpr *parse_struct(Context *ctx, Parser *parser) { 
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

static AstExpr *parse_enum(Context *ctx, Parser *parser) {
	return NULL;
	#if 0
    if (!consume(parser, Token_ENUM)) {
        compile_error(ctx, *parser->curr, "expected an enum declaration");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    Token start = *parser->curr;
    AstEnum e;
    e.base_type = ctx->type_int;

    if (!consume(parser, Token_OPEN_BRACE)) {
        compile_error(ctx, *parser->curr, "expected a list of enum members");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    table_init(&e.fields);

    while (parser->curr->type == Token_IDENT) {
        Token start = *parser->curr;
        AstExpr *expr = parse_expression(ctx, parser, 1);
        if (!expr) {
            compile_error(ctx, start, "expected enum field specifier");
            parser_recover_to_declaration(parser);
            return NULL;
        }
        Name *decl_name = NULL;
        AstExpr *value = NULL;
        if (expr->tag == Node_IDENT) {
            decl_name = expr->as.name;
            value = expr;
        }
        else if (expr->tag == Node_BINARY) {
            AstBinary *binary = (AstBinary *)expr;
            if (binary->left->tag != Node_IDENT || binary->op != Token_EQUAL) {
                compile_error(ctx, *parser->curr, "enum declarations can only contain assignments");
                parser_recover(parser, Token_CLOSE_BRACE);
                return NULL;
            }
            decl_name = binary->left->as.name;
            if (binary->right->tag != Node_IDENT && binary->right->tag != Node_INT) {
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

        AstDecl var;
        var.name = decl_name;
        var.given_type = NULL;
        var.expr = value;
        var.flags = 0;
        AstDecl *decl = ast_decl(ctx, start, &var);
        assert(table_add(&e.fields, decl_name->text, (AstNode *)decl));
        consume(parser, Token_COMMA);
    }

    if (!consume(parser, Token_CLOSE_BRACE)) {
        compile_error(ctx, *parser->curr, "expected a close brace");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    return ast_enum(ctx, start, &e);
	#endif
}

static AstTypename *parse_typename(Context *ctx, Parser *parser) {
    Token t = *parser->curr;

	AstTypeDecl *type     = NULL;
    Name        *name     = NULL;
    AstBinary   *selector = NULL;

    switch (t.type) {
    case Token_RESERVED_TYPE: {
        parser_next(parser);
        type = shget(ctx->builtin_types, t.text);
        assert(type);
    } break;

    case Token_IDENT: {
        if (parser->curr[1].type == Token_DOT) {
            AstExpr *maybe_selector = parse_expression(ctx, parser, 1);
            if (!maybe_selector) {
                compile_error(ctx, *parser->curr, "expected a type name");
                parser_recover_to_declaration(parser);
                return NULL;
            }
            assert(maybe_selector->tag == Node_BINARY && ((AstBinary *)maybe_selector)->op == Token_DOT); // TODO: real error
            selector = (AstBinary *)maybe_selector;
        } else {
            parser_next(parser);
            name = make_name_from_token(ctx, t);
            AstDecl *existing = find_decl_from_local_scope_upwards(ctx, name, parser->module);
            if (existing && existing->tag == Node_TYPE_DECL) {
				type = (AstTypeDecl *)existing;
			} else if (parser->in_type_instantiation) {
                type = make_placeholder_type(name);
            }
        }
    } break;

    //case Token_STAR: {
    case Token_CARAT: {
        parser_next(parser);
        parser->in_type_instantiation = true;
        AstExpr *base = parse_typename(ctx, parser);
        parser->in_type_instantiation = false;
        if (!base) return NULL;
        // TODO error
        type = make_pointer_type(base->resolved_type);
    } break;

    case Token_OPEN_BRACKET: {
        parser_next(parser); // [
        if (!consume(parser, Token_CLOSE_BRACKET)) {
            compile_error(ctx, t, "array type has no closing ']'");
            parser_recover_to_declaration(parser);
            return NULL;
        }
        parser->in_type_instantiation = true;
        AstExpr *base = parse_typename(ctx, parser);
        parser->in_type_instantiation = false;
        if (!base) return NULL;
        // TODO error
        // TODO array size
        type = make_array_type(base->resolved_type);
    } break;
    default: return NULL;
    }

    if (!type) assert(name || selector);

    auto ref           = (AstTypename *)malloc(sizeof(AstTypename));
    ref->tag           = Node_TYPENAME;
    ref->token         = t;
    ref->resolved_type = type; // might be null, in which case it will get resolved later.

         if (name)     ref->name = name;
    else if (selector) ref->selector = selector;

    return ref;
}

static AstIf *parse_if(Context *ctx, Parser *parser) {
    Token start = *parser->curr;
    parser_next(parser); // skip keyword

    AstStmt *block_or_stmt = NULL;
    AstStmt *other_branch  = NULL;
    AstExpr *expr          = parse_expression(ctx, parser, 1);

    if (!expr) {
        compile_error(ctx, start, "expected an expression as 'if' condition");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    if (consume(parser, Token_THEN)) {
        AstStmt *stmt = parse_statement(ctx, parser);
        if (!stmt) {
            parser_recover_to_declaration(parser);
            return NULL;
        }

        if (stmt->tag == Node_DECL) {
            compile_error(ctx, stmt->token, "\"then\" clause cannot be a declaration")
            ;
            parser_recover_to_declaration(parser);
            return NULL;
        }
        block_or_stmt = stmt;

    } else if (consume(parser, Token_OPEN_BRACE)) {
        block_or_stmt = parse_block(ctx, parser);

    } else {
        compile_error(ctx, *parser->curr, "expected an open brace or 'then' on 'if' statement");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    // Skip the inserted semi-colon after close braces
    consume(parser, Token_SEMI_COLON);

    if (consume(parser, Token_ELSE)) {
        if (parser->curr->type == Token_IF)         other_branch = parse_if(ctx, parser);
        else if (consume(parser, Token_OPEN_BRACE)) other_branch = parse_block(ctx, parser);
        else if (consume(parser, Token_THEN))       other_branch = parse_statement(ctx, parser);
        else {
            compile_error(ctx, *parser->curr, "expected either: body of else statement, 'then' clause, or else-if");
            parser_recover(parser, Token_CLOSE_BRACE);
            return NULL;
        }

        if (!other_branch) {
            return NULL;
        }
    }

    AstIf *node = (AstIf *)malloc(sizeof(AstIf));
    node->tag = Node_IF;
    node->token = start;
    node->condition = expr;
    node->other_branch = other_branch;
    node->block_or_stmt = block_or_stmt;
    return node;
}

static AstWhile *parse_while(Context *ctx, Parser *parser) {
    Token start = *parser->curr;
    parser_next(parser);

    AstExpr *expr = parse_expression(ctx, parser, 1);
    if (!expr) {
        compile_error(ctx, *parser->curr, "expected an expression for while condition");
        parser_recover(parser, Token_SEMI_COLON);
        return NULL;
    }

    if (!consume(parser, Token_OPEN_BRACE)) {
        parser_recover(parser, Token_SEMI_COLON);
        compile_error(ctx, *parser->curr, "expected a block on while loop");
        return NULL;
    }

    AstBlock *block = parse_block(ctx, parser);

    auto node = (AstWhile *)malloc(sizeof(AstWhile *));
    node->tag = Node_WHILE;
    node->token = start;
    node->condition = expr;
    node->block = block;
    return node;
}

static int parse_proc_mod(Context *ctx, Parser *parser, AstExpr **out_maybe_foreign_link_name, Name **out_library_name) { 
    if (strcmp(parser->curr->text, "foreign")==0) {
        parser_next(parser);
        if (parser->curr->type == Token_STRING_LIT) {
            *out_maybe_foreign_link_name = string_literal(ctx, parser);
        }
        if (consume(parser, Token_COLON)) {
            if (consume(parser, Token_IDENT)) {
                *out_library_name = (Name *)ast_name(ctx, *parser->prev);
            } else {
                compile_error(ctx, *parser->curr, "expected library declaration");
                parser_recover(parser, Token_SEMI_COLON);
                return -1;
            }
        }
        return PROC_IS_FOREIGN;
    }
    return -1;
}

// Parse a procedure declaration.
static AstExpr *parse_proc(Context *ctx, Parser *parser, bool in_typedef) { 
    Token start = *parser->curr;

    AstProcedure proc;
    proc.params = NULL;
	proc.tag = Node_PROCEDURE;
	proc.var_args_index = -1;

    // Skip the "proc" keyword.
    if (!consume(parser, Token_PROC)) {
        compile_error(ctx, *parser->curr, "expected procedure declaration");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    if (!consume(parser, Token_OPEN_PAREN)) {
        compile_error(ctx, *parser->curr, "expected parameter list (even if it's empty) after procedure name");
        return NULL;
    }

	int arg_count = 0;

	proc.params = make_subtree();

    // If the argument list isn't empty.
    if (!consume(parser, Token_CLOSE_PAREN)) {
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

            if (parser->curr->type != Token_IDENT) {
                Token t = *parser->curr;
                compile_error(ctx, t, "procedure parameter list must only contain variable declarations");
                parser_recover_to_declaration(parser);
                return NULL;
            }

            // Each parameter is just a variable declaration.
            AstDecl *arg = parse_declaration(ctx, parser);
            if (!arg) {
                parser_recover(parser, Token_CLOSE_PAREN);
                return NULL;
            }

            if (consume(parser, Token_DOT_DOT)) {
                if (got_var_args) {
                    compile_error(ctx, arg->token, "multiple var-args arguments not allowed");
                    parser_recover(parser, Token_CLOSE_PAREN);
                } else {
					arg->flags |= DECL_IS_VAR_ARGS;
					proc.var_args_index = arg_count;
					got_var_args = true;
                }
            }
			
			ast_add(proc.params, arg);

            consume(parser, Token_COMMA);
			arg_count++;
        }
    }

    proc.flags = 0;
    proc.block = NULL;
    proc.params = proc.params;
    proc.foreign_link_name = NULL;

	auto return_type   = (AstTypename *)malloc(sizeof(AstTypename));
    return_type->tag   = Node_TYPENAME;
	return_type->token = *parser->curr;
	return_type->resolved_type = ctx->type_void;

    if (consume(parser, Token_ARROW)) {
        return_type = parse_typename(ctx, parser);
        if (!return_type) {
            compile_error(ctx, *parser->curr, "expected return type");
            parser_recover_to_declaration(parser);
            return NULL;
        }
    }
    proc.return_type = return_type;

    while (consume(parser, Token_HASH)) {
        AstExpr *link_name = NULL;
        Name    *library_name = NULL;
        int mod = parse_proc_mod(ctx, parser, &link_name, &library_name);
        if (mod == PROC_IS_FOREIGN) {
            proc.foreign_link_name = link_name;
            proc.library_name      = library_name;
        }
        proc.flags |= mod;
    }

    proc_stack_push(&ctx->proc_stack, &proc);

    if (in_typedef) {
        if (parser->curr->type == Token_OPEN_BRACE) {
            compile_error(ctx, *parser->prev, "a typedef'd procedure must not have body");
            parser_recover_to_declaration(parser);
            return NULL;
        }
        proc_stack_pop(&ctx->proc_stack);
        return ast_procedure(ctx, start, &proc);
    }

    if (proc.flags & PROC_IS_FOREIGN) {
        if (consume(parser, Token_OPEN_BRACE)) {
            compile_error(ctx, *parser->curr, "procedure marked as foreign should not have a body");
            parser_recover_to_declaration(parser);
            proc_stack_pop(&ctx->proc_stack);
            return NULL;
        }
    } else if (!consume(parser, Token_OPEN_BRACE)) {
        compile_error(ctx, *parser->curr, "expected a block on procedure declaration");
        parser_recover_to_declaration(parser);
        proc_stack_pop(&ctx->proc_stack);
        return NULL;
    } else {
        proc.block = parse_block(ctx, parser);
    }

    proc_stack_pop(&ctx->proc_stack);

    auto node = (AstProcedure *)malloc(sizeof(AstProcedure));
    *node = proc;
    return node;
}

static AstStmt *parse_statement(Context *ctx, Parser *parser) {
    Token start = *parser->curr;

    switch (start.type) {
    case Token_IDENT: {
        // Function call
        if (parser->curr[1].type == Token_OPEN_PAREN) {
            AstExpr *call = parse_expression(ctx, parser, 1);
			if (!call || call->tag != Node_CALL) {
				assert(false);
				// TODO real error
			}
            return ast_call_stmt(ctx, start, (AstCall *)call);
        }
        // Variable declaration
        if (parser->curr[1].type == Token_COLON) {
            return parse_declaration(ctx, parser);
        }
    } break;
    case Token_IF:         return parse_if(ctx, parser);
    case Token_WHILE:      return parse_while(ctx, parser);
    case Token_RETURN:     return parse_return(ctx, parser);
    case Token_DEFER:      return parse_defer(ctx, parser);
    case Token_OPEN_BRACE: return parse_block(ctx, parser);
    }

    AstExpr *expr_left = parse_expression(ctx, parser, 1);
    if (!expr_left) {
        compile_error(ctx, *parser->curr, "bad statement (expected an assigment or procedure call)");
        parser_recover_to_declaration(parser);
        return NULL;
    }
    if (expr_left->tag == Node_CALL) {
        return ast_call_stmt(ctx, expr_left->token, (AstCall *)expr_left);
    }
    if (expr_left->tag == Node_IDENT) {
        compile_error(ctx, expr_left->token, "unexpected identifier");
        return NULL;
    }
    if (expr_left->tag != Node_BINARY && expr_left->tag != Node_UNARY) {
        compile_error(ctx, *parser->curr, "only assignments are allowed as statements");
        return NULL;
    }
    return ast_assignment(ctx, start, expr_left);
}

static AstNode *parse_top_level_directive(Context *ctx, Parser *parser) { 
    parser_next(parser); // #
    if (parser->curr->type != Token_IDENT && parser->curr->type != Token_IMPORT) {
        compile_error(ctx, *parser->curr, "expected an identifier after #");
        parser_recover_to_declaration(parser);
        return NULL;
    }
    return NULL;
}

static AstDecl *parse_declaration(Context *ctx, Parser *parser) {
    AstDecl spec = AstDecl{};
    spec.name = make_name_from_token(ctx, *parser->curr);

	auto default_type = (AstTypename *)malloc(sizeof(AstTypename));
	default_type->tag = Node_TYPENAME;
	default_type->name = NULL;
	default_type->resolved_type = NULL;
	default_type->selector = NULL;
	default_type->token = Token{};

    spec.given_type = default_type;
    spec.status = Status_UNRESOLVED;
	spec.tag = Node_DECL;

    if (!proc_stack_top(ctx->proc_stack)) spec.flags |= DECL_IS_TOP_LEVEL;

    Token loc = *parser->curr;
	spec.token = loc;

    parser_next(parser);

    if (!consume(parser, Token_COLON)) {
        compile_error(ctx, loc, "expected a ':' after declaration name");
        parser_recover_to_declaration(parser);
        return NULL;
    }

    AstTypename *type = parse_typename(ctx, parser);

    if (parser->curr->type == Token_COLON) {
        spec.flags |= DECL_IS_CONST;
    } else if (parser->curr->type != Token_EQUAL) {
        if (type) {
            spec.given_type = type;
        } else {
            compile_error(ctx, loc, "expected type-name or assignment");
            parser_recover_to_declaration(parser);
            return NULL;
        }
        return ast_decl(ctx, loc, spec);
    }

    if (!type) {
        spec.flags |= DECL_IS_INFERRED;
    } else {
        spec.given_type = type;
    }

    parser_next(parser);

    AstExpr *value = parse_expression(ctx, parser, 1);
    if (!value) {
        compile_error(ctx, loc, "expected an expression on declaration of '%s'", spec.name->text);
        parser_recover_to_declaration(parser);
        return NULL;
    }

    // Type declarations look slighly different in the AST.
    // TODO: this is messy though
    // If the value was an identifier, the name of which is a type variable
    // this needs to become a AstTypeDecl as well
    // I have no idea how to do this.

    if ((value->tag == Node_STRUCT /*|| value->tag == Node_ENUM*/|| value->tag == Node_TYPENAME) && spec.flags & DECL_IS_CONST) {
        auto typedecl = AstTypeDecl{};
		typedecl.tag = Node_TYPE_DECL;
		typedecl.token = loc;
        if (value->tag == Node_STRUCT) {
            typedecl.struct_ = (AstStruct *)value;
            typedecl.expr_type = TypeDecl_STRUCT;
        } else if (value->tag == Node_TYPENAME) {
            typedecl.alias = (AstTypename *)value;
            typedecl.expr_type = TypeDecl_ALIAS;
        }
        typedecl.status = Status_UNRESOLVED;
        typedecl.flags = spec.flags;
        typedecl.size  = 0;
        if (spec.given_type) typedecl.given_type = spec.given_type;
        typedecl.name = spec.name;

        auto node = (AstTypeDecl *)malloc(sizeof(AstTypeDecl));
        *node = typedecl;
		node->tag = Node_TYPE_DECL;
        return node;
    }

    spec.expr = value;
	spec.tag  = Node_DECL;
	spec.token = loc;
	auto node = (AstDecl *)malloc(sizeof(AstDecl));
	*node = spec;
    return node;
}

static AstDecl *parse_using(Context *ctx, Parser *parser) {
    consume(parser, Token_USING);
    return NULL;
}

static AstNode *parse_top_level(Context *ctx, Parser *parser) {
    switch (parser->curr->type) {
    case Token_USING: return (AstNode *)parse_using(ctx, parser);
    case Token_IDENT: return parse_declaration(ctx, parser);
    }
    compile_error(ctx, *parser->curr, "unexpected token");
    parser_recover(parser, Token_EOF);
    return NULL;
}

void parse(Context *ctx, Parser *parser, char *path) {
    for (;;) {
        Token curr = *parser->curr;
        if (curr.type == Token_EOF) break;
        
        AstNode *node = parse_top_level(ctx, parser);
        if (node && node->tag == Node_DECL) {
            AstDecl *decl = (AstDecl *)node;
            if (decl && decl->name == ctx->entry_point_name) {
                ctx->decl_for_main = (AstDecl *)decl;
            }
        }
        consume(parser, Token_SEMI_COLON);
        ast_add(&parser->module->ast, (AstNode *)node);
    }
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
static Ast *make_subtree() {
    Ast *ast = (Ast *)malloc(sizeof(Ast));
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

void parser_init(Parser *parser, Module *module) {
    parser->curr = module->tokens.tokens;
    parser->prev = module->tokens.tokens;
    parser->module = module;
	ast_init(&parser->module->ast, 100);
}

static inline AstLiteral *int_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
    AstLiteral *l = (AstLiteral *)malloc(sizeof(AstLiteral));
	l->tag = Node_INT_LIT;
	l->token = *parser->prev;
	l->integer = atoi(parser->prev->text);
    return l;
}

static inline AstLiteral *string_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
    AstLiteral *l = (AstLiteral *)malloc(sizeof(AstLiteral));
	l->tag = Node_STRING_LIT;
	l->token = *parser->prev;
	l->string = parser->prev->text;
	return l;
}

static inline AstLiteral *float_literal(Context *ctx, Parser *parser) {
	parser_next(parser);
    AstLiteral *l = (AstLiteral *)malloc(sizeof(AstLiteral));
	l->tag = Node_FLOAT_LIT;
	l->token = *parser->prev;
	l->floating = strtod(parser->prev->text, NULL);
	return l;
}

static inline AstLiteral *false_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
	AstLiteral *l = (AstLiteral *)malloc(sizeof(AstLiteral));
	l->tag = Node_BOOL_LIT;
	l->token = *parser->prev;
	l->boolean = false;
    return l;
}

static inline AstLiteral *true_literal(Context *ctx, Parser *parser) {
    parser_next(parser);
	AstLiteral *l = (AstLiteral *)malloc(sizeof(AstLiteral));
	l->tag = Node_BOOL_LIT;
	l->token = *parser->prev;
	l->boolean = true;
    return l;
}

static inline AstLiteral *null_literal(Context *ctx, Parser *parser) {
	parser_next(parser);
	AstLiteral *l = (AstLiteral *)malloc(sizeof(AstLiteral));
	l->tag = Node_BOOL_LIT;
	l->token = *parser->prev;
    return l;
}

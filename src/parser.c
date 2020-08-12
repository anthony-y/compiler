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

static AstNode *parse_call(Context *, AstNode *name);
static AstNode *parse_expression(Context *, int min_precedence);
static AstNode *parse_statement(Context *);
static AstNode *parse_proc(Context *c, bool in_typedef);
static AstNode *parse_var(Context *, bool top_level);
static AstNode *parse_typename(Context *);
static AstNode *parse_top_level(Context *);

// These are implemented at the very bottom of the file
static inline AstNode *int_literal(Parser *);
static inline AstNode *string_literal(Parser *);
static inline AstNode *float_literal(Parser *);
static inline AstNode *false_literal(Parser *);
static inline AstNode *true_literal(Parser *);
static inline AstNode *null_literal(Parser *);

// TODO disallow duplication
static inline void remember_unknown_typename(Context *c, Token t) {
    sb_push(c->deferred_names, t);
}

static inline void parser_next(Parser *p) {
    p->prev = p->curr++;
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

static AstNode *parse_simple_expr(Context *c) {
    Parser *p = &c->parser;
    switch (p->curr->type) {
    case Token_OPEN_PAREN: {
        Token start = *p->curr;
        parser_next(p);

        AstNode *sub_expr = parse_expression(c, 1);
        if (sub_expr->tag == Node_ERROR) {
            parser_recover(p, Token_SEMI_COLON);
            return sub_expr;
        }
        consume(p, Token_CLOSE_PAREN);

        AstNode *enclosing = ast_node(p, Node_ENCLOSED, start);
        enclosing->as.enclosed.sub_expr = sub_expr;

        return enclosing;

        #if 0
        // This will account for if excess parentheses were given BEFORE the expression but not after
        if (p->expr_depth != before) {
            return make_error_node(p, start, "Uneven parentheses.");
        }

        // This will account for excess parens after. Without this, an unknown statement error
        // will happen because the leftover parens were left in the token list.
        if (p->curr->type == Token_CLOSE_PAREN) {
            parser_recover(p, Token_SEMI_COLON);
            return make_error_node(p, start, "Uneven parentheses.");
        }
        #endif
    }

    //case Token_AMPERSAN:
    case Token_CARAT:
    case Token_MINUS:
    case Token_BANG: {
        Token start = *p->curr;
        parser_next(p);

        AstNode *unary = ast_node(p, Node_UNARY, start);
        unary->as.unary.expr = parse_expression(c, 1);
        unary->as.unary.op = start.type;
        return unary;
    }

    case Token_CAST: {
        Token start = *p->curr;
        parser_next(p);

        if (!consume(p, Token_OPEN_PAREN)) {
            parser_recover(p, Token_SEMI_COLON);
            return make_error_node(p, start, "Incorrect cast syntax (missing an opening parenthese)");
        }

        AstNode *cast = ast_node(p, Node_CAST, start);
        cast->as.cast.typename = parse_typename(c);

        if (!consume(p, Token_CLOSE_PAREN)) {
            parser_recover(p, Token_SEMI_COLON);
            return make_error_node(p, start, "Incorrect cast syntax (missing a closing parenthese)");
        }

        AstNode *expr = parse_expression(c, 1);
        cast->as.cast.expr = expr;

        return cast;
    }

    case Token_INT_LIT: return int_literal(p);
    case Token_STRING_LIT: return string_literal(p);
    case Token_FLOAT_LIT: return float_literal(p);
    case Token_TRUE: return true_literal(p);
    case Token_FALSE: return false_literal(p);
    case Token_NIL: return null_literal(p);

    case Token_IDENT: {
        if (p->curr[1].type == Token_COLON) {
            return make_error_node(p, *p->curr, "Expected an expression, got a variable declaration.");
        }

        parser_next(p);
        return make_ident_node(c, *p->prev);
    }
    }
    return make_error_node(p, *p->prev, "Expected an expression.");
}

static AstNode *parse_postfix_expr(Context *c, AstNode *left) {
    Parser *p = &c->parser;
    switch (p->curr->type) {
    case Token_OPEN_PAREN: return parse_call(c, left);
    case Token_OPEN_BRACKET: {
        parser_next(p);
        AstNode *index = ast_node(p, Node_INDEX, *p->prev);
        index->as.array_index.name = left;
        index->as.array_index.index = parse_expression(c, 1);
        if (!consume(p, Token_CLOSE_BRACKET)) {
            return make_error_node(p, *p->curr, "Uneven brackets on array index.");
        }
        return index;
    }
    }
    return NULL;
}

static AstNode *maybe_parse_array_assignment(Context *c, AstNode *index) {
    Parser *p = &c->parser;
    if (p->curr->type < Token_ASSIGNMENTS_START && p->curr->type > Token_ASSIGNMENTS_END) {
        // Not an assignment
        return index;
    }

    Token op = *p->curr;
    parser_next(p);

    AstNode *value = parse_expression(c, 1);
    AstNode *assign = ast_node(p, Node_BINARY, op);
    assign->as.binary.left = index;
    assign->as.binary.op = op.type;
    assign->as.binary.right = value;
    return assign;
}

static AstNode *parse_expression(Context *c, int min_prec) {
    Parser *p = &c->parser;
    Token start = *p->curr;
    AstNode *left = parse_simple_expr(c);
    while (true) {
        TokenType op = p->curr->type;
        BinaryOperator info = get_binary_op_info(op);

        if ((op < Token_BINOP_START && op > Token_BINOP_END) || info.prec < min_prec) {
            break;
        }

        int next_min_prec = info.prec;
        if (info.left_assoc) next_min_prec++;

        AstNode *postfix = parse_postfix_expr(c, left);
        if (postfix && postfix->tag == Node_CALL)
            return postfix;
        if (postfix && postfix->tag == Node_INDEX)
            return maybe_parse_array_assignment(c, postfix);

        parser_next(p);

        AstNode *new_left = ast_node(p, Node_BINARY, start);
        new_left->as.binary.left = left;
        new_left->as.binary.op = op;
        new_left->as.binary.right = parse_expression(c, next_min_prec);;

        left = new_left;
    }
    return left;
}

static AstNode *parse_call(Context *c, AstNode *name) {
    Parser *p = &c->parser;
    parser_next(p); // (

    AstNode *call = ast_node(p, Node_CALL, *p->prev);
    call->as.function_call.name = name;

    if (consume(p, Token_CLOSE_PAREN)) {
        return call;
    }

    Ast *params = make_subtree(p);
    while (!consume(p, Token_CLOSE_PAREN) && p->curr->type != Token_SEMI_COLON) {
        AstNode *arg = parse_expression(c, 1);
        if (arg->tag == Node_ERROR) {
            parser_recover(p, Token_SEMI_COLON);
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
        if (!consume(p, Token_COMMA)) {
            if (p->curr->type != Token_CLOSE_PAREN) {
                arg = make_error_node(p, arg->token, "Expected comma or end of argument list.");
            }
        }

        ast_add(params, arg);
    }
    call->as.function_call.params = params;
    return call;
}

// You need to consume the opening token of the brace
// before you call this.
static AstNode *parse_block(Context *c) {
    Parser *p = &c->parser;

    Token open = *p->prev;

    AstNode *block = ast_node(p, Node_BLOCK, open);
    sh_new_arena(block->as.block.symbols); // TODO leak

    Ast *stmts = make_subtree(p);

    while (!consume(p, Token_CLOSE_BRACE)) {
        AstNode *statement = parse_statement(c);
        consume(p, Token_SEMI_COLON);
        if (p->curr->type == Token_EOF) {
            return make_error_node(p, open, "Unclosed block (missing a '}').");
        }
        Name *name = get_decl_name(statement);
        if (name) {
            shput(block->as.block.symbols, name->text, ((Symbol){.decl=statement, .status=Sym_UNRESOLVED}));
            ast_add(stmts, statement);
            consume(p, Token_SEMI_COLON);
        }
    }

    block->as.block.statements = stmts;

    return block;
}

static AstNode *parse_return(Context *c) {
    Parser *p = &c->parser;
    parser_next(p);

    AstNode *node = ast_node(p, Node_RETURN, *p->curr);
    if (p->curr->type == Token_SEMI_COLON) {
        node->as.return_.expr = NULL;    
        return node;
    }

    node->as.return_.expr = parse_expression(c, 1);
    return node;
}

static AstNode *parse_defer(Context *c) {
    Parser *p = &c->parser;
    AstNode *defer = ast_node(p, Node_DEFER, *p->curr);
    parser_next(p);
    defer->as.defer.statement = parse_statement(c);
    if (defer->as.defer.statement->tag == Node_DEFER)
        return make_error_node(p, *p->prev, "You can't defer a defer statement.");
    return defer;
}

static AstNode *parse_struct(Context *c) {
    Parser *p = &c->parser;
    if (!consume(p, Token_STRUCT)) {
        return make_error_node(p, *p->curr, "Expected a struct declaration.");
    }

    AstNode *n = ast_node(p, Node_STRUCT, *p->prev);

    if (!consume(p, Token_OPEN_BRACE)) {
        return make_error_node(p, *p->curr, "Expected a struct body.");
    }

    n->as.struct_.members = parse_block(c);

    return n;
}

static AstNode *parse_typedef(Context *c) {
    Parser *p = &c->parser;
    parser_next(p);

    Token name = *p->curr;

    if (!consume(p, Token_IDENT)) {
        return make_error_node(p, name, "Typedefs must be given a name.");
    }

    AstNode *n = ast_node(p, Node_TYPEDEF, name);
    n->as.typedef_.name = make_ident_node(c, name);

    if (!consume(p, Token_COLON)) {
        return make_error_node(p, *p->curr, "Expected a ':' in type definition.");
    }

    if (shgeti(c->type_table, name.text) != -1) {
        assert(name.length < 61); // TODO lol
        char *buffer = arena_alloc(&c->scratch, 100);
        sprintf(buffer, "Type \"%s\" was declared more than once.", name.text);
        AstNode *err = make_error_node(p, name, buffer);
        return err;
    }

    AstNode *decl = NULL;
    Type *type = make_type(0, name.text, 0); // NOTE size is wrong lol

    switch (p->curr->type) {
    case Token_STRUCT:
        decl = parse_struct(c);
        type->kind      = Type_STRUCT;
        type->data.user = decl;
        break;
    case Token_PROC:
        decl = parse_proc(c, true);
        break;

    // These should be equivalent to the tokens we check for in parse_typename.
    case Token_IDENT:
    case Token_RESERVED_TYPE:
    case Token_CARAT:
    case Token_OPEN_BRACKET:
        decl = parse_typename(c);
        type->kind = Type_ALIAS;
        type->data.alias_of = decl->as.type;
        break;
    default:
        return make_error_node(p, *p->curr, "Expected a declaration.");
    }

    shput(c->type_table, name.text, type);
    n->as.typedef_.of = decl;

    return n;
}

static AstNode *parse_typename(Context *c) {
    Parser *p = &c->parser;
    Token t = *c->parser.curr;

    AstNode *type_node = ast_node(p, Node_TYPENAME, t);

    switch (t.type) {
    case Token_RESERVED_TYPE: {
        parser_next(p);
        u64 i = shgeti(c->type_table, t.text);
        type_node->as.type = c->type_table[i].value;
        assert(type_node->as.type);
        return type_node;
    } break;
    case Token_STRUCT: {
        type_node->as.type = make_type(Type_STRUCT, "anonymous struct", 0);
        type_node->as.type->data.user = parse_struct(c);
        return type_node;
    } break;
    case Token_IDENT: {
        parser_next(p);

        if (shgeti(c->type_table, t.text) == -1) { // type doesn't exist (or not appeared in program text yet)
            remember_unknown_typename(c, t);
            type_node->as.type = make_type(
                Type_DEFERRED_NAMED,
                t.text,
                0
            );
            return type_node;
        }
        type_node->as.type = shget(c->type_table, t.text);
        return type_node;
    } break;

    //case Token_STAR: {
    case Token_CARAT: {
        parser_next(p);

        // parse the base type
        // make the pointer type
        // assign the base type to the pointer type

        AstNode *base = parse_typename(c);
        if (base->tag == Node_ERROR) return base;

        type_node->as.type = make_pointer_type(base->as.type);

        return type_node;
    } break;
    case Token_OPEN_BRACKET: {
        parser_next(p); // [
        if (!consume(p, Token_CLOSE_BRACKET)) {
            return make_error_node(p, t, "Array type has no closing ']'.");
        }
        AstNode *base = parse_typename(c);
        if (base->tag == Node_ERROR) return base;

        // TODO array size

        type_node->as.type = make_type(Type_ARRAY, "[]", sizeof(ArrayType));
        type_node->as.type->data.base = base->as.type;

        return type_node;
    } break;
    }

    return make_error_node(p, *p->prev, "Expected a type name."); // TODO bad error message, vague
}

static AstNode *parse_if(Context *c) {
    Parser *p = &c->parser;

    Token start = *p->curr;
    parser_next(p); // skip keyword

    AstNode *ast_if = ast_node(p, Node_IF, start);
    ast_if->as.if_.condition = parse_expression(c, 1);

    if (consume(p, Token_THEN)) {
        AstNode *stmt = parse_statement(c);
        if (stmt->tag == Node_ERROR) {
            parser_recover_to_declaration(p);
            return stmt;
        }
        ast_if->as.if_.block_or_stmt = stmt;
    } else if (consume(p, Token_OPEN_BRACE)) {
        ast_if->as.if_.block_or_stmt = parse_block(c);
    } else {
        return make_error_node(p, start, "Expected an open brace or 'then' on 'if' statement.");
    }

    // Skip the inserted semi-colon after close braces
    consume(p, Token_SEMI_COLON);

    AstNode *other_branch = NULL;

    if (consume(p, Token_ELSE)) {
        if (p->curr->type == Token_IF)         other_branch = parse_if(c);
        else if (consume(p, Token_OPEN_BRACE)) other_branch = parse_block(c);
        else {
            AstNode *err = make_error_node(p, *p->prev, "Expected either: body of else statement ,, or, else-if.");
            parser_recover(p, Token_CLOSE_BRACE);
            return err;
        }
    }

    ast_if->as.if_.other_branch = other_branch;

    return ast_if;
}

static AstNode *parse_while(Context *c) {
    Parser *p = &c->parser;
    Token start = *p->curr;
    parser_next(p);

    AstNode *node = ast_node(p, Node_WHILE, start);
    node->as.while_.condition = parse_expression(c, 1);

    if (!consume(p, Token_OPEN_BRACE)) {
        parser_recover(p, Token_SEMI_COLON);
        return make_error_node(p, start, "Expected a block on while loop.");
    }

    node->as.while_.block = parse_block(c);

    return node;
}

static AstNode *parse_var(Context *c, bool top_level) {
    Parser *p = &c->parser;

    if (p->curr->type != Token_IDENT) {
        parser_recover(p, Token_SEMI_COLON);
        return make_error_node(p, *p->curr, "Expected a name on variable declaration.");
    }

    Token name = *p->curr;
    parser_next(p);

    AstNode *node = ast_node(p, Node_VAR, name);
    node->as.var.name = make_ident_node(c, name);
    node->as.var.flags = 0;
    node->as.var.typename = ast_node(p, Node_TYPENAME, *p->curr);

    if (!consume(p, Token_COLON)) {
        parser_recover(p, Token_SEMI_COLON);
        return make_error_node(p, name, "Expected a colon as type specifer or inference assignment.");
    }

    // Assignment on inferred decls:
    //     name := value
    if (consume(p, Token_EQUAL)) {
        AstNode *value = parse_expression(c, 1);
        node->as.var.flags |= VAR_IS_INITED;
        if (value->tag == Node_ERROR) {
            parser_recover_to_declaration(p);
            return value;
        }
        node->as.var.flags |= VAR_IS_INFERRED;
        node->as.var.value = value;

        if (top_level) add_symbol(c, node, name.text); // TODO IMPORTANT ensure no redeclarations
        return node;
    }

    AstNode *typename = parse_typename(c);
    if (typename->tag == Node_ERROR) {
        return typename;
    }
    
    if (typename->as.type->kind == Type_STRUCT) {
        node->as.var.flags |= VAR_TYPE_IS_ANON_STRUCT;
    }

    node->as.var.typename = typename;

    // Assignment on explicitly-typed decls:
    //     name: Type = value
    if (consume(p, Token_EQUAL)) {
        AstNode *value = parse_expression(c, 1);
        node->as.var.flags |= VAR_IS_INITED;
        if (value->tag == Node_ERROR) {
            parser_recover_to_declaration(p);
            return value;
        }
        node->as.var.value = value;
    }

    if (top_level) add_symbol(c, node, name.text);

    return node;
}

// Parse a procuedure declaration.
static AstNode *parse_proc(Context *c, bool in_typedef) {
    Parser *p = &c->parser;
    Token start = *p->curr;

    // Skip the "proc" keyword.
    if (!consume(p, Token_PROC)) {
        return make_error_node(p, *p->curr, "Expected procedure declaration.");
    }

    if (!consume(p, Token_IDENT) && !in_typedef) {
        return make_error_node(p, *p->curr, "Expected name on procedure declaration.");
    }
    Token name = *p->prev;

    AstNode *name_node = make_ident_node(c, *p->prev);
    SymbolTable *params = NULL;
    sh_new_arena(params);

    if (!consume(p, Token_OPEN_PAREN)) {
        return make_error_node(p, *p->curr, "Expected parameter list (even if it's empty) after procedure name.");
    }

    // If the argument list isn't empty.
    if (!consume(p, Token_CLOSE_PAREN)) {
        while (!consume(p, Token_CLOSE_PAREN)) {
            if (p->curr->type == Token_OPEN_BRACE || 
                p->curr->type == Token_SEMI_COLON ||
                p->curr->type == Token_EOF        ||
                p->curr->type == Token_COLON)
            {
                parser_recover_to_declaration(p);
                return make_error_node(p, start, "Unclosed parameter list.");
            }

            if (p->curr->type != Token_IDENT) {
                Token t = *p->curr;
                parser_recover_to_declaration(p);
                return make_error_node(p, t, "Procedure parameter list must only contain variable declarations.");
            }

            // Each parameter is just a variable declaration.
            AstNode *arg = parse_var(c, /*top_level=*/false);
            if (arg->tag == Node_ERROR) {
                parser_recover(p, Token_CLOSE_PAREN);
                return arg;
            }
            if (arg->as.var.flags & VAR_IS_INITED) {
                arg = make_error_node(p, arg->token, "This language does not (yet) have default call values; procedure parameters must be uninitialized.");
                parser_recover(p, Token_CLOSE_PAREN);
            }

            Symbol sym = (Symbol){.decl=arg, .status=Sym_UNRESOLVED};
            shput(params, ((AstVar *)arg)->name, sym);

            consume(p, Token_COMMA);
        }
    }

    AstNode *node = ast_node(p, Node_PROCEDURE, start);
    node->as.procedure.block = NULL;
    node->as.procedure.name = name_node;
    node->as.procedure.params = params;

    AstNode *return_type = ast_node(p, Node_TYPENAME, *p->curr);
    return_type->as.type = c->type_void;
    if (consume(p, Token_COLON)) {
        return_type = parse_typename(c);
        if (return_type->tag == Node_ERROR) {
            parser_recover_to_declaration(p);
            return return_type;
        }
    }
    node->as.procedure.return_type = return_type;

    if (in_typedef) {
        if (p->curr->type == Token_OPEN_BRACE) {
            parser_recover_to_declaration(p);
            return make_error_node(p, *p->prev, "A typedef'd procedure must not have body.");
        }
        return node;
    }

    if (!consume(p, Token_OPEN_BRACE)) {
        AstNode *err = make_error_node(p, *p->curr, "Expected a block on procedure declaration.");
        parser_recover_to_declaration(p);
        return err;
    }

    node->as.procedure.block = parse_block(c);

    add_symbol(c, node, name.text);

    return node;
}

static AstNode *parse_top_level(Context *c) {
    Parser *p = &c->parser;
    switch (p->curr->type) {
    case Token_PROC: return parse_proc(c, false);
    case Token_IDENT: return parse_var(c, /*top_level=*/true);
    case Token_TYPEDEF: return parse_typedef(c);
    }
    AstNode *err = make_error_node(p, *p->curr, "Unknown top level statement.");
    parser_recover_to_declaration(p);
    return err;
}

static AstNode *parse_statement(Context *c) {
    Parser *p = &c->parser;
    switch (p->curr->type) {
    case Token_IF: return parse_if(c);
    case Token_WHILE: return parse_while(c);
    case Token_RETURN: return parse_return(c);
    case Token_DEFER: return parse_defer(c);
    case Token_OPEN_BRACE: return parse_block(c);

    case Token_IDENT: {
        // Function call
        if (p->curr[1].type == Token_OPEN_PAREN) {
            AstNode *call_name = make_ident_node(c, *p->curr);
            parser_next(p);
            return parse_call(c, call_name);
        }

        // Variable declaration
        if (p->curr[1].type == Token_COLON) {
            return parse_var(c, /*top_level=*/false);
        }

        AstNode *expr = parse_expression(c, 1);
        if (expr->tag == Node_ERROR) {
            parser_recover_to_declaration(p);
            return expr;
        }

        // TODO make expr return sooner if it's valid

        if (expr->tag == Node_IDENT) {
            AstNode *err = make_error_node(p, *p->curr, "Stray identifier.");
            parser_recover(p, Token_SEMI_COLON);
            return err;
        }

        // Basically: if it's not some sort of assignment
        if (expr->tag != Node_BINARY || !is_assignment(expr->as.binary)) {
            AstNode *err = make_error_node(p, *p->curr, "Only assignments are allowed as statements.");
            parser_recover(p, Token_SEMI_COLON);
            return err;
        }

        return expr;
    }
    }
    AstNode *err = make_error_node(p, *p->curr, "Failed to parse statement.");
    parser_recover_to_declaration(p);
    return err;
}

Ast parse(Context *c) {
    Parser *p = &c->parser;

    Ast nodes;
    ast_init(&nodes, 100);

    for (;;) {
        arena_clear(&c->scratch); // clear the temp allocator
        Token curr = *p->curr;
        if (curr.type == Token_EOF) break;
        AstNode *node = parse_top_level(c);
        consume(p, Token_SEMI_COLON);
        ast_add(&nodes, node);
    }

    return nodes;
}

// If a token of type `tt` is present, eat it.
static bool consume(Parser *p, TokenType tt) {
    if (p->curr->type == tt) {
        parser_next(p);
        return true;
    }
    return false;
}

// Allocates an AST on the heap using an arena so that it may persist between parse states,
// and then freed neatly at the end of parsing.
static Ast *make_subtree(Parser *p) {
    Ast *ast = arena_alloc(&p->tree_allocator, sizeof(Ast));
    ast_init(ast, 16);
    return ast;
}

// Used to recover from errors by skipping to a token of type `tt`.
void parser_recover(Parser *p, TokenType tt) {
    while (p->curr->type != tt && p->curr->type != Token_EOF) {
        parser_next(p);
    }
}

// TODO if new top levels are added then they need to be on here
static void parser_recover_to_declaration(Parser *p) {
    while (p->curr->type != Token_EOF &&
           p->curr->type != Token_TYPEDEF &&
           p->curr->type != Token_PROC) {
        parser_next(p);
    }
}

void parser_init(Parser *p, const TokenList *l, const SourceStats *stats) {
    // Rough estimate of how many AST nodes we'll need.
    const u64 n = (u64)(l->len * 0.9);

    /* Initialize the allocator for the AST nodes */
    arena_init(&p->node_allocator, n, sizeof(AstNode), 8);

    /* Initialize the persistent storage for subtrees */
    u64 num_trees = (stats->blocks) + stats->argument_lists;
    arena_init(&p->tree_allocator, num_trees, sizeof(Ast), 8);

    arena_init(&p->error_msg_allocator, 10240, sizeof(char), 1);

    p->curr = l->tokens;
    p->prev = l->tokens;
    p->node_count = 0;
}

// Free all resources held by the parser.
void parser_free(Parser *p, Ast *a) {
    //
    // TODO LEAK FOR SOME REASON
    //
    // u8 *end = p->tree_allocator.block+p->tree_allocator.pos;
    // for (u8 *data = p->tree_allocator.block; data != end; data++) {
    //     ast_free((Ast *)data);
    // }

    arena_free(&p->tree_allocator);
    arena_free(&p->node_allocator);
    arena_free(&p->error_msg_allocator);
    ast_free(a);
}

static inline AstNode *int_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_INT_LIT, *p->prev);
    l->as.literal.data.integer = atoi(p->prev->text);
    return l;
}

static inline AstNode *string_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_STRING_LIT, *p->prev);
    l->as.literal.data.string = p->prev->text;
    return l;
}

static inline AstNode *float_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_FLOAT_LIT, *p->prev);
    l->as.literal.data.floating = strtod(p->prev->text, NULL);
    return l;
}

static inline AstNode *false_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_BOOL_LIT, *p->prev);
    l->as.literal.data.boolean = false;
    return l;
}

static inline AstNode *true_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_BOOL_LIT, *p->prev);
    l->as.literal.data.boolean = true;
    return l;
}

static inline AstNode *null_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_NIL, *p->prev);
    return l;
}

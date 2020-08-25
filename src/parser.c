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

static AstCall  parse_call(Context *, AstExpr *name);
static AstExpr *parse_expression(Context *, int min_precedence);
static AstNode *parse_statement(Context *);
static AstNode *parse_proc(Context *c, bool in_typedef);
static AstNode *parse_var_as_decl(Context *c, bool top_level);
static AstNode *parse_typename(Context *);
static AstNode *parse_top_level(Context *);
static bool parse_var(Context *, bool top_level, AstVar *out);

// These are implemented at the very bottom of the file
static inline AstExpr *int_literal(Parser *);
static inline AstExpr *string_literal(Context *, Parser *);
static inline AstExpr *float_literal(Parser *);
static inline AstExpr *false_literal(Parser *);
static inline AstExpr *true_literal(Parser *);
static inline AstExpr *null_literal(Parser *);

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

static AstExpr *parse_simple_expr(Context *c) {
    Parser *p = &c->parser;
    switch (p->curr->type) {
    case Token_OPEN_PAREN: {
        Token start = *p->curr;
        parser_next(p);

        AstExpr *sub_expr = parse_expression(c, 1);
        if (!sub_expr) {
            parser_recover(p, Token_SEMI_COLON);
            return NULL;
        }
        consume(p, Token_CLOSE_PAREN);

        AstParen paren;
        paren.sub_expr = sub_expr;

        return ast_paren(p, start, &paren);

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

    case Token_CARAT:
    case Token_MINUS:
    case Token_STAR:
    case Token_BANG: {
        Token start = *p->curr;
        parser_next(p);

        AstUnary unary;
        unary.expr = parse_expression(c, 1);
        unary.op = start.type;

        return ast_unary(p, start, &unary);
    }

    case Token_CAST: {
        Token start = *p->curr;
        parser_next(p);

        if (!consume(p, Token_OPEN_PAREN)) {
            compile_error(c, *p->curr, "incorrect cast syntax (missing an opening parenthese)");
            parser_recover(p, Token_SEMI_COLON);
            return NULL;
        }

        AstCast cast;
        cast.typename = parse_typename(c);

        if (!consume(p, Token_CLOSE_PAREN)) {
            compile_error(c, *p->curr, "incorrect cast syntax (missing a closing parenthese)");
            parser_recover(p, Token_SEMI_COLON);
            return NULL;
        }

        cast.expr = parse_expression(c, 1);

        return ast_cast(p, start, &cast);
    }

    case Token_INT_LIT:    return int_literal(p);
    case Token_STRING_LIT: return string_literal(c, p);
    case Token_FLOAT_LIT:  return float_literal(p);
    case Token_TRUE:       return true_literal(p);
    case Token_FALSE:      return false_literal(p);
    case Token_NIL:        return null_literal(p);

    case Token_IDENT: {
        if (p->curr[1].type == Token_COLON) {
            compile_error(c, *p->curr, "expected an expression, got a variable declaration");
            return NULL;
        }

        parser_next(p);
        return ast_name(c, *p->prev);
    }
    }
    compile_error(c, *p->curr, "\"%s\" is not an expression", p->curr->text);
    return NULL;
}

static AstExpr *parse_postfix_expr(Context *c, AstExpr *left) {
    Parser *p = &c->parser;
    Token start = *p->curr;

    switch (p->curr->type) {
    case Token_OPEN_PAREN: {
        Token start = *p->curr;
        AstCall call = parse_call(c, left);
        return ast_call_expr(p, start, &call);
    } break;
    case Token_OPEN_BRACKET: {
        parser_next(p);
        AstArrayIndex index;
        index.name = left;
        index.index = parse_expression(c, 1);
        if (!consume(p, Token_CLOSE_BRACKET)) {
            compile_error(c, *p->curr, "uneven brackets on array index");
            return NULL;
        }
        return ast_index(p, start, &index);
    } break;
    }
    return NULL;
}

static AstExpr *maybe_parse_array_assignment(Context *c, AstExpr *index) {
    Parser *p = &c->parser;
    if (!(p->curr->type > Token_ASSIGNMENTS_START && p->curr->type < Token_ASSIGNMENTS_END)) {
        // Not an assignment
        return index;
    }

    Token op = *p->curr;
    parser_next(p);

    AstBinary binary;

    binary.right = parse_expression(c, 1);
    binary.left = index;
    binary.op = op.type;
    return ast_binary(p, op, &binary); // TODO stmt assigns are broken
}

static AstExpr *parse_expression(Context *c, int min_prec) {
    Parser *p = &c->parser;
    Token start = *p->curr;

    AstExpr *left = parse_simple_expr(c);
    if (!left) return NULL;

    while (true) {
        TokenType op = p->curr->type;
        BinaryOperator info = get_binary_op_info(op);

        if ((op < Token_BINOP_START && op > Token_BINOP_END) || info.prec < min_prec) {
            break;
        }

        int next_min_prec = info.prec;
        if (info.left_assoc) next_min_prec++;

        AstExpr *right = NULL;

        AstExpr *postfix = parse_postfix_expr(c, left);
        if (postfix) {
            if (postfix->tag == Expr_CALL)
                return postfix;
            if (postfix->tag == Expr_INDEX && p->curr->type != Token_DOT)
                return maybe_parse_array_assignment(c, postfix);

            left = postfix;
            right = postfix;
        } else {
            parser_next(p);
            right = parse_expression(c, next_min_prec);
        }

        AstBinary binary;
        binary.left  = left;
        binary.op    = op;
        binary.right = right;

        AstExpr *new_left = ast_binary(p, start, &binary);

        left = new_left;
    }
    return left;
}

static AstCall parse_call(Context *c, AstExpr *name) {
    Parser *p = &c->parser;
    parser_next(p); // (

    AstCall call;
    call.name = name;
    call.flags = 0;
    call.params = NULL;

    if (consume(p, Token_CLOSE_PAREN)) {
        return call;
    }

    Ast *params = make_subtree(p);
    while (!consume(p, Token_CLOSE_PAREN) && p->curr->type != Token_SEMI_COLON) {
        AstExpr *arg = parse_expression(c, 1);
        if (!arg) {
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
                compile_error(c, *p->curr, "expected comma or end of argument list");
                parser_recover(p, Token_CLOSE_PAREN);
            }
        }

        ast_add(params, (AstNode *)arg);
    }
    call.params = params;
    return call;
}

// You need to consume the opening token of the brace
// before you call this.
static AstStmt *parse_block(Context *c) {
    Parser *p = &c->parser;
    Token open = *p->prev;

    AstBlock *parent = p->current_scope;

    AstNode *blocknode = ast_node(p, Node_BLOCK, open);
    blocknode->as.stmt.tag = Stmt_BLOCK;
    AstBlock *block = &blocknode->as.stmt.as.block;
    sh_new_arena(block->symbols); // TODO leak

    p->current_scope = block;

    Ast *stmts = make_subtree(p);

    while (!consume(p, Token_CLOSE_BRACE) && p->curr->type != Token_EOF) {
        if (p->curr->type == Token_EOF) {
            compile_error(c, *p->curr, "unclosed block (missing a '}')");
            return NULL;
        }
        AstNode *statement = parse_statement(c);
        if (!statement) {
            parser_recover(p, Token_CLOSE_BRACE);
            continue;
        }
        consume(p, Token_SEMI_COLON);
        if (p->curr->type == Token_EOF) {
            compile_error(c, *p->curr, "unclosed block (missing a '}')");
            return NULL;
        }
        if (is_decl(statement)) {
            AstDecl *decl = (AstDecl *)statement;
            if (shgeti(block->symbols, decl->name->text) != -1) {
                compile_error(c, *p->curr, "redeclaration of local \"%s\"", decl->name->text);
            }
            shput(block->symbols, decl->name->text, decl);
            consume(p, Token_SEMI_COLON);
        }

        // Declarations still get added to the statements list so that we can resolve them
        // properly later on.
        ast_add(stmts, statement);
    }

    block->statements = stmts;
    block->parent = parent;

    return &blocknode->as.stmt;
}

static AstStmt *parse_return(Context *c) {
    Parser *p = &c->parser;
    parser_next(p);

    Token start = *p->curr;
    AstReturn ret;

    if (p->curr->type == Token_SEMI_COLON) {
        ret.expr = NULL;
    } else {
        ret.expr = parse_expression(c, 1);
    }

    return ast_return(p, start, &ret);
}

static AstStmt *parse_defer(Context *c) {
    Parser *p = &c->parser;
    parser_next(p);

    Token start = *p->curr;
    AstDefer defer;

    AstNode *sub_stmt = parse_statement(c);
    if (sub_stmt->tag == Node_DEFER) {
        compile_error(c, *p->curr, "you can't defer a defer statement");
        parser_recover_to_declaration(p);
        return NULL;
    }

    defer.statement = (AstStmt *)sub_stmt;
    return ast_defer(p, start, &defer);
}

static AstStmt *parse_struct(Context *c) { // TODO SHOULD STRUCTS BE STATEMENTS?
    Parser *p = &c->parser;
    if (!consume(p, Token_STRUCT)) {
        compile_error(c, *p->curr, "expected a struct declaration");
        parser_recover_to_declaration(p);
        return NULL;
    }

    Token start = *p->curr;
    AstStruct s;

    if (!consume(p, Token_OPEN_BRACE)) {
        compile_error(c, *p->curr, "expected a struct body");
        parser_recover_to_declaration(p);
        return NULL;
    }

    s.members = parse_block(c);

    return ast_struct(p, start, &s);
}

static AstNode *parse_typedef(Context *c) {
    Parser *p = &c->parser;
    parser_next(p);

    Token name = *p->curr;

    if (!consume(p, Token_IDENT)) {
        compile_error(c, name, "typedefs must be given a name");
        parser_recover_to_declaration(p);
        return NULL;
    }

    AstTypedef td;

    AstExpr *namenode = ast_name(c, name);

    if (!consume(p, Token_COLON)) {
        compile_error(c, *p->curr, "expected a ':' in type definition");
        return NULL;
    }

    u64 type_i = shgeti(c->type_table, name.text); // hopefully -1
    if (type_i != -1) {
        Type *existing = c->type_table[type_i].value;
        AstDecl *existing_decl = shget(c->symbol_table, existing->name);
        assert(existing_decl);
        compile_error_start(c, name, "type \"%s\" was declared more than once; first declared here:");
        compile_error_add_line(c, "\t%s:%lu", c->current_file_path, decl_tok(existing_decl).line); // TODO file path may be diffrent
        return NULL;
    }

    AstNode *decl = NULL;
    Type *type = make_type(0, name.text, 0); // NOTE size is wrong lol

    switch (p->curr->type) {
    case Token_STRUCT:
        decl = (AstNode *)parse_struct(c);
        type->kind      = Type_STRUCT;
        type->data.user = (AstStmt *)decl;
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
        compile_error(c, *p->curr, "expected a declaration");
        parser_recover_to_declaration(p);
        return NULL;
    }

    shput(c->type_table, name.text, type);
    td.of = decl;
    td.name = namenode;

    AstDecl *n = ast_typedefi(p, name, namenode->as.name, &td);

    add_symbol(c, n, name.text);

    return (AstNode *)n;
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
        type_node->as.type = make_type(Type_ANON_STRUCT, "anonymous struct", 0);
        type_node->as.type->data.user = parse_struct(c);
        return type_node;
    } break;
    case Token_IDENT: {
        parser_next(p);

        if (shgeti(c->type_table, t.text) == -1) { // type doesn't exist (or not appeared in program text yet)
            type_node->as.type = make_type(
                Type_UNRESOLVED,
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
        if (!base) return NULL;

        type_node->as.type = make_pointer_type(base->as.type);

        return type_node;
    } break;
    case Token_OPEN_BRACKET: {
        parser_next(p); // [
        if (!consume(p, Token_CLOSE_BRACKET)) {
            compile_error(c, t, "array type has no closing ']'");
            parser_recover_to_declaration(p);
            return NULL;
        }
        AstNode *base = parse_typename(c);
        if (!base) return NULL;

        // TODO array size

        type_node->as.type = make_type(Type_ARRAY, "[]", sizeof(ArrayType));
        type_node->as.type->data.base = base->as.type;

        return type_node;
    } break;
    }

    compile_error(c, *p->prev, "expected a type name, got \"%s\"", p->curr->text);
    parser_recover(p, Token_SEMI_COLON);
    return NULL;
}

static AstStmt *parse_if(Context *c) {
    Parser *p = &c->parser;

    Token start = *p->curr;
    parser_next(p); // skip keyword

    AstIf _if = (AstIf){0};

    _if.condition = parse_expression(c, 1);

    if (consume(p, Token_THEN)) {
        AstNode *stmt = parse_statement(c);
        if (!stmt) {
            parser_recover_to_declaration(p);
            return NULL;
        }

        if (is_decl(stmt)) {
            compile_error(c, stmt->token, "\"then\" clause cannot be a declaration")
            ;
            parser_recover_to_declaration(p);
            return NULL;
        }

        _if.block_or_stmt = (AstStmt *)stmt;

    } else if (consume(p, Token_OPEN_BRACE)) {
        _if.block_or_stmt = parse_block(c);
    } else {
        compile_error(c, *p->curr, "expected an open brace or 'then' on 'if' statement");
        parser_recover_to_declaration(p);
        return NULL;
    }

    // Skip the inserted semi-colon after close braces
    consume(p, Token_SEMI_COLON);

    if (consume(p, Token_ELSE)) {
        if (p->curr->type == Token_IF)         _if.other_branch = parse_if(c);
        else if (consume(p, Token_OPEN_BRACE)) _if.other_branch = parse_block(c);
        else {
            compile_error(c, *p->curr, "expected either: body of else statement ,, or, else-if");
            parser_recover(p, Token_CLOSE_BRACE);
            return NULL;
        }
    }

    return ast_if(p, start, &_if);
}

static AstStmt *parse_while(Context *c) {
    Parser *p = &c->parser;
    Token start = *p->curr;
    parser_next(p);

    AstWhile w;

    w.condition = parse_expression(c, 1);

    if (!consume(p, Token_OPEN_BRACE)) {
        parser_recover(p, Token_SEMI_COLON);
        compile_error(c, *p->curr, "expected a block on while loop");
        return NULL;
    }

    w.block = parse_block(c);

    return ast_while(p, start, &w);
}

static AstNode *parse_var_as_decl(Context *c, bool top_level) {
    Parser *p = &c->parser;
    Token start = *p->curr;

    AstVar var;
    if (!parse_var(c, top_level, &var)) {
        return NULL;
    }
    AstDecl *decl = ast_var(p, start, var.name->as.expr.as.name, &var);
    if (top_level) {
        decl->flags |= DECL_IS_TOP_LEVEL;
        add_symbol(c, decl, decl->name->text);
    }
    return (AstNode *)decl;
}

static bool parse_var(Context *c, bool top_level, AstVar *out) {
    Parser *p = &c->parser;

    if (p->curr->type != Token_IDENT) {
        parser_recover(p, Token_SEMI_COLON);
        compile_error(c, *p->curr, "expected a name on variable declaration");
        return false;
    }

    Token name = *p->curr;
    parser_next(p);

    AstVar var;
    var.flags    = 0;
    var.name     = (AstNode *)ast_name(c, name); // probs remove AstDecl names from their actual nodes
    var.typename = ast_node(p, Node_TYPENAME, *p->curr);

    if (!consume(p, Token_COLON)) {
        parser_recover(p, Token_SEMI_COLON);
        compile_error(c, name, "expected a colon as type specifer or inference assignment");
        return false;
    }

    // Assignment on inferred decls:
    //     name := value
    if (consume(p, Token_EQUAL)) {
        AstExpr *value = parse_expression(c, 1);
        var.flags |= VAR_IS_INITED;
        if (!value) {
            parser_recover_to_declaration(p);
            return false;
        }
        var.flags |= VAR_IS_INFERRED;
        var.value = value;
    } else { // explicit type
        AstNode *typename = parse_typename(c);
        if (!typename) {
            return false;
        }

        var.typename = typename;

        // Assignment on explicitly-typed decls:
        //     name: Type = value
        if (consume(p, Token_EQUAL)) {
            AstExpr *value = parse_expression(c, 1);
            var.flags |= VAR_IS_INITED;
            if (!value) {
                parser_recover_to_declaration(p);
                return false;
            }
            var.value = value;
        }
    }

    *out = var;
    return true;
}

static int parse_proc_mod(Context *c) {
    Parser *p = &c->parser;
    if (!consume(p, Token_HASH)) return -1;
    if (strcmp(p->curr->text, "foreign")==0) {
        parser_next(p);
        return PROC_MOD_FOREIGN;
    }
    return -2; // TODO make this an actual node
}

// Parse a procuedure declaration.
static AstNode *parse_proc(Context *c, bool in_typedef) {
    Parser *p = &c->parser;
    Token start = *p->curr;

    AstProcedure proc;
    proc.params = NULL;

    // Skip the "proc" keyword.
    if (!consume(p, Token_PROC)) {
        compile_error(c, *p->curr, "expected procedure declaration");
        parser_recover_to_declaration(p);
        return NULL;
    }

    Token name = *p->curr;
    if (!consume(p, Token_IDENT) && !in_typedef) {
        compile_error(c, *p->curr, "expected name on procedure declaration");
        parser_recover_to_declaration(p);
        return NULL;
    }

    AstExpr *name_node = ast_name(c, *p->prev);
    proc.name = (AstNode *)name_node;

    Name *ident = make_name(c, *p->prev);

    if (!consume(p, Token_OPEN_PAREN)) {
        compile_error(c, *p->curr, "expected parameter list (even if it's empty) after procedure name");
        return NULL;
    }

    // If the argument list isn't empty.
    if (!consume(p, Token_CLOSE_PAREN)) {
        sh_new_arena(proc.params);
        while (!consume(p, Token_CLOSE_PAREN)) {
            if (p->curr->type == Token_OPEN_BRACE || 
                p->curr->type == Token_SEMI_COLON ||
                p->curr->type == Token_EOF        ||
                p->curr->type == Token_COLON)
            {
                parser_recover_to_declaration(p);
                compile_error(c, start, "unclosed parameter list.");
                return NULL;
            }

            if (p->curr->type != Token_IDENT) {
                Token t = *p->curr;
                compile_error(c, t, "procedure parameter list must only contain variable declarations");
                parser_recover_to_declaration(p);
                return NULL;
            }

            // Each parameter is just a variable declaration.
            AstNode *arg = parse_var_as_decl(c, /*top_level=*/false);
            if (!arg) {
                parser_recover(p, Token_CLOSE_PAREN);
                return NULL;
            }
            AstDecl *decl = &arg->as.decl;
            if (decl->as.var.flags & VAR_IS_INITED) {
                compile_error(c, arg->token, "this language does not (yet) have default call values; procedure parameters must be uninitialized");
                parser_recover(p, Token_CLOSE_PAREN);
            }

            shput(proc.params, decl->name->text, (AstDecl *)arg);

            consume(p, Token_COMMA);
        }
    }

    proc.flags = 0;
    proc.block = NULL;
    proc.params = proc.params;

    AstNode *return_type = ast_node(p, Node_TYPENAME, *p->curr);
    return_type->as.type = c->type_void;
    if (consume(p, Token_COLON)) {
        return_type = parse_typename(c);
        if (!return_type) {
            parser_recover_to_declaration(p);
            return NULL;
        }
    }
    proc.return_type = return_type;

    if (in_typedef) {
        if (p->curr->type == Token_OPEN_BRACE) {
            compile_error(c, *p->prev, "a typedef'd procedure must not have body");
            parser_recover_to_declaration(p);
            return NULL;
        }
        return (AstNode *)ast_proc(p, start, ident, &proc);
    }

    int modifier = parse_proc_mod(c);
    if (modifier == -2) { // TODO cleanup
        compile_error(c, *p->curr, "unknown procedure modifier");
        return NULL;
    }

    if (modifier == PROC_MOD_FOREIGN) {
        if (consume(p, Token_OPEN_BRACE)) {
            compile_error(c, *p->curr, "procedure marked as foreign should not have a body");
            parser_recover_to_declaration(p);
            return NULL;
        }
        proc.flags |= modifier;
    } else if (!consume(p, Token_OPEN_BRACE)) {
        compile_error(c, *p->curr, "expected a block on procedure declaration");
        parser_recover_to_declaration(p);
        return NULL;
    } else {
        proc.block = parse_block(c);
    }

    AstDecl *procnode = ast_proc(p, start, ident, &proc);
    if (ident == make_namet(c, "main")) {
        c->decl_for_main = procnode;
    }
    add_symbol(c, procnode, name.text);

    return (AstNode *)procnode;
}

static AstNode *parse_top_level(Context *c) {
    Parser *p = &c->parser;
    switch (p->curr->type) {
    case Token_PROC: return parse_proc(c, false);
    case Token_IDENT: return parse_var_as_decl(c, /*top_level=*/true);
    case Token_TYPEDEF: return parse_typedef(c);
    }
    compile_error(c, *p->curr, "unknown top level statement");
    parser_recover_to_declaration(p);
    return NULL;
}

static AstNode *parse_statement(Context *c) {
    Parser *p = &c->parser;
    Token start = *p->curr;
    switch (start.type) {
    case Token_IF:         return (AstNode *)parse_if(c);
    case Token_WHILE:      return (AstNode *)parse_while(c);
    case Token_RETURN:     return (AstNode *)parse_return(c);
    case Token_DEFER:      return (AstNode *)parse_defer(c);
    case Token_OPEN_BRACE: return (AstNode *)parse_block(c);

    case Token_IDENT: {
        // Function call
        if (p->curr[1].type == Token_OPEN_PAREN) {
            AstExpr *call_name = ast_name(c, start);
            parser_next(p);
            AstCall call = parse_call(c, call_name);
            return (AstNode *)ast_call_stmt(p, start, &call);
        }

        // Variable declaration
        if (p->curr[1].type == Token_COLON) {
            return parse_var_as_decl(c, false);
        }
    }
    }

    AstExpr *expr_left = parse_expression(c, 1);
    if (!expr_left) {
        return NULL;
    }
    if (expr_left->tag == Expr_NAME) {
        compile_error(c, expr_tok(expr_left), "unexpected identifier");
        return NULL;
    }
    if (expr_left->tag != Expr_BINARY && expr_left->tag != Expr_UNARY) {
        compile_error(c, *p->curr, "only assignments are allowed as statements");
        return NULL;
    }
    return (AstNode *)ast_assignment(p, start, expr_left);
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

    p->curr = l->tokens;
    p->prev = l->tokens;
    p->node_count = 0;
    p->current_scope = NULL;
}

// Free all resources held by the parser.
void parser_free(Parser *p, Ast *a) {
    arena_free(&p->tree_allocator);
    arena_free(&p->node_allocator);
    ast_free(a);
}

static inline AstExpr *int_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_INT_LIT, *p->prev);
    l->as.expr.tag = Expr_INT;
    l->as.expr.as.literal.data.integer = atoi(p->prev->text);
    return &l->as.expr;
}

static inline AstExpr *string_literal(Context *ctx, Parser *p) {
    parser_next(p);
    u64 str_index = shgeti(ctx->string_literal_pool, p->prev->text);
    if (str_index == -1) {
        AstNode *l = ast_node(p, Node_STRING_LIT, *p->prev);
        l->as.expr.tag = Expr_STRING;
        l->as.expr.as.literal.data.string = p->prev->text;
        shput(ctx->string_literal_pool, p->prev->text, (AstLiteral *)l);
        return &l->as.expr;
    }
    return (AstExpr *)ctx->string_literal_pool[str_index].value;
}

static inline AstExpr *float_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_FLOAT_LIT, *p->prev);
    l->as.expr.tag = Expr_FLOAT;
    l->as.expr.as.literal.data.floating = strtod(p->prev->text, NULL);
    return &l->as.expr;
}

static inline AstExpr *false_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_BOOL_LIT, *p->prev);
    l->as.expr.tag = Expr_BOOL;
    l->as.expr.as.literal.data.boolean = false;
    return &l->as.expr;
}

static inline AstExpr *true_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_BOOL_LIT, *p->prev);
    l->as.expr.tag = Expr_BOOL;
    l->as.expr.as.literal.data.boolean = true;
    return &l->as.expr;
}

static inline AstExpr *null_literal(Parser *p) {
    parser_next(p);
    AstNode *l = ast_node(p, Node_NIL, *p->prev);
    l->as.expr.tag = Expr_NULL;
    return &l->as.expr;
}

#include "headers/common.h"
#include "headers/ast.h"
#include "headers/type.h"
#include "headers/context.h"
#include "headers/passes.h"

#include "headers/stb/stb_ds.h"

#include <assert.h>

static void resolve_procedure(AstDecl *procsym, Context *ctx);
static Type *resolve_var(AstDecl *varsym, Context *ctx);
static Type *resolve_selector(Context *ctx, AstBinary *accessor);
static Type *resolve_expression(AstExpr *expr, Context *ctx);
static Type *resolve_type(Context *ctx, Type *type, bool cyclic_allowed);
static void resolve_block(Context *ctx, AstBlock *block);

// stbds arrays
static AstProcedure **proc_stack = NULL;
static AstBlock **scope_stack = NULL;

static void resolve_assignment_expr(AstExpr *assign, Context *ctx) {
    if (assign->tag == Expr_UNARY) {
        AstUnary *unary = (AstUnary *)assign;
        resolve_assignment_expr(unary->expr, ctx);
    } else if (assign->tag == Expr_BINARY) {
        AstBinary *bin = (AstBinary *)assign;
        if (!is_assignment(*bin)) {
            compile_error(ctx, expr_tok(assign), "expected assignment"); // TODO maybe bad
            return;
        }
        resolve_expression(bin->left, ctx);
        resolve_expression(bin->right, ctx);
    } else {
        compile_error(ctx, expr_tok(assign), "expected assignment");
    }
}

// Resolves the dependencies of an assignment statement,
static void resolve_assignment(AstNode *ass, Context *ctx) {
    assert(ass->tag == Node_ASSIGN);
    AstStmt *stmt = (AstStmt *)ass;
    resolve_assignment_expr(&stmt->as.assign, ctx);
}

static AstProcedure *resolve_call(AstNode *callnode, Context *ctx) {
    Token tok = callnode->token;
    AstCall *call = (AstCall *)callnode;
    char *str_name = call->name->as.name->text;
    u64 symbol_index = shgeti(ctx->symbol_table, str_name);
    if (symbol_index == -1) {
        compile_error(ctx, tok, "call to undeclared procedure \"%s\"", str_name);
        return NULL;
    }
    if (call->params) for (int i = 0; i < call->params->len; i++) {
        AstExpr *arg = (AstExpr *)call->params->nodes[i];
        resolve_expression(arg, ctx);
    }
    AstDecl *hopefully_proc = ctx->symbol_table[symbol_index].value;
    if (hopefully_proc->tag != Decl_PROC) {
        compile_error(ctx, tok, "attempted to call \"%s\", but it's not a procedure", str_name);
        return NULL;
    }
    if (hopefully_proc->status == Status_UNRESOLVED) {
        resolve_procedure(hopefully_proc, ctx);
    }
    AstProcedure *calling = (AstProcedure *)hopefully_proc;
    call->calling = calling;
    return calling;
}

static Type *resolve_expression_1(AstExpr *expr, Context *ctx) {
    Token t = expr_tok(expr);
    switch (expr->tag) {
    case Expr_STRING: return ctx->type_string;
    case Expr_INT:    return ctx->type_int;
    case Expr_BOOL:   return ctx->type_bool;
    case Expr_NULL:   return make_pointer_type(NULL);

    case Expr_CALL: {
        AstProcedure *resolved = resolve_call((AstNode *)expr, ctx);
        if (!resolved) return NULL;
        return resolved->return_type->as.type;
    } break;
    case Expr_UNARY: {
        AstUnary *unary = (AstUnary *)expr;
        Type *expr_type = resolve_expression(unary->expr, ctx);
        if (unary->op == Token_STAR) {
            if (expr_type->kind != Type_POINTER) {
                compile_error(ctx, t, "expected pointer operand to dereference");
                return NULL;
            }
            return expr_type->data.base;
        }
        return expr_type;
    } break;
    case Expr_NAME: {
        Name *name = expr->as.name;

        AstProcedure *in = stbds_arrlast(proc_stack);
        AstBlock *scope = stbds_arrlast(scope_stack);

        AstDecl *var = lookup_local(ctx, in, name, scope);
        if (!var) {
            compile_error(ctx, t, "undeclared identifier \"%s\"", name->text);
            return NULL;
        }
        assert(var->name == name);
        if (var->status == Status_UNRESOLVED) {
            Type *resolved_type = resolve_var(var, ctx);
            return resolved_type;
        }
        if (var->status == Status_RESOLVING) {
            compile_error(ctx, t, "initial instantiation of variable \"%s\" mentions itself", name->text);
            return NULL;
        }
        if (var->tag != Decl_VAR) {
            compile_error(ctx, t, "\"%s\" was used like a variable, but it isn't one", name->text);
            return NULL;
        }
        name->resolved_decl = var;
        return var->as.var.typename->as.type;
    } break;
    case Expr_CAST: {
        AstCast *cast = (AstCast *)expr;
        resolve_expression(cast->expr, ctx);
        return cast->typename->as.type;
    } break;
    case Expr_INDEX: {
        AstArrayIndex *index = (AstArrayIndex *)expr;
        Type *resolved_name = resolve_expression(index->name, ctx);
        assert(resolved_name->data.base);
        resolve_expression(index->index, ctx);
        return resolved_name->data.base;
    } break;

    case Expr_BINARY: {
        AstBinary *bin = (AstBinary *)expr;
        if (bin->op == Token_DOT) {
            return resolve_selector(ctx, bin);
        }
        if (is_assignment(*bin)) {
            resolve_assignment_expr(expr, ctx);
            if (bin->op != Token_EQUAL) {
                return ctx->type_int;
            }
        }
        Type *lhs = resolve_expression(bin->left, ctx);
        resolve_expression(bin->right, ctx);
        if (is_binary_comparison(*bin)) {
            return ctx->type_bool;
        }
        return lhs;
    } break;

    case Expr_PAREN: {
        return resolve_expression(((AstParen *)expr)->sub_expr, ctx);
    } break;
    }
    assert(false);
    return NULL;
}

static Type *resolve_expression(AstExpr *expr, Context *ctx) {
    if (!expr) return NULL;
    Type *re = resolve_expression_1(expr, ctx);
    expr->resolved_type = re;
    return re;
}

void resolve_struct(AstStruct *def, Context *ctx) {
    SymbolTable *table = def->members->as.block.symbols;
    u64 len = shlenu(table);
    for (int i = 0; i < len; i++) {
        AstDecl *field = table[i].value;
        if (field->tag != Decl_VAR) {
            compile_error(ctx, decl_tok(field), "only variable declarations are valid inside a struct body"); // this is semantic checking but we have to do it here otherwise it's a nightmare
            continue;
        }
        resolve_var(field, ctx);
    }
}

// Resolves an unresolved type to it's "real" type
static Type *resolve_type(Context *ctx, Type *type, bool cyclic_allowed) {
    Token t = ((AstNode *)type)->token; // TODO doesnt work lol

    if (type->kind == Type_PRIMITIVE) return type;

    //
    // Pointers and arrays might have unresolved types in their sub-types.
    // We resolve those and wrap them back up each time.
    //
    if (type->kind == Type_POINTER)
        return make_pointer_type(resolve_type(ctx, type->data.base, true)); // ... cyclic_allowed should only be passed as true here and below

    if (type->kind == Type_ARRAY)
        return make_array_type(resolve_type(ctx, type->data.base, true));

    if (type->kind != Type_UNRESOLVED)
        return type;

    // Unresolved types (that is, types which are used before they are defined) are returned
    // as placeholder types in the type table. If they then go on to be defined in the source code,
    // they will be placed into the type table.
    // Here, we lookup the type in the type table. If it is not found, then it was never declared.
    // ...
    u64 i = shgeti(ctx->type_table, type->name);
    if (i == -1) {
        compile_error(ctx, t, "undeclared type \"%s\"", type->name);
        return NULL;
    }

    // ... if it was we store it here
    Type *real_type = ctx->type_table[i].value;

    // Next we'll look up the actual declaration of the type.
    u64 type_i = shgeti(ctx->symbol_table, type->name);
    AstDecl *sym = ctx->symbol_table[type_i].value;

    // If the declaration is already resolving...
    if (sym->status == Status_RESOLVING) {
        if (cyclic_allowed) {
            sym->status = Status_RESOLVED;
            return real_type;
        }
        compile_error(ctx, decl_tok(sym), "type definition for \"%s\" directly mentions itself", type->name);
        return NULL;
    }

    sym->status = Status_RESOLVING; // the above if statements works because of this

    if (sym->tag != Decl_TYPEDEF) {
        compile_error(ctx, t, "\"%s\" is not a type", type->name);
        return NULL;
    }

    AstTypedef *my_typedef = (AstTypedef *)sym;
    if (my_typedef->of->tag == Node_STRUCT) {
        resolve_struct(&my_typedef->of->as.stmt.as._struct, ctx);
    }

    sym->status = Status_RESOLVED;

    return real_type;
}

// Resolves the dependencies of a selector and returns the type of the field it selects.
static Type *resolve_selector(Context *ctx, AstBinary *accessor) { // TODO rename to resolve_selector
    assert(accessor->op == Token_DOT);

    Name *rhs = accessor->right->as.name;
    Type *lhs_type = resolve_expression(accessor->left, ctx);

    if (!lhs_type) return NULL; // resolve_expression will have already errored, so we can just exit

    if (lhs_type->kind == Type_UNRESOLVED) {
        lhs_type = resolve_type(ctx, lhs_type, false);
        assert(lhs_type);
    }

    // In English: throw an error if the type of the left hand side is not either:
    //  - a struct or anonymous struct
    //  - a pointer, the base type of which is a struct or anonymous struct
    if (lhs_type->kind != Type_STRUCT && lhs_type->kind != Type_ANON_STRUCT &&
        (lhs_type->kind != Type_POINTER || (lhs_type->data.base->kind != Type_STRUCT && lhs_type->data.base->kind != Type_ANON_STRUCT))) {
        compile_error(ctx, expr_tok(accessor->left), "attempt to access member in non-struct value");
        return NULL;
    }

    // If it was a pointer, unwrap it, but only by one "level",
    // selectors shouldn't be able to reach into far-down structs in pointers.
    if (lhs_type->kind == Type_POINTER) {
        lhs_type = lhs_type->data.base;
    }

    AstStruct *struct_def = &lhs_type->data.user->as._struct;
    AstDecl *field = lookup_struct_field(struct_def, rhs);
    if (!field) {
        compile_error(ctx, expr_tok(accessor->right), "no such field as \"%s\" in struct field access", rhs->text);
        return NULL;
    }
    assert(field->tag == Decl_VAR); // should have been checked by now
    return field->as.var.typename->as.type;
}

// Resolves the dependencies and type of a variable declaration,
// and apply type inference if needed.
static Type *resolve_var(AstDecl *decl, Context *ctx) {
    AstVar *var = (AstVar *)decl;
    Type **specified_type = &var->typename->as.type;
    if (decl->status == Status_RESOLVED)
        return *specified_type;

    if (!(var->flags & VAR_IS_INFERRED)) {
        if (var->typename->as.type->kind == Type_ANON_STRUCT) {
            resolve_struct(&var->typename->as.stmt.as._struct, ctx);
        } else {
            *specified_type = resolve_type(ctx, *specified_type, false);
        }
    } 

    if (!(var->flags & VAR_IS_INITED)) {
        decl->status = Status_RESOLVED;
        return *specified_type;
    }

    decl->status = Status_RESOLVING;
    Type *inferred_type = resolve_expression(var->value, ctx);
    decl->status = Status_RESOLVED;

    if (!inferred_type) return NULL;
    if (var->flags & VAR_IS_INFERRED) {
        *specified_type = inferred_type; // type inference!
    }
    return inferred_type;
}

static void resolve_statement(Context *ctx, AstNode *stmt) {
    switch (stmt->tag) {
    case Node_BLOCK:
        resolve_block(ctx, (AstBlock *)stmt);
        break;
    case Node_CALL:
        resolve_call(stmt, ctx);
        break;
    case Node_ASSIGN:
        resolve_assignment(stmt, ctx);
        break;
    case Node_RETURN:
        resolve_expression(((AstReturn *)stmt)->expr, ctx);
        break;
    case Node_IF: {
        AstIf *iff = (AstIf *)stmt;
        resolve_expression(iff->condition, ctx);
        resolve_statement(ctx, (AstNode *)iff->block_or_stmt);
        if (iff->other_branch) resolve_statement(ctx, (AstNode *)iff->other_branch);
    } break;
    case Node_WHILE: {
        AstWhile *w = (AstWhile *)stmt;
        resolve_expression(w->condition, ctx);
        resolve_block(ctx, (AstBlock *)w->block);
    } break;
    default: printf("resolve_statement tag %d\n", stmt->tag);
    }
}

static void resolve_block(Context *ctx, AstBlock *block) {
    stbds_arrpush(scope_stack, block);
    u64 decls_len = shlenu(block->symbols);
    for (int i = 0; i < decls_len; i++) {
        AstDecl *sym = block->symbols[i].value;
        switch (sym->tag) {
        case Decl_VAR:
            resolve_var(sym, ctx);
            break;
        // TODO
        }
    }

    for (int i = 0; i < block->statements->len; i++) {
        AstNode *stmt = block->statements->nodes[i];
        resolve_statement(ctx, stmt);
    }
    stbds_arrpop(scope_stack);
}

static void resolve_procedure(AstDecl *procsym, Context *ctx) {
    if (procsym->status == Status_RESOLVED) return;
    procsym->status = Status_RESOLVING;

    AstProcedure *proc = (AstProcedure *)procsym;

    if (proc->params) {
        u64 num_params = shlenu(proc->params);
        for (int i = 0; i < num_params; i++) {
            resolve_var(proc->params[i].value, ctx);
        }
    }

    Type **return_type = &proc->return_type->as.type;
    *return_type = resolve_type(ctx, *return_type, false);

    if (proc->flags & PROC_MOD_FOREIGN) {
        procsym->status = Status_RESOLVED;
        return;
    }

    stbds_arrpush(proc_stack, proc); // push the new scope

    AstBlock *block = (AstBlock *)proc->block;
    resolve_block(ctx, block);
    procsym->status = Status_RESOLVED;
    stbds_arrpop(proc_stack); // pop the scope
}

void resolve_top_level(Context *ctx) {
    u64 len = shlenu(ctx->symbol_table);
    for (int i = 0; i < len; i++) {
        AstDecl *decl = ctx->symbol_table[i].value;
        decl->status = Status_RESOLVING;
        switch (decl->tag) {
        case Decl_PROC:
            resolve_procedure(decl, ctx);
            break;
        case Decl_VAR:
            resolve_var(decl, ctx);
            break;
        case Decl_TYPEDEF: {
            AstTypedef *def = (AstTypedef *)decl;
            if (def->of->tag == Node_STRUCT) {
                resolve_struct((AstStruct *)def->of, ctx);
            }
        } break;
        }
    }
}

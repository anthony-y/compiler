#include "headers/common.h"
#include "headers/ast.h"
#include "headers/type.h"
#include "headers/context.h"
#include "headers/passes.h"

#include "headers/stb/stb_ds.h"

#include <assert.h>

void resolve_procedure(Symbol *procsym, Context *ctx);
Type *resolve_var(Symbol *varsym, Context *ctx);
Type *resolve_accessor(Context *ctx, AstBinary *accessor);

static AstProcedure **scope_stack = NULL; // stbds array

AstProcedure *resolve_call(AstNode *callnode, Context *ctx) {
    AstCall *call = (AstCall *)callnode;
    char *str_name = call->name->as.ident->text;
    u64 symbol_index = shgeti(ctx->symbol_table, str_name);
    if (symbol_index == -1) {
        compile_error(ctx, callnode->token, "Call to undeclared procedure \"%s\"", str_name);
        return NULL;
    }
    Symbol *hopefully_proc = &ctx->symbol_table[symbol_index].value;
    if (hopefully_proc->decl->tag != Node_PROCEDURE) {
        compile_error(ctx, callnode->token, "Attempted to call \"%s\", but it's not a procedure", str_name);
        return NULL;
    }
    if (hopefully_proc->status == Sym_UNRESOLVED) {
        resolve_procedure(hopefully_proc, ctx);
    }
    AstProcedure *calling = (AstProcedure *)hopefully_proc->decl;
    call->calling = calling;
    return calling;
}

Type *resolve_expression(AstNode *expr, Context *ctx) {
    if (is_literal(expr)) {
        switch (expr->tag) {
        case Node_STRING_LIT: return ctx->type_string;
        case Node_INT_LIT: return ctx->type_int;
        case Node_BOOL_LIT: return ctx->type_bool;
        case Node_NIL: return make_pointer_type(NULL);
        // TODO array and float
        }
    }
    switch (expr->tag) {
    case Node_CALL: {
        AstProcedure *resolved = resolve_call(expr, ctx);
        if (!resolved) return NULL;
        return resolved->return_type->as.type;
    } break;
    case Node_UNARY: {
        AstUnary *unary = (AstUnary *)expr;
        Type *expr_type = resolve_expression(unary->expr, ctx);
        return expr_type;
    } break;
    case Node_IDENT: {
        Name *name = expr->as.ident;
        AstProcedure *in = stbds_arrlast(scope_stack);
        Symbol *var = lookup_local(ctx, in, name);
        if (!var) {
            compile_error(ctx, expr->token, "Undeclared identifier \"%s\"", name->text);
            return NULL;
        }
        if (var->status == Sym_UNRESOLVED) {
            Type *resolved_type = resolve_var(var, ctx);
            return resolved_type;
        }
        if (var->status == Sym_RESOLVING) {
            compile_error(ctx, expr->token, "Initial instantiation of variable \"%s\" mentions itself", name->text);
            return NULL;
        }
        if (var->decl->tag != Node_VAR) {
            compile_error(ctx, expr->token, "\"%s\" used like a variable, but it isn't one");
            return NULL;
        }
        AstVar *vardecl = (AstVar *)var->decl;
        return vardecl->typename->as.type;
    } break;
    case Node_CAST: {
        return ((AstCast *)expr)->typename->as.type;
    } break;
    case Node_INDEX: {
        AstArrayIndex *index = (AstArrayIndex *)expr;
        Type *resolved_name = resolve_expression(index->name, ctx);
        // TODO expr
        return resolved_name;
    } break;
    case Node_BINARY: {
        AstBinary *bin = (AstBinary *)expr;
        if (bin->op == Token_DOT) {
            return resolve_accessor(ctx, bin);
        }
        Type *lhs = resolve_expression(bin->left, ctx);
        resolve_expression(bin->right, ctx);
        return lhs;
    } break;
    case Node_ENCLOSED: {
        return resolve_expression(((AstEnclosed *)expr)->sub_expr, ctx);
    } break;
    }
    assert(false);
    return NULL;
}

// TODO somehow make types symbols lmao
Type *resolve_type(Context *ctx, Type *type, Token t) {
    assert(type->kind == Type_DEFERRED_NAMED);

    u64 i = shgeti(ctx->type_table, type->name);
    if (i == -1) {
        compile_error(ctx, t, "Undeclared type \"%s\"", type->name);
        return NULL;
    }

    Type *real_type = ctx->type_table[i].value;

    u64 type_i = shgeti(ctx->symbol_table, type->name);
    Symbol *sym = &ctx->symbol_table[type_i].value;
    if (sym->decl->tag != Node_TYPEDEF) {
        compile_error(ctx, t, "\"%s\" is not a type", type->name);
        return NULL;
    }
    sym->status = Sym_RESOLVED;
    return real_type;
}

Type *resolve_accessor(Context *ctx, AstBinary *accessor) {
    assert(accessor->op == Token_DOT);
    assert(accessor->right->tag == Node_IDENT);
    assert(accessor->left->tag == Node_IDENT || accessor->left->tag == Node_BINARY);

    Name *rhs = accessor->right->as.ident;
    Type *lhs_type = resolve_expression(accessor->left, ctx);

    if (!lhs_type) return NULL; // we already errored

    if (lhs_type->kind == Type_DEFERRED_NAMED) {
        lhs_type = resolve_type(ctx, lhs_type, accessor->left->token);
        assert(lhs_type);
    }

    if (lhs_type->kind == Type_POINTER)
        lhs_type = unwrap_pointer_type(lhs_type, NULL);

    if (lhs_type->kind != Type_STRUCT) {
        compile_error(ctx, accessor->left->token, "Attempt to access member in non-struct value");
        return NULL;
    }

    AstStruct *struct_def = &lhs_type->data.user->as.struct_;
    Symbol *field = lookup_struct_field(struct_def, rhs);
    if (!field) {
        compile_error(ctx, accessor->right->token, "No such field as \"%s\" in struct field access", rhs->text);
        return NULL;
    }
    if (field->decl->tag != Node_VAR) {
        assert(false); // should have been checked by now, ill see if i can make this go off
    }
    return field->decl->as.var.typename->as.type;
}

// Resolves the dependencies and type of a variable declaration,
// and apply type inference if needed.
Type *resolve_var(Symbol *varsym, Context *ctx) {
    AstVar *var = (AstVar *)varsym->decl;
    if (varsym->status == Sym_RESOLVED)
        return var->typename->as.type;

    if (!(var->flags & VAR_IS_INITED)) {
        varsym->status = Sym_RESOLVED;
        return var->typename->as.type;
    }

    varsym->status = Sym_RESOLVING;
    Type *inferred_type = resolve_expression(var->value, ctx);
    varsym->status = Sym_RESOLVED;

    if (!inferred_type) return NULL;
    if (var->flags & VAR_IS_INFERRED) {
        var->typename->as.type = inferred_type; // type inference!
    }
    return inferred_type;
}

// Resolves the dependencies of an assignment statement,
void resolve_assignment(AstNode *ass, Context *ctx) {
    assert(ass->tag == Node_BINARY);
    AstBinary *bin = (AstBinary *)ass;
    assert(is_assignment(*bin));
    resolve_expression(bin->left, ctx);
    resolve_expression(bin->right, ctx);
}

void resolve_procedure(Symbol *procsym, Context *ctx) {
    if (procsym->status == Sym_RESOLVED) return;
    procsym->status = Sym_RESOLVING;

    AstProcedure *proc = (AstProcedure *)procsym->decl;

    u64 num_params = shlenu(proc->params);
    for (int i = 0; i < num_params; i++) {
        resolve_var(&proc->params[i].value, ctx);
    }

    Type *return_type = proc->return_type->as.type;
    if (return_type->kind == Type_DEFERRED_NAMED)
        resolve_type(ctx, return_type, procsym->decl->token);

    stbds_arrpush(scope_stack, proc); // push the new scope

    AstBlock *block = (AstBlock *)proc->block;
    u64 decls_len = shlenu(block->symbols);
    for (int i = 0; i < decls_len; i++) {
        Symbol *sym = &block->symbols[i].value;
        switch (sym->decl->tag) {
        case Node_VAR:
            resolve_var(sym, ctx);
            break;
        // TODO
        }
    }

    for (int i = 0; i < block->statements->len; i++) {
        AstNode *stmt = block->statements->nodes[i];
        if (is_decl(stmt)) continue;
        switch (stmt->tag) {
        case Node_CALL:
            resolve_call(stmt, ctx);
            break;
        case Node_BINARY:
            resolve_assignment(stmt, ctx);
            break;
        case Node_RETURN:
            resolve_expression(((AstReturn *)stmt)->expr, ctx);
            break;
        }
    }
    procsym->status = Sym_RESOLVED;
    stbds_arrpop(scope_stack); // pop the scope
}

void resolve_top_level(Context *ctx) {
    // TODO proc stack
    u64 len = shlenu(ctx->symbol_table);
    for (int i = 0; i < len; i++) {
        Symbol *sym = &ctx->symbol_table[i].value;
        switch (sym->decl->tag) {
        case Node_PROCEDURE:
            resolve_procedure(sym, ctx);
            break;
        case Node_VAR:
            resolve_var(sym, ctx);
            break;
        }
    }
}

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
static bool inside_decl = false;

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
    if (expr->tag == Node_CALL) {
        AstProcedure *resolved = resolve_call(expr, ctx);
        if (!resolved) return ctx->error_type;
        return resolved->return_type->as.type;
    }
    if (expr->tag == Node_IDENT) {
        Name *name = expr->as.ident;
        Symbol *var = lookup_local(stbds_arrlast(scope_stack)->block, name);
        if (!var) {
            compile_error(ctx, expr->token, "Undeclared identifier \"%s\"", name->text);
            return NULL;
        }
        if (var->status == Sym_UNRESOLVED) {
            Type *resolved_type = resolve_var(var, ctx);
            return resolved_type;
        }
        if (var->status == Sym_RESOLVING && inside_decl) {
            compile_error(ctx, expr->token, "Initial instantiation of variable \"%s\" mentions itself", name->text);
            return NULL;
        }
        if (var->decl->tag != Node_VAR) {
            compile_error(ctx, expr->token, "\"%s\" used like a variable, but it isn't one");
            return NULL;
        }
        AstVar *vardecl = (AstVar *)var->decl;
        return vardecl->typename->as.type;
    }
    if (expr->tag == Node_BINARY) {
        AstBinary *bin = (AstBinary *)expr;
        if (bin->op == Token_DOT) {
            return resolve_accessor(ctx, bin);
        }
        Type *lhs = resolve_expression(bin->left, ctx);
        Type *rhs = resolve_expression(bin->right, ctx);
        return lhs;
    }
    if (expr->tag == Node_ENCLOSED) {
        return resolve_expression(((AstEnclosed *)expr)->sub_expr, ctx);
    }
    
    return NULL;
}

Type *resolve_accessor(Context *ctx, AstBinary *accessor) {
    assert(accessor->op == Token_DOT);
    assert(accessor->right->tag == Node_IDENT);

    Name *rhs = accessor->right->as.ident;
    Type *lhs_type = NULL;

    if (accessor->left->tag == Node_BINARY) {
        lhs_type = resolve_accessor(ctx, &accessor->left->as.binary);
        if (!lhs_type) return NULL;
    } else {
        lhs_type = resolve_expression(accessor->left, ctx);
    }
    assert(lhs_type);

    AstStruct *struct_def = &lhs_type->data.user->as.struct_;
    Symbol *field = lookup_local(struct_def->members, rhs);
    if (!field) {
        compile_error(ctx, accessor->right->token, "No such field as \"%s\" in struct field access", rhs->text);
        return NULL;
    }
    if (field->decl->tag != Node_VAR) {
        assert(false); // should have been checked by now, ill see if i can make this go off
    }
    return field->decl->as.var.typename->as.type;
}

Type *resolve_var(Symbol *varsym, Context *ctx) {
    AstVar *var = (AstVar *)varsym->decl;
    if (varsym->status == Sym_RESOLVED) {
        return var->typename->as.type;
    }
    if (!(var->flags & VAR_IS_INITED)) {
        varsym->status = Sym_RESOLVED;
        return var->typename->as.type;
    }
    varsym->status = Sym_RESOLVING;
    inside_decl = true;
    Type *inferred_type = resolve_expression(var->value, ctx);
    inside_decl = false;
    varsym->status = Sym_RESOLVED;
    if (inferred_type == ctx->error_type) return ctx->error_type;
    if (var->flags & VAR_IS_INFERRED) {
        var->typename->as.type = inferred_type;
    }
    return inferred_type;
}

void resolve_procedure(Symbol *procsym, Context *ctx) {
    if (procsym->status == Sym_RESOLVED) return;
    procsym->status = Sym_RESOLVING;
    AstProcedure *proc = (AstProcedure *)procsym->decl;
    stbds_arrpush(scope_stack, proc);
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
        if (stmt->tag > Node_DECLS_START && stmt->tag < Node_DECLS_END) {
            continue;
        }
        switch (stmt->tag) {
        case Node_CALL:
            resolve_call(stmt, ctx);
            break;
        }
    }
    procsym->status = Sym_RESOLVED;
    stbds_arrpop(scope_stack);
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

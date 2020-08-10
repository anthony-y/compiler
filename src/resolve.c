#include "headers/common.h"
#include "headers/ast.h"
#include "headers/type.h"
#include "headers/context.h"
#include "headers/passes.h"

#include "headers/stb/stb_ds.h"

void resolve_procedure(Symbol *procsym, Context *ctx);
Type *resolve_var(Symbol *varsym, Context *ctx);

static AstProcedure *current_proc = NULL;

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
        Symbol *var = lookup_local(current_proc->block, name);
        if (!var) {
            compile_error(ctx, expr->token, "Undeclared identifier \"%s\"", name->text);
            return NULL;
        }
        if (var->status == Sym_UNRESOLVED) {
            Type *resolved_type = resolve_var(var, ctx);
            return resolved_type;
        }
        if (var->status == Sym_RESOLVING) {
            compile_error(ctx, expr->token, "TODO idk if this should error or not");
            // TODO why does this happen twice in instances like this:
            // b := b
            return NULL;
        }
        if (var->decl->tag != Node_VAR) {
            compile_error(ctx, expr->token, "\"%s\" used like a variable, but it isn't one");
            return NULL;
        }
        AstVar *vardecl = (AstVar *)var->decl;
        return vardecl->typename->as.type;
    }
    return NULL;
}

Type *resolve_var(Symbol *varsym, Context *ctx) {
    AstVar *var = (AstVar *)varsym->decl;
    if (varsym->status == Sym_RESOLVED || !(var->flags & VAR_IS_INITED)) {
        return var->typename->as.type;
    }
    varsym->status = Sym_RESOLVING;
    Type *inferred_type = resolve_expression(var->value, ctx);
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
}

void resolve_top_level(Context *ctx) {
    u64 len = shlenu(ctx->symbol_table);
    for (int i = 0; i < len; i++) {
        Symbol *sym = &ctx->symbol_table[i].value;
        switch (sym->decl->tag) {
        case Node_PROCEDURE:
            current_proc = (AstProcedure *)sym->decl;
            resolve_procedure(sym, ctx);
            break;
        case Node_VAR:
            resolve_var(sym, ctx);
            break;
        }
    }
}

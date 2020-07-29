#include "headers/common.h"
#include "headers/ast.h"
#include "headers/type.h"
#include "headers/context.h"
#include "headers/passes.h"

Type *resolve_typename(Type *name) {
    if (name->kind == Type_PRIMITIVE) return name;
}

#if 0
void resolve_names(Context *ctx) {
    for (int i = 0; i < shlenu(ctx->symbol_table); i++) {
        Symbol *sym = &ctx->symbol_table[i].value;
        switch (sym->decl->tag) {
        case Node_VAR: {
            AstVar *var = (AstVar *)sym->decl;
            switch (var->value->tag) {
            case Node_IDENT: var->value = resolve_name(var->value->as.ident.name)->decl;
            }
        } break;
        case Node_PROCEDURE: {
            AstProcedure *proc = (AstProcedure *)sym->decl;
            proc->return_type = resolve_typename(proc->return_type);
        }
        }
    }
}
#endif

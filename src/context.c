#include "headers/context.h"
#include "headers/ast.h"

#include "headers/stb/stb_ds.h"

#include <stdarg.h>
#include <assert.h>

AstDecl *lookup_in_block(AstBlock *block, Name *name) {
    u64 index = shgeti(block->symbols, name->text);
    if (index == -1) return NULL;
    return block->symbols[index].value;
}

AstDecl *lookup_local(Context *ctx, AstProcedure *proc, Name *name) {
    u64 param_i = shgeti(proc->params, name->text);
    if (param_i != -1) {
        return proc->params[param_i].value;
    }

    AstBlock *block = (AstBlock *)proc->block;
    AstDecl *s = lookup_in_block(block, name);
    if (!s) {
        AstBlock *parent = block->parent;
        while (parent) {
            AstDecl *sym = lookup_in_block(parent, name);
            if (sym) return sym;
            parent = parent->parent;
        }
        u64 global_i = shgeti(ctx->symbol_table, name->text);
        if (global_i != -1) return ctx->symbol_table[global_i].value;
        return NULL;
    }
    return s;
}

AstDecl *lookup_struct_field(AstStruct *def, Name *name) {
    assert(def->members->tag == Stmt_BLOCK);
    return lookup_in_block(&def->members->as.block, name);
}

Name *make_name(Context *ctx, Token token) {
    Name *n = malloc(sizeof(Name));
    char *txt = token.text;
    n->text = txt;
    n->resolved_decl = NULL;
    u64 i = shgeti(ctx->name_table, txt);
    if (i == -1) { // not in the table yet
        shput(ctx->name_table, txt, n);
        return n;
    }
    return ctx->name_table[i].value;
}

inline void add_symbol(Context *c, AstDecl *n, char *name) {
    Token t = ((AstNode *)n)->token;
    if (shgeti(c->symbol_table, name) != -1) {
        compile_error(c, t, "Redefinition of symbol \"%s\" at global scope", name);
        return;
    }
    shput(c->symbol_table, name, n);
}

void init_context(Context *c, const char *file_path) {
    *c = (Context){0};
    c->current_file_path = file_path;
    arena_init(&c->scratch, CONTEXT_SCRATCH_SIZE, sizeof(u8), 1);
}

void free_context(Context *c) {
    u64 len = shlenu(c->name_table);
    for (int i = 0; i < len; i++)
        free(c->name_table[i].value);
    shfree(c->name_table);
    arena_free(&c->scratch);
}

void compile_error(Context *ctx, Token t, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "%s:%lu: Error: ", ctx->current_file_path, t.line);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
    va_end(args);

    ctx->error_count++;
}

void compile_error_start(Context *ctx, Token t, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "%s:%lu: Error: ", ctx->current_file_path, t.line);
    vfprintf(stderr, fmt, args);
    va_end(args);

    ctx->error_count++;
}

void compile_error_add_line(Context *ctx, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vfprintf(stderr, fmt, args);
    va_end(args);
}

void compile_error_end() {
    fprintf(stderr, ".\n");
}

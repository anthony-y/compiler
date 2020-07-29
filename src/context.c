#include "headers/context.h"
#include "headers/ast.h"

#include "headers/stb/stb_ds.h"

#include <stdarg.h>
#include <assert.h>

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

// TODO make it a hash table
// TODO ascend through the blocks until file scope
AstNode *lookup_local(AstNode *nodein, Name *name) {
    assert(nodein->tag == Node_BLOCK);
    AstBlock *in = (AstBlock *)nodein;
    for (int i = 0; i < in->decls->len; i++) {
        AstNode *decl = in->decls->nodes[i];
        Name *decl_name = NULL;
        switch (decl->tag) {
        case Node_VAR: decl_name = ((AstVar*)decl)->name->as.ident; break;
        case Node_PROCEDURE: decl_name = ((AstProcedure*)decl)->name->as.ident; break;
        case Node_TYPEDEF: decl_name = ((AstTypedef*)decl)->name->as.ident; break;
        }
        if (decl_name == name) {
            return decl;
        }
    }
    return NULL;
}

Name *make_name(Context *ctx, Token token) {
    Name *n = malloc(sizeof(Name));
    char *txt = token.text;
    n->text = txt;
    u64 i = shgeti(ctx->name_table, txt);
    if (i == -1) { // not in the table yet
        shput(ctx->name_table, txt, n);
        return n;
    }
    return ctx->name_table[i].value;
}

inline void add_symbol(Context *c, AstNode *n, char *name) {
    shput(c->symbol_table, name, n);
}

void init_context(Context *c, const char *file_path) {
    *c = (Context){0};
    c->current_file_path = file_path;
    c->deferred_names = NULL; // stb stretchy buffer
    arena_init(&c->scratch, CONTEXT_SCRATCH_SIZE, 1, 1);
}

void free_context(Context *c) {
    u64 len = shlenu(c->name_table);
    for (int i = 0; i < len; i++)
        free(c->name_table[i].value);
    shfree(c->name_table);
    arena_free(&c->scratch);
}

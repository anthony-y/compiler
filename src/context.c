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

AstDecl *lookup_local(Context *ctx, AstProcedure *proc, Name *name, AstBlock *start_from) {
    if (proc->params) {
        for (int i = 0; i < proc->params->len; i++) {
            AstDecl *decl = (AstDecl *)proc->params->nodes[i];
            if (decl->name == name) return decl;
        }
    }

    AstDecl *s = lookup_in_block(start_from, name);
    if (!s) {
        AstBlock *parent = start_from->parent;
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
    return make_namet(ctx, token.text);
}

Name *make_namet(Context *ctx, const char *txt) {
    u64 i = shgeti(ctx->name_table, txt);
    if (i == -1) { // not in the table yet
        Name *n = malloc(sizeof(Name));
        n->text = (char *)txt;
        n->resolved_decl = NULL;
        shput(ctx->name_table, txt, n);
        return n;
    }
    return ctx->name_table[i].value;
}

inline void add_symbol(Context *c, AstDecl *n, char *name) {
    Token t = ((AstNode *)n)->token;
    if (shgeti(c->current_module->symbol_table, name) != -1) {
        compile_error(c, t, "Redefinition of symbol \"%s\" in module %s", name, c->current_module->file_path);
        return;
    }
    shput(c->current_module->symbol_table, name, n);
}

void init_context(Context *c, const char *file_path) {
    *c = (Context){0};
    c->current_file_path = file_path;
    arena_init(&c->scratch, CONTEXT_SCRATCH_SIZE, sizeof(u8), 1);
    sh_new_arena(c->string_literal_pool);
}

void free_context(Context *c) {
    u64 len = shlenu(c->name_table);
    for (int i = 0; i < len; i++)
        free(c->name_table[i].value);
    shfree(c->name_table);
    arena_free(&c->scratch);
    shfree(c->string_literal_pool);
    shfree(c->symbol_table);
    //stbds_arrfree(c->imports);
}

Module *create_module(Context *ctx, char *file_path, SourceStats stats) {
    Module *module = malloc(sizeof(Module));
    module->file_path = file_path;
    module->imports = NULL;

    init_types_for_module(module, &stats);

    sh_new_arena(module->symbol_table);
    sh_new_arena(module->type_table);

    return module;
}

void free_module(Module *module) {
    stbds_arrfree(module->imports);
    arena_free(&module->type_allocator);
    shfree(module->symbol_table);
    shfree(module->type_table);
    free(module);
}

void import_symbols_from_module_into_module(Module *from, Module *to) {
    for (int i = 0; i < shlenu(from->symbol_table); i++) {
        AstDecl *decl = from->symbol_table[i].value;
        char *name = from->symbol_table[i].key;
        shput(to->symbol_table, name, decl);
    }
    for (int i = 0; i < from->ast.len; i++) {
        ast_add(&to->ast, from->ast.nodes[i]);
    }
}

void compile_error(Context *ctx, Token t, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    // The weird looking escape characters are to set the text color
    // to red, print "Error", and then reset the colour.
    fprintf(stderr, "%s:%lu: \033[0;31mError\033[0m: ", ctx->current_file_path, t.line);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
    va_end(args);

    ctx->error_count++;
}

void compile_error_start(Context *ctx, Token t, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "%s:%lu: \033[0;31mError\033[0m: ", ctx->current_file_path, t.line);
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

void compile_warning(Context *ctx, Token t, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "%s:%lu: \033[0;33mWarning\033[0m: ", ctx->current_file_path, t.line);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
    va_end(args);
}

// Mostly utility functions for find declarations in scopes, error logging, and initialising the Context struct.
#include "headers/context.h"
#include "headers/ast.h"

#include "headers/stb/stb_ds.h"

#include <stdarg.h>
#include <assert.h>
#include <stdlib.h>

// Move context.c's global proc/block stacks into Module
// Maybe replace the Ast on Module with an AstBlock

AstDecl *find_in_block(AstBlock *block, Name *name) {
    for (u64 i = 0; i < block->statements->len; i++) {
        AstNode *node = block->statements->nodes[i];
        if (node->tag != Node_USING && node->tag != Node_DECL && node->tag != Node_TYPE_DECL) continue;
        auto decl = (AstDecl *)node;
        if (decl->name == name) return decl;
    }
    return NULL;
}

AstDecl *find_top_level_decl(Module *in, Name *name) {
    for (u64 i = 0; i < in->ast.len; i++) {
        AstNode *node = in->ast.nodes[i];
        assert(node->tag == Node_USING || node->tag == Node_DECL || node->tag == Node_TYPE_DECL);
        auto decl = (AstDecl *)node;
        if (decl->name == name) return decl;
    }
    return NULL;
}

AstDecl *find_decl_from_local_scope_upwards(Context *ctx, Name *name, Module *file_scope) {
    AstProcedure *proc        = proc_stack_top(ctx->proc_stack);
    AstBlock     *local_scope = block_stack_top(ctx->block_stack);

    if (proc && proc->params) {
        for (u64 i = 0; i < proc->params->len; i++) {
            auto decl = (AstDecl *)proc->params->nodes[i];
            if (decl->name == name) return decl;
        }
    }

    AstDecl *global = find_top_level_decl(file_scope, name);
    if (global) return global;

    if (!local_scope) return NULL;

    for (u64 i = 0; i < local_scope->statements->len; i++) {
        AstNode *node = local_scope->statements->nodes[i];
        if (node->tag != Node_DECL && node->tag != Node_TYPE_DECL) continue;
        auto decl = (AstDecl *)node;
        if (decl->name == name) return decl;
    }

    AstBlock *parent = local_scope->parent;
    while (parent) {
        for (u64 i = 0; i < parent->statements->len; i++) {
            AstNode *node = parent->statements->nodes[i];
            if (node->tag != Node_DECL && node->tag != Node_TYPE_DECL) continue;
            auto decl = (AstDecl *)node;
            if (decl->name == name) return decl;
        }
        parent = parent->parent;
    }

    return NULL;
}

AstDecl *find_struct_field(AstStruct *def, Name *name) {
    assert(def->members->tag == Node_BLOCK);
    return find_in_block((AstBlock *)def->members, name);
}

Module *get_module(Context *ctx, Name *name, Module *in, Token site) {
    AstDecl *import = find_top_level_decl(in, name);
    if (!import) {
        compile_error(ctx, site, "undeclared package '%s', did you forget to #import it?", name->text);
        return NULL;
    }

    char *path = ((AstImport *)import->expr)->path;

    int module_i = shgeti(ctx->modules, path);
    if (module_i != -1) {
        return ctx->modules[module_i].value;
    }
    compile_error(ctx, site, "no such module '%s'", path); // this fires if there is an import declaration but the file it points to doesn't exist.
    return NULL;
}

Name *make_name_from_token(Context *ctx, Token token) {
    return make_name_string(ctx, token.text);
}

Name *make_name_string(Context *ctx, const char *txt) {
    assert(ctx->name_table);
    s64 i = shgeti(ctx->name_table, txt);
    if (i == -1) { // not in the table yet
        Name *n = (Name *)malloc(sizeof(Name));
        n->text = (char *)txt;
        shput(ctx->name_table, txt, n);
        return n;
    }
    return ctx->name_table[i].value;
}

//
// First-in-last-out stacks for procedures and blocks.
//
void block_stack_push(BlockStack *bs, AstBlock *block) {
    bs->data[++bs->top] = block;
    assert(bs->top <= CONTEXT_STACK_SIZE);
}

AstBlock *block_stack_pop(BlockStack *bs) {
    AstBlock *block = bs->data[bs->top];
    bs->data[bs->top--] = NULL;
    return block;
}

AstBlock *block_stack_top(BlockStack bs) {
    return bs.data[bs.top];
}

void proc_stack_push(ProcedureStack *ps, AstProcedure *proc) {
    ps->data[++ps->top] = proc;
}

AstProcedure *proc_stack_pop(ProcedureStack *ps) {
    AstProcedure *proc = ps->data[ps->top];
    ps->data[ps->top--] = NULL;
    return proc;
}

AstProcedure *proc_stack_top(ProcedureStack ps) {
    return ps.data[ps.top];
}


void init_context(Context *ctx) {
    *ctx = Context{};

    arena_init(&ctx->string_allocator, 1024 * 1000, sizeof(char), 1);
    node_arena_init(&ctx->node_allocator);

    sh_new_arena(ctx->name_table);
    sh_new_arena(ctx->modules);

    ast_init(&ctx->link_libraries, 10);

    ctx->entry_point_name = make_name_string(ctx, "main");

    // make_name_string(ctx, "_");
}

void free_context(Context *ctx) {
    for (s64 i = 0; i < shlen(ctx->modules); i++) {
        Ast *ast = &ctx->modules[i].value->ast;
        ast_free(ast);
        free(ast);
    }
    shfree(ctx->modules);

    u64 len = shlenu(ctx->name_table);
    for (u64 i = 0; i < len; i++)
        free(ctx->name_table[i].value);
    shfree(ctx->name_table);

    node_arena_free(&ctx->node_allocator);

    ast_free(&ctx->link_libraries);
}

void compile_error(Context *ctx, Token t, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    // The weird looking escape characters are to set the text color
    // to red, print "Error", and then reset the colour.
    fprintf(stderr, "%s:%lu: \033[0;31mError\033[0m: ", t.file, t.line);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
    va_end(args);

    ctx->error_count++;
}

void compile_error_start(Context *ctx, Token t, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "%s:%lu: \033[0;31mError\033[0m: ", t.file, t.line);
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

    fprintf(stderr, "%s:%lu: \033[0;33mWarning\033[0m: ", t.file, t.line);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
    va_end(args);
}

char *read_file(const char *path) {
    FILE *f = fopen(path, "r");
    if (!f) {
        fprintf(stderr, "Error: failed to open file %s.\n", path);
        exit(0);
    }

    fseek(f, 0L, SEEK_END);
    u64 file_length = ftell(f);
    rewind(f);

    char *buffer = (char *)malloc(file_length + 1);
    if (!buffer) {
        fprintf(stderr, "Error: not enough memory to read \"%s\".\n", path);
        fclose(f);
        exit(0);
    }

    u64 bytes_read = fread(buffer, sizeof(char), file_length, f);
    if (bytes_read < file_length) {
        fprintf(stderr, "Error: failed to read file \"%s\".\n", path);
        fclose(f);
        exit(0);
    }

    buffer[bytes_read] = '\0';

    fclose(f);

    return buffer;
}

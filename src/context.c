#include "headers/context.h"
#include "headers/ast.h"

#define STB_DS_IMPLEMENTATION
#include "headers/stb/stb_ds.h"

#include <stdarg.h>
#include <assert.h>
#include <stdlib.h>

AstDecl *lookup_in_block(AstBlock *block, Name *name) {
    for (u64 i = 0; i < block->statements->len; i++) {
        AstNode *node = block->statements->nodes[i];
        if (!is_decl(node)) continue;
        AstDecl *decl = (AstDecl *)node;
        if (decl->name == name) return decl;
    }
    return NULL;
}

AstDecl *find_decl(Ast *ast, Name *name) {
    for (u64 i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        if (!is_decl(node)) continue;
        AstDecl *decl = (AstDecl *)node;
        if (decl->name == name) return decl;
    }
    return NULL;
}

AstDecl *find_type_decl(Ast *ast, Name *name) {
    for (u64 i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        if (node->tag != Node_TYPEDEF) continue;
        assert(is_decl(node));
        AstDecl *decl = &node->as.decl;
        if (decl->name == name) return decl;
    }
    return NULL;
}

// Returns the Ast of an imported module, or NULL is there's no match.
Ast *get_module(Context *ctx, Name *name, Ast *in, Token site) {
    AstDecl *import = find_decl(in, name);
    if (!import) {
        compile_error(ctx, site, "undeclared package '%s', did you forget to #import it?", name->text);
        return NULL;
    }
    if (import->tag != Decl_VAR) {
        compile_error(ctx, site, "you tried to use a procedure/type name as a package name ('%s')", name->text);
        return NULL;
    }
    
    AstVar *var = (AstVar *)import;
    assert (var->value->tag == Expr_IMPORT); // TODO real error

    char *path = var->value->as.import.path;

    int module_i = shgeti(ctx->modules, path);
    if (module_i != -1) {
        return ctx->modules[module_i].value;
    }
    compile_error(ctx, site, "no such module '%s'", path); // this fires if there is an import declaration but the file it points to doesn't exist.
    return NULL;
}

AstDecl *lookup_local(Context *ctx, AstProcedure *proc, Name *name, AstBlock *start_from, Ast *file_scope) {
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
		AstDecl *global = find_decl(file_scope, name);
        if (global) {
            return global;
        }
        return NULL;
    }
    return s;
}

AstDecl *lookup_struct_field(AstStruct *def, Name *name) {
    assert(def->members->tag == Stmt_BLOCK);
    return lookup_in_block(&def->members->as.block, name);
}

Name *make_name_from_token(Context *ctx, Token token) {
    return make_name_from_string(ctx, token.text);
}

Name *make_name_from_string(Context *ctx, const char *txt) {
    assert(ctx->name_table);
    u64 i = shgeti(ctx->name_table, txt);
    if (i == -1) { // not in the table yet
        Name *n = malloc(sizeof(Name));
        n->text = (char *)txt;
        shput(ctx->name_table, txt, n);
        return n;
    }
    return ctx->name_table[i].value;
}

void init_context(Context *c) {
    *c = (Context){0};
    arena_init(&c->string_allocator, 1024 * 1000, sizeof(char), 1);
    arena_init(&c->scratch, CONTEXT_SCRATCH_SIZE, sizeof(u8), 1);
    sh_new_arena(c->name_table);
    sh_new_arena(c->modules);
}

void free_context(Context *c) {
    u64 len = shlenu(c->name_table);
    for (int i = 0; i < len; i++)
        free(c->name_table[i].value);
    shfree(c->name_table);

    arena_free(&c->scratch);
    arena_free(&c->node_allocator);
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

	char *buffer = malloc(file_length + 1);
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

#include "headers/context.h"
#include "headers/ast.h"

#include "headers/stb/stb_ds.h"

#include <stdarg.h>
#include <assert.h>

AstDecl *lookup_in_block(AstBlock *block, Name *name) {
	return table_get(&block->symbols, name->text);
}

AstDecl *lookup_local(Context *ctx, AstProcedure *proc, Name *name, AstBlock *start_from) {
    if (proc->params) {
        for (int i = 0; i < proc->params->len; i++) {
            AstDecl *decl = (AstDecl *)proc->params->nodes[i];
            if (decl->name == name) return decl;
        }
    }

    Table *table = &ctx->symbols;
    AstDecl *s = lookup_in_block(start_from, name);
    if (!s) {
        AstBlock *parent = start_from->parent;
        while (parent) {
            AstDecl *sym = lookup_in_block(parent, name);
            if (sym) return sym;
            parent = parent->parent;
        }
		AstDecl *global = table_get(table, name->text);
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

char *encode_module_into_name(Context *ctx, char *raw) {
    if (!ctx->current_module) {
        return get_encoded_name(NULL, raw);
    } else {
        return get_encoded_name(ctx->current_module->path, raw);
    }
}

char *get_encoded_name(char *module_path, char *raw) {
    if (!module_path) return raw;

    static const int postfix_len = 5/*strlen(".lang")*/;
    int module_path_len = strlen(module_path);
    int no_extension_len = (module_path_len - postfix_len);

    int raw_len = strlen(raw);
    int final_len = no_extension_len + raw_len + 2;

    char *final = malloc(final_len);
    strncpy(final, module_path, no_extension_len+1);
    strcat(final, raw);

    // Replace unfriendly characters with underscores.
    for (int i = 0; i < no_extension_len + raw_len; i++) {
        char c = module_path[i];
        if (c == '.' || c == '/' || c == '\\') {
            final[i] = '_';
        }
    }
    final[no_extension_len] = '@'; // sentinal character that the user can't put in an identifier.
    final[final_len] = 0; // null terminator.
    return final;
}

inline void add_symbol(Context *c, AstDecl *n, char *name) {
    Table *table = &c->symbols;
    Token t = ((AstNode *)n)->token;
    //char *encoded_name = encode_module_into_name(c, name); // TODO fix leak
    if (table_get(table, /*encoded_*/name)) {
        compile_error(c, t, "Redefinition of symbol \"%s\" in module %s", name, c->current_module->path);
        return;
    }
    assert(table_add(table, /*encoded_*/name, n));
}

void init_context(Context *c) {
    *c = (Context){0};
    arena_init(&c->string_allocator, 1024 * 20, sizeof(char), 1);
    arena_init(&c->scratch, CONTEXT_SCRATCH_SIZE, sizeof(u8), 1);
    sh_new_arena(c->name_table);
	table_init(&c->symbols);
}

void free_context(Context *c) {
    u64 len = shlenu(c->name_table);
    for (int i = 0; i < len; i++)
        free(c->name_table[i].value);
    shfree(c->name_table);
    arena_free(&c->scratch);
    arena_free(&c->node_allocator);
	table_free(&c->symbols);
}

void compile_error(Context *ctx, Token t, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    // The weird looking escape characters are to set the text color
    // to red, print "Error", and then reset the colour.
    fprintf(stderr, "%s:%lu: \033[0;31mError\033[0m: ", ctx->current_module->path, t.line);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
    va_end(args);

    ctx->error_count++;
}

void compile_error_start(Context *ctx, Token t, const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    fprintf(stderr, "%s:%lu: \033[0;31mError\033[0m: ", ctx->current_module->path, t.line);
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

    fprintf(stderr, "%s:%lu: \033[0;33mWarning\033[0m: ", ctx->current_module->path, t.line);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, ".\n");
    va_end(args);
}

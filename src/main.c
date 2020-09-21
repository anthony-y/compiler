// main.c is the entry point for the compiler.
// It reads the source code, and invokes stages
// of the compilation pipeline, as well as performing
// rough profiling of each step.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#include "headers/context.h"
#include "headers/parser.h"
#include "headers/lexer.h"
#include "headers/passes.h"
#include "headers/type.h"
#include "headers/ast.h"
#include "headers/arena.h"
#include "headers/bytecode.h"

#define STB_DS_IMPLEMENTATION
#include "headers/stb/stb_ds.h"
#include "headers/stb/stretchy_buffer.h"

bool lexer_lex(Lexer *l, TokenList *list, SourceStats *stats, ModuleTable *table);
static char *read_file(const char *path);
static void ensure_main_is_declared(Context *ctx);
static void print_unused_symbol_warnings(Context *, Module *);
static void do_front_end_for_module(Context *, Module *module, char *path, char *data, Module *imported_in);

// Roughly time the execution of "code" in microseconds
// There must be a variable called "id"_delta in the scope that you use this in
#define PROFILE(id, code) do {\
    struct timeval id ## end, id ## start;\
    gettimeofday(&id##start, NULL);\
    code\
    gettimeofday(&id##end, NULL);\
    id##_delta = (id##end.tv_sec - id##start.tv_sec) * 1000000 + id##end.tv_usec - id##start.tv_usec;\
} while (false)

static Module *modules_storage;

int main(int arg_count, char *args[]) {
    if (arg_count < 2) {
        fprintf(stderr, "Error: expected a root compilation target (a file path).\n");
        return 1;
    }

    Context context;
    init_context(&context);

    modules_storage = NULL;

    #define NEXT_STAGE_OR_QUIT() if (context.error_count > 0) goto end

    char *data = read_file(args[1]);

    SourceStats main_stats = (SourceStats){10}; // stats for main module
    SourceStats total_stats; // stats for all used modules

    TokenList tokens; // tokens for main module
    TokenList import_paths; // import paths for main module
    Parser parser; // parser for main module
    Lexer lexer;

    token_list_init(&tokens);
    token_list_init(&import_paths);
    lexer_init(&lexer, args[1], data);
    lexer.string_allocator = &context.string_allocator;

    ModuleTable *import_table;
    sh_new_arena(import_table);

    // Collect tokens, stats and import paths from the main module.
    if (!lexer_lex(&lexer, &tokens, &main_stats, import_table)) {
        return 1;
    }

    if (tokens.len == 1) {
        assert(tokens.tokens[0].type == Token_EOF);
        return 0; // just an EOF token, so exit.
    }

    // Tally up SourceStats for every file imported into the program
    // This includes files imported indirectly (imports inside imports).
    total_stats = main_stats;
    for (u64 i = 0; i < shlenu(import_table); i++) {
        char *path = import_table[i].key;
        char *tmp_file_data = read_file(path);
        lexer_init(&lexer, path, tmp_file_data);
        bool success = lexer_lex(&lexer, NULL, &total_stats, import_table);
        free(tmp_file_data);

        if (!success) goto end;
    }

    //
    // At this point we know how much memory to allocate.
    //

    modules_storage = malloc(shlenu(import_table) * sizeof(Module));
    assert(modules_storage);

    // Allocate space for AST nodes for all modules.
    const u64 max_nodes = (u64)(total_stats.number_of_lines * 10); // roughly 5 nodes for each line
    arena_init(&context.node_allocator, max_nodes, sizeof(AstNode), 8);
    init_types(&context, &total_stats);

    //
    // Main module
    //
    Module main_module = (Module){0};
    parser_init(&parser, &tokens, &main_stats);
    init_module(&context, &main_module, main_stats, args[1]);
    main_module.imports = import_table;
    Ast ast = parse(&context, &parser);
    main_module.ast = ast;

    NEXT_STAGE_OR_QUIT();

    ensure_main_is_declared(&context);

    // Do front-end for files imported in main
    for (u64 i = 0; i < shlenu(import_table); i++) {
        NEXT_STAGE_OR_QUIT();
        char *path = import_table[i].key;
        char *module_data = read_file(path);
        do_front_end_for_module(&context, &modules_storage[i], path, module_data, &main_module);
    }

    // Reset the current module to main.
    context.current_module = &main_module;

    //
    // Complete compilation pipeline for the main file.
    //
    NEXT_STAGE_OR_QUIT();
    resolve_main_module(&context, &main_module);
    print_unused_symbol_warnings(&context, &main_module);
    NEXT_STAGE_OR_QUIT();
    check_ast(&context, &ast);
    NEXT_STAGE_OR_QUIT();

    char *output_path = generate_and_write_c_code(&context, &ast);
    u64 len = strlen("gcc -std=c99") + strlen(output_path) + strlen("-o ") + strlen("-Wno-discarded-qualifiers ") + strlen("-Wno-return-local-addr") + strlen("-Wno-builtin-declaration-mismatch") + 1;
    char *command = arena_alloc(&context.scratch, len);
    sprintf(command, "gcc -std=c99 %s -Wno-return-local-addr -Wno-discarded-qualifiers -Wno-builtin-declaration-mismatch -o output_bin", output_path);
    system(command);

end:
    shfree(import_table);
    parser_free(&parser, &ast);
    token_list_free(&tokens);
    token_list_free(&import_paths);
    free_context(&context);
    free(data);

    return 0;
}

static void ensure_main_is_declared(Context *ctx) {
    if (!ctx->decl_for_main) {
        compile_error(ctx, (Token){0}, "No entry point found. Please declare \"main\"");
    }
    
    Token main_decl_token = decl_tok(ctx->decl_for_main);
    if (ctx->decl_for_main->tag != Decl_PROC) {
        compile_error(ctx, main_decl_token, "Entry point \"main\" must be a procedure");
    } else if (((AstProcedure *)ctx->decl_for_main)->params) {
        compile_error(ctx, main_decl_token, "Entry point \"main\" must not take any arguments");
    } else if (((AstProcedure *)ctx->decl_for_main)->return_type->as.type != ctx->type_void) {
        compile_error(ctx, main_decl_token, "Entry point \"main\" must return void");
    }
}

static void do_front_end_for_module(Context *ctx, Module *module, char *path, char *data, Module *imported_in) {
    SourceStats module_stats = (SourceStats){10};
    TokenList module_import_paths;
    TokenList module_tokens;
    Lexer module_lexer;
    Parser module_parser;

    token_list_init(&module_tokens);
    token_list_init(&module_import_paths);
    lexer_init(&module_lexer, path, data);
    module_lexer.string_allocator = &ctx->string_allocator;

    ModuleTable *import_table;
    sh_new_arena(import_table);

    if (!lexer_lex(&module_lexer, &module_tokens, &module_stats, import_table)) {
        shfree(import_table);
        free(data);
        return;
    }

    init_module(ctx, module, module_stats, path);
    parser_init(&module_parser, &module_tokens, &module_stats);

    u64 mi = shgeti(imported_in->imports, path);

    Ast module_ast = parse(ctx, &module_parser);
    module->ast = module_ast;

    if (ctx->error_count > 0) return;

    for (u64 i = 0; i < shlenu(import_table); i++) {
        if (ctx->error_count > 0) return;
        char *import_path = import_table[i].key;
        char *module_data = read_file(path);
        module->imports = import_table;
        do_front_end_for_module(ctx, &modules_storage[i], import_path, module_data, module);
        import_table[i].value = module;
    }
    ctx->current_module = module;

    // for (u64 i = 0; i < shlenu(module->symbols); i++) {
    //     AstDecl *decl = module->symbols[i].value;
    //     shput(imported_in->symbols, decl->name->text, decl);
    // }

    // print_unused_symbol_warnings(ctx, module);

    resolve_module(ctx);
    if (ctx->error_count > 0) return;
    check_ast(ctx, &module_ast);

    imported_in->imports[mi].value = module;
}

static void print_unused_symbol_warnings(Context *ctx, Module *module) {
    u64 len = shlenu(module->symbols);
    for (int i = 0; i < len; i++) {
        AstDecl *d = module->symbols[i].value;
        char *name = d->name->text;
        Token t = decl_tok(d);
        if (d->status == Status_UNRESOLVED) {
            switch (d->tag) {
            case Decl_VAR: compile_warning(ctx, t, "variable \"%s\" was declared but never used", name); break;
            case Decl_PROC: compile_warning(ctx, t, "procedure \"%s\" was declared but never called",name); break;
            case Decl_TYPEDEF: compile_warning(ctx, t, "type \"%s\" was declared but never used", name); break;
            }
        }
    }
}

static char *read_file(const char *path) {
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

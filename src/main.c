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

static char *read_file(const char *path);
static void print_unused_symbol_warnings(Context *, Module *);

// Roughly time the execution of "code" in microseconds
// There must be a variable called "id"_delta in the scope that you use this in
#define PROFILE(id, code) do {\
    struct timeval id ## end, id ## start;\
    gettimeofday(&id##start, NULL);\
    code\
    gettimeofday(&id##end, NULL);\
    id##_delta = (id##end.tv_sec - id##start.tv_sec) * 1000000 + id##end.tv_usec - id##start.tv_usec;\
} while (false)

// THINK: for imports, just lex, parse and resolve all the files and then merge their scopes into one scope, then generate code for the symbols, symbols that are Sym_UNRESOLVED, can be ignored.

int main(int arg_count, char *args[]) {
    if (arg_count < 2) {
        fprintf(stderr, "Error: expected a root compilation target (a file path).\n");
        return 1;
    }

    Context context;
    init_context(&context, args[1]);

    char *data = read_file(args[1]);

    SourceStats main_stats = (SourceStats){10};
    SourceStats total_stats;

    TokenList tokens;
    TokenList import_paths;
    Parser parser;

    Lexer lexer;
    lexer.string_allocator = &context.string_allocator;
    lexer_init(&lexer, args[1], data);

    token_list_init(&tokens);
    token_list_init(&import_paths);

    // Collect tokens, stats and import paths from the main module.
    if (!lexer_lex(&lexer, &tokens, &main_stats, &import_paths)) {
        return 1;
    }

    if (tokens.len == 1) return 0;

    total_stats = main_stats;

    // Tally up SourceStats for every file imported into the program
    // This includes files imported indirectly.
    for (u64 i = 0; i < total_stats.number_of_imports; i++) {
        Token path_with_quotes = import_paths.tokens[i];
        char *unquoted = arena_alloc(&context.scratch, path_with_quotes.length-1);
        strncpy(unquoted, path_with_quotes.text+1, path_with_quotes.length-2);

        char *tmp_file_data = read_file(unquoted);
        lexer_init(&lexer, unquoted, tmp_file_data);
        bool success = lexer_lex(&lexer, NULL, &total_stats, &import_paths);
        free(tmp_file_data);

        if (!success) goto end;
    }

    // At this point we know how much memory to allocate

    const u64 max_nodes = (u64)(total_stats.number_of_lines * 5); // roughly 5 nodes for each line
    arena_init(&context.node_allocator, max_nodes, sizeof(AstNode), 8);

    // Ensure there is enough space for all modules plus the root module,
    context_init_modules(&context, &total_stats); // and then reserve the space for them.
    init_types(&context, &total_stats);

    // Compile main module
    Module main_module = (Module){0};
    main_module.name = make_namet(&context, "main");
    context.modules[0] = main_module;

    parser_init(&parser, &tokens, &main_stats);
    Ast ast = parse(&context, &parser);
    if (context.error_count > 0) goto end;

    // Compile imported modules
    for (u64 i = 1; i <= total_stats.number_of_imports; i++) {
        Module *module = &context.modules[i];

        Token path_with_quotes = import_paths.tokens[i-1];
        char *unquoted = arena_alloc(&context.scratch, path_with_quotes.length-1);
        strncpy(unquoted, path_with_quotes.text+1, path_with_quotes.length-2);

        char *module_data = read_file(unquoted);

        SourceStats module_stats = (SourceStats){10};

        TokenList module_tokens;
        token_list_init(&module_tokens);

        Lexer module_lexer;
        module_lexer.string_allocator = &context.string_allocator;
        lexer_init(&module_lexer, unquoted, module_data);

        if (!lexer_lex(&module_lexer, &module_tokens, &module_stats, NULL)) {
            free(module_data);
            goto end;
        }

        Parser module_parser;
        parser_init(&module_parser, &module_tokens, &module_stats);
        Ast module_ast = parse(&context, &module_parser);
        if (context.error_count > 0) goto end;
        for (int i = 0; i < module_ast.len; i++) {
            ast_add(&ast, module_ast.nodes[i]);
        }
    }

    resolve_program(&context);
    if (context.error_count > 0) goto end;
    check_ast(&context, &ast);
    if (context.error_count > 0) goto end;

    char *output_path = generate_and_write_c_code(&context, &ast);
    u64 len = strlen("gcc -std=c99") + strlen(output_path) + strlen("-o ") + strlen("-Wno-discarded-qualifiers ") + strlen("-Wno-return-local-addr") + strlen("-Wno-builtin-declaration-mismatch") + 1;
    char *command = arena_alloc(&context.scratch, len);
    sprintf(command, "gcc -std=c99 %s -Wno-return-local-addr -Wno-discarded-qualifiers -Wno-builtin-declaration-mismatch -o output_bin", output_path);
    system(command);

end:
    parser_free(&parser, &ast);
    token_list_free(&tokens);
    token_list_free(&import_paths);
    free_context(&context);
    free(data);

    return 0;
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

// main.c is the entry point for the compiler.
// It reads the source code, and invokes stages
// of the compilation pipeline, as well as performing
// rough profiling of each step.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <assert.h>

#include "headers/table.h"
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
static void ensure_main_is_declared(Context *ctx);
static void print_unused_symbol_warnings(Context *);

// Roughly time the execution of "code" in microseconds
// There must be a variable called "id"_delta in the scope that you use this in
#define PROFILE(id, code) do {\
    struct timeval id ## end, id ## start;\
    gettimeofday(&id##start, NULL);\
    code\
    gettimeofday(&id##end, NULL);\
    id##_delta = (id##end.tv_sec - id##start.tv_sec) * 1000000 + id##end.tv_usec - id##start.tv_usec;\
} while (false)

static Arena module_arena;

// All imports must be initialized individually before they are collectively
// resolved, checked and compiled.
Module *load_imported_module(Context *ctx, Arena *storage, char *path) {
	// Lex, parse, resolve, check.
	// And keep lexer, parser, etc. on Module to be freed later.
	// Allocate into "storage" and store the pointers in the ModuleTable.
	//
	// All the functionality is there, just need to find the right way
	// to hook it up to multiple files/namespaces.
	//
	char *data = read_file(path);
	if (!data) return false;
	
	Table imports;
	SourceStats stats = (SourceStats){10};
	TokenList   tokens;
	Parser      parser;
	Lexer       lexer;
	Ast         ast;

	init_table(&imports);

	token_list_init(&tokens);
	lexer_init(&lexer, path, data);
	lexer.string_allocator = &ctx->string_allocator;

	Module *mod = arena_alloc(storage, sizeof(Module));
    mod->path = path;

	if (!lexer_lex(&lexer, &tokens, &stats, &imports)) return NULL;
	if (tokens.len == 1) return mod;
	ctx->current_module = mod;
	parser_init(&parser, &tokens, &stats);
	ast = parse(ctx, &parser);

	mod->imports = imports;
    // mod->symbols = NULL; // TODO
	mod->ast = ast;
	return mod;
}

void free_imported_module(Module *mod) {} // TODO

int main(int arg_count, char *args[]) {
    if (arg_count < 2) {
        fprintf(stderr, "Error: expected a root compilation target (a file path) as argument.\n");
        return 1;
    }

    Context context;
    init_context(&context);

	#define NEXT_STAGE_OR_QUIT() if (context.error_count > 0) goto end

    char *data = read_file(args[1]);

    SourceStats main_stats = (SourceStats){10}; // stats for main module
    SourceStats total_stats; // stats for all used modules

    TokenList tokens; // tokens for main module
    Parser parser; // parser for main module
    Lexer lexer; // lexer used to gather telemetry for all used modules

    token_list_init(&tokens);
    lexer_init(&lexer, args[1], data);
    lexer.string_allocator = &context.string_allocator;

    Table import_table; // Hash-table of char* -> Module*
    init_table(&import_table);

    // Collect tokens, stats and import paths from the main module.
    if (!lexer_lex(&lexer, &tokens, &main_stats, &import_table)) {
        return 1;
    }

    if (tokens.len == 1) {
        assert(tokens.tokens[0].type == Token_EOF);
        return 0; // just an EOF token, so exit.
    }

    // Tally up SourceStats for every file imported into the program
    // This includes files imported indirectly (imports inside imports).
    total_stats = main_stats;
    TableIter import_iter = table_get_iterator(&import_table);
    for (u64 i = 0; i < import_iter.num_entries; i++) {
        char *path = import_iter.pairs[i].key;
        char *tmp_file_data = read_file(path);
        lexer_init(&lexer, path, tmp_file_data);
        bool success = lexer_lex(&lexer, NULL, &total_stats, &import_table);
        free(tmp_file_data);
        if (!success) goto end;
        import_iter = table_get_iterator(&import_table);
    }

    //
    // At this point we know how much memory to allocate.
    //

    // Allocate space for AST nodes for all modules.
    const u64 max_nodes = (u64)(total_stats.number_of_lines * 7);
    assert(arena_init(&context.node_allocator, max_nodes, sizeof(AstNode), 8));
	assert(arena_init(&module_arena, import_table.num_entries, sizeof(Module), 8));

    init_types(&context, &total_stats); // Initialize the type table, and allocate space for user defined types

    parser_init(&parser, &tokens, &main_stats); // Initialize the parser for the main module

	// Perform front end for each imported file.
	for (u64 i = 0; i < import_iter.num_entries; i++) {
		char *path = import_iter.pairs[i].key;
		Module *mod = load_imported_module(&context, &module_arena, path);
		if (!mod) {
			break;
		}

		// Put the Module* into the imports table.
		// TODO: this could probably be cleaner.
        u64 rehash = table_hash_key(path);
		import_table.pairs[rehash % import_table.capacity].value = mod;
	}

    NEXT_STAGE_OR_QUIT();
	
	// Actually parse the main module.
    Ast ast;
    ast = parse(&context, &parser);
	
	// Copy the declarations from each of the imported modules
	// into the main modules AST.
	// TODO: namespaced modules.
    import_iter = table_get_iterator(&import_table);
	for (u64 i = 0; i < import_iter.num_entries; i++) {
		Module *mod = import_iter.pairs[i].value;
		for (u64 j = 0; j < mod->ast.len; j++) {
			ast_add(&ast, mod->ast.nodes[j]);
		}
	}
	free(import_iter.pairs);

    ensure_main_is_declared(&context);
    NEXT_STAGE_OR_QUIT();

    //
    // Complete compilation pipeline for the main file.
    //
	resolve_module(&context);
	print_unused_symbol_warnings(&context);

    NEXT_STAGE_OR_QUIT();

	check_ast(&context, &ast);

    NEXT_STAGE_OR_QUIT();
	
	//
	// C code generation
	//
    char *output_path = generate_and_write_c_code(&context, &ast);
    u64 len = strlen("gcc -g -std=c99") + strlen(output_path) + strlen("-o ") + strlen("-Wno-discarded-qualifiers ") + strlen("-Wno-return-local-addr") + strlen("-Wno-builtin-declaration-mismatch") + 1;
    char *command = arena_alloc(&context.scratch, len);
    sprintf(command, "gcc -g -std=c99 %s -Wno-return-local-addr -Wno-discarded-qualifiers -Wno-builtin-declaration-mismatch -o output_bin", output_path);
    system(command);
	printf("It ran.\n");
end:
    free_table(&import_table);
	parser_free(&parser, &ast);
    token_list_free(&tokens);
    free_context(&context);
    free(data);
    arena_free(&module_arena);

    return 0;
}

static void ensure_main_is_declared(Context *ctx) {
    if (!ctx->decl_for_main) {
        compile_error(ctx, (Token){0}, "No entry point found. Please declare \"main\"");
        return;
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

static void print_unused_symbol_warnings(Context *ctx) {
    TableIter it = table_get_iterator(&ctx->symbols);
    for (int i = 0; i < it.num_entries; i++) {
        AstDecl *d = it.pairs[i].value;
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
    free(it.pairs);
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

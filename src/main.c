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

// THINK: for imports, just lex, parse and resolve all the files and then merge their scopes into one scope, then generate code for the symbols, symbols that are Sym_UNRESOLVED, can be ignored.

Module *do_front_end_for_file(Context *ctx, char *file_path) {
    #define NEXT_STAGE_OR_QUIT() if (ctx->error_count > 0) goto end;

    char *file_data = read_file(file_path);
    ctx->current_file_path = file_path;

    Ast ast;
    Lexer lexer;
    Parser parser;
    TokenList tokens;
    SourceStats stats = (SourceStats){10}; // initialize all the fields to 10

    u64 lexer_delta    = 0;
    u64 parser_delta   = 0;
    u64 checker_delta  = 0;
    u64 bc_gen_delta   = 0;
    u64 interp_delta   = 0;
    u64 code_gen_delta = 0;

    lexer_init(&lexer, file_path, file_data);
    token_list_init(&tokens);

    PROFILE(lexer, {
        if (!lexer_lex(&lexer, &tokens, &stats)) {
            lexer_free(&lexer);
            token_list_free(&tokens);
            free(file_data);
            return NULL;
        }
    });

    if (tokens.len == 1) return NULL;

    Module *module = create_module(ctx, file_path, stats);

    parser_init(&parser, &tokens, &stats);
    ctx->parser = &parser;
    ctx->current_module = module;

    ast = parse(ctx);
    module->ast = ast;

    for (int i = 0; i < stbds_arrlen(module->imports); i++) {
        Module *imported = do_front_end_for_file(ctx, module->imports[i]->path);
        ctx->current_module = module;
        if (imported) {
            import_symbols_from_module_into_module(imported, module);
        } else {
            break;
        }
    }

    #undef NEXT_STAGE_OR_QUIT
end:
    return module;
}

bool do_compilation(const char *file_path) {
    char *file_data = read_file(file_path); // read the file into a zero-terminated buffer.

    u64 checker_delta  = 0;
    u64 bc_gen_delta   = 0;
    u64 interp_delta   = 0;
    u64 code_gen_delta = 0;

    Context context;
    #define NEXT_STAGE_OR_QUIT() if (context.error_count > 0) goto end;

    init_context(&context, file_path);
    init_builtin_types(&context);

    Module *main = do_front_end_for_file(&context, file_path);

    NEXT_STAGE_OR_QUIT();

    if (!context.decl_for_main)
        compile_error(&context, (Token){0}, "No entry point found. Please declare \"main\"");
    else if (context.decl_for_main->tag != Decl_PROC)
        compile_error(&context, decl_tok(context.decl_for_main), "Entry point \"main\" must be a procedure");
    else if (((AstProcedure *)context.decl_for_main)->params)
        compile_error(&context, decl_tok(context.decl_for_main), "Entry point \"main\" must not take any arguments");
    else if (((AstProcedure *)context.decl_for_main)->return_type->as.type != context.type_void)
        compile_error(&context, decl_tok(context.decl_for_main), "Entry point \"main\" must return void");

    PROFILE(checker, {
        resolve_program(&context);
        NEXT_STAGE_OR_QUIT();
        check_ast(&context, &main->ast); // type and semantic checking
        NEXT_STAGE_OR_QUIT();
    });

    PROFILE(code_gen, {
        context.current_file_path = file_path;
        char *output_path = generate_and_write_c_code(&context, &main->ast);

        u64 len = strlen("gcc -std=c99") + strlen(output_path) + strlen("-o ") + strlen("-Wno-discarded-qualifiers ") + strlen("-Wno-return-local-addr") + strlen("-Wno-builtin-declaration-mismatch") + 1;
        char *command = arena_alloc(&context.scratch, len);
        sprintf(command, "gcc -std=c99 %s -Wno-return-local-addr -Wno-discarded-qualifiers -Wno-builtin-declaration-mismatch -o output_bin", output_path);
        system(command);
    });

    NEXT_STAGE_OR_QUIT();

    print_unused_symbol_warnings(&context);

end:
    {
        bool ret = (context.error_count > 0);

        if (ret) {
            printf("\n");
        }

        printf("Error count: %d\n", context.error_count);
        //printf("Total time: %ldus\n", lexer_delta + parser_delta + checker_delta + code_gen_delta);
        //printf("\tLexing took %ldus\n", lexer_delta);
        //printf("\tParsing took %ldus\n", parser_delta);
        printf("\t\tParser used %lu/%lu nodes\n", context.parser->node_count, context.parser->node_allocator.capacity);
        printf("\tInferring, resolving and checking took %ldus\n", checker_delta);
        printf("\tCodegen took %ldus\n", code_gen_delta);
        printf("\tBytecode generation took %ldus\n", bc_gen_delta);
        printf("\tCompile time execution took %ldus\n", interp_delta);

        //interp_free(&interp);
        //free_subtrees_and_blocks(&ast);
        free_types(&context);
        //parser_free(&parser, &ast);
        //lexer_free(&lexer);
        free_context(&context);
        free(file_data);

        return ret;
    }
}

int main(int arg_count, char *args[]) {
    if (arg_count < 2) {
        fprintf(stderr, "Error: expected a root compilation target (a file path).\n");
        return 1;
    }

    return !do_compilation(args[1]); // 0 means success, which is false, so flip it
}

static void print_unused_symbol_warnings(Context *ctx) {
    u64 len = shlenu(ctx->symbol_table);
    for (int i = 0; i < len; i++) {
        AstDecl *d = ctx->symbol_table[i].value;
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
        fprintf(stderr, "Error: failed to open file \"%s\".\n", path);
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

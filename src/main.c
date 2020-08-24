#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "headers/context.h"
#include "headers/parser.h"
#include "headers/lexer.h"
#include "headers/passes.h"
#include "headers/type.h"
#include "headers/ast.h"

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

// From parser.c, circular dependency in the header files.
Ast parse(Context *);

// THINK: for imports, just lex, parse and resolve all the files and then merge their scopes into one scope, then generate code for the symbols, symbols that are Sym_UNRESOLVED, can be ignored.

bool process_file(const char *file_path) {
    char *file_data = read_file(file_path); // read the file into a zero-terminated buffer.

    u64 lexer_delta = 0;
    u64 parser_delta = 0;
    u64 checker_delta = 0;

    SourceStats stats = (SourceStats){10}; // initialize all the fields to 10
    TokenList tokens;
    Context context;
    Ast ast;

    #define NEXT_STAGE_OR_QUIT() if (context.error_count > 0) goto end;

    init_context(&context, file_path);
    token_list_init(&tokens);
    lexer_init(&context.lexer, file_path, file_data);

    PROFILE(lexer, {
        if (!lexer_lex(&context, &tokens, &stats)) return 1;
    });

    if (tokens.len == 1) return 0;

    init_types(&context, &stats);
    parser_init(&context.parser, &tokens, &stats);

    PROFILE(parser, {
        ast = parse(&context);
    });

    token_list_free(&tokens);

    if (!context.decl_for_main)
        compile_error(&context, (Token){0}, "No entry point found. Please declare \"main\"");
    if (context.decl_for_main->tag != Decl_PROC)
        compile_error(&context, decl_tok(context.decl_for_main), "Entry point \"main\" must be a procedure");

    NEXT_STAGE_OR_QUIT();

    PROFILE(checker, {
        resolve_program(&context);
        NEXT_STAGE_OR_QUIT();
        check_ast(&context, &ast); // type and semantic checking
        NEXT_STAGE_OR_QUIT();
    });

    print_unused_symbol_warnings(&context);

end:
    {
        bool ret = (context.error_count > 0);

        if (ret) {
            printf("\n");
        }

        printf("Error count: %d\n", context.error_count);
        printf("Total time: %ldus\n", lexer_delta + parser_delta + checker_delta);
        printf("\tLexing took %ldus\n", lexer_delta);
        printf("\tParsing took %ldus\n", parser_delta);
        printf("\tInferring, resolving and checking took %ldus\n", checker_delta);
        printf("Parser used %lu nodes out of %lu allocated.\n", context.parser.node_count, context.parser.node_allocator.capacity);

        free_types(&context);
        parser_free(&context.parser, &ast);
        free_subtrees_and_blocks(&ast);
        lexer_free(&context.lexer);
        free(file_data);

        return ret;
    }
}

int main(int arg_count, char *args[]) {
    if (arg_count < 2) {
        fprintf(stderr, "Error: expected a root compilation target (a file path).\n");
        return 1;
    }

    return !process_file(args[1]); // 0 (good) is false in this case
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
    printf("\n");
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

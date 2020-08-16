#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#include "headers/context.h"
#include "headers/parser.h"
#include "headers/lexer.h"
#include "headers/passes.h"
#include "headers/type.h"

#define STB_DS_IMPLEMENTATION
#include "headers/stb/stb_ds.h"
#include "headers/stb/stretchy_buffer.h"

#define perfstats 1

static char *read_file(const char *path);

// From parser.c, circular dependency in the header files.
Ast parse(Context *);

void dump_parser_errors(const Parser *p) {
    for (int i = 0; i < p->error_msg_allocator.pos; i++) {
        const char c = p->error_msg_allocator.block[i];
        if (c == '\n') printf("\n");
        printf("%c", c);
    }
}

// THINK: for imports, just lex, parse and resolve all the files and then merge their scopes into one scope, then generate code for the symbols, symbols that are Sym_UNRESOLVED, can be ignored.

bool process_file(const char *file_path) {
    char *file_data = read_file(file_path); // read the file into a zero-terminated buffer.

    SourceStats stats = (SourceStats){10}; // initialize all the fields to 10
    TokenList tokens;
    Context context;
    Ast ast;

    init_context(&context, file_path);
    token_list_init(&tokens);
    lexer_init(&context.lexer, file_path, file_data);

    u64 lex_delta = 0;
    #if perfstats
        struct timeval lend, lstart;
        gettimeofday(&lstart, NULL);
    #endif

    if (!lexer_lex(&context, &tokens, &stats)) return 1;

    #if perfstats
        gettimeofday(&lend, NULL);
        /* In microseconds */
        lex_delta = (lend.tv_sec - lstart.tv_sec) * 1000000 + lend.tv_usec - lstart.tv_usec;
    #endif

    if (tokens.len == 1) return 0;

    init_types(&context, &stats);
    parser_init(&context.parser, &tokens, &stats);

    u64 parse_delta = 0;
    #if perfstats
        struct timeval pend, pstart;
        gettimeofday(&pstart, NULL);
    #endif

    ast = parse(&context);

    #if perfstats
        gettimeofday(&pend, NULL);
        /* In microseconds */
        parse_delta = (pend.tv_sec - pstart.tv_sec) * 1000000 + pend.tv_usec - pstart.tv_usec;
    #endif

    context.error_count += context.parser.error_count;

    token_list_free(&tokens);

    if (context.error_count > 0) {
        // dump_parser_errors(&context.parser);
        print_ast_errors(&context, &ast);
        goto end;
    }

    u64 check_delta = 0;
    #if perfstats
        struct timeval cend, cstart;
        gettimeofday(&cstart, NULL);
    #endif

    resolve_top_level(&context);

    if (context.error_count > 0)
        goto end;

    check_ast(&context, &ast); // type and semantic checking

    if (context.error_count > 0)
        goto end;

    #if perfstats
        gettimeofday(&cend, NULL);
        /* In microseconds */
        check_delta = (cend.tv_sec - cstart.tv_sec) * 1000000 + cend.tv_usec - cstart.tv_usec;
    #endif

    bool ret = (context.error_count > 0);

    if (ret) {
        printf("\n");
    }

end:
    printf("Lexing took %ldus\n", lex_delta);
    printf("Parsing took %ldus\n", parse_delta);
    printf("Inferring, resolving and checking took %ldus\n", check_delta);
    printf("Parser used %lu nodes out of %lu allocated.\n", context.parser.node_count, context.parser.node_allocator.capacity);
    printf("Error count: %d\n", context.error_count);

    free_types(&context);
    parser_free(&context.parser, &ast);
    lexer_free(&context.lexer);
    free(file_data);

    return ret;
}

// TODO disallow duplicate symbols

int main(int arg_count, char *args[]) {
    if (arg_count < 2) {
        fprintf(stderr, "Error: expected a root compilation target (a file path).\n");
        return 1;
    }

    return !process_file(args[1]); // 0 (good) is false in this case
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

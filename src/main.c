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

static char *read_file(const char *path);

//
// Allocate and zero out syntax
//   t: *T = alloct(1){0};
//

// From parser.c, circular dependency in the header files.
Ast parse(Context *);

void dump_parser_errors(const Parser *p) {
    for (int i = 0; i < p->error_msg_allocator.pos; i++) {
        const char c = p->error_msg_allocator.block[i];
        if (c == '\n') printf("\n");
        printf("%c", c);
    }
}

void dump_deferred_names(const Context *p) {
    if (!p->deferred_names) return;
    u64 len = sb_len(p->deferred_names);
    if (len == 0) return;
    for (int i = 0; i < len; i++) {
        printf("%s\n", p->deferred_names[i].text);
    }
}

int main(int arg_count, char *args[]) {
    if (arg_count < 2) {
        fprintf(stderr, "Error: expected a root compilation target (a file path).\n");
        return 1;
    }
    char *file_data = read_file(args[1]); // read the file into a zero-terminated buffer.

    SourceStats stats = (SourceStats){10}; // set all values to 10 to start with.
    TokenList tokens;
    Context context;
    Ast ast;

    init_context(&context, args[1]);
    token_list_init(&tokens);
    lexer_init(&context.lexer, args[1], file_data);

    u64 lex_delta = 0;
    #ifdef __linux__
        struct timeval end, start;
        gettimeofday(&start, NULL);
    #endif

    if (!lexer_lex(&context.lexer, &tokens, &stats)) return 1;

    #ifdef __linux__
        gettimeofday(&end, NULL);
        /* In microseconds */
        lex_delta = (end.tv_sec - start.tv_sec) * 1000000 + end.tv_usec - start.tv_usec;
    #endif

    if (tokens.len == 1) return 0;

    init_types(&context, &stats);
    parser_init(&context.parser, &tokens, &stats);

    ast = parse(&context);
    context.error_count += context.parser.error_count;

    token_list_free(&tokens);

    if (context.error_count > 0) {
        dump_parser_errors(&context.parser);
        print_ast_errors(&context, &ast);
        goto end;
    }

    // TODO maybe merge these idk
    if (check_types_were_declared(&context)) { // potentially could still error, and is likely to
        fill_in_types(&context, &ast); // and this relies on the last call not failing
    } else goto end;

    check_ast(&context, &ast); // type and semantic checking

    if (context.error_count > 0) {
        printf("\n");
    }

end:
    printf("Lexing took %ldus\n", lex_delta);
    printf("Parser used %lu nodes out of %lu allocated.\n", context.parser.node_count, context.parser.node_allocator.capacity);
    printf("Error count: %d\n", context.error_count);

    free_types(&context);
    parser_free(&context.parser, &ast);
    lexer_free(&context.lexer);
    arena_free(&context.scratch);
    free(file_data);

    return 0;
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

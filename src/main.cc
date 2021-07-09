// The entry point, this code invokes each step of the compilation pipeline.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "headers/arena.h"
#include "headers/lexer.h"
#include "headers/parser.h"
#include "headers/context.h"
#include "headers/passes.h"
#include "headers/ast.h"

#define STB_DS_IMPLEMENTATION
#include "headers/stb/stb_ds.h"

static void ensure_main_is_declared(Context *ctx);

/*
 * do enums
 * do bytecode VM
 * do polymorphism
 * do type aliases for identifiers
 * complete typename as expression
 * cleanup C++ warnings
 * do C code generation again
 * do LLVM
 * do multiple return values
 * do procedure types/header parsing as typename
*/

// No type mismatch error when assigning float to int
// 'Using' import
//
// For specific imports, just add it to the file scope and do inference and checking.
//   printf := #import "hi.mule" :: printf ??`

// TODO: write a game in the language (2d thing, simply concept).
//       fix bugs found during game dev
//       turn identifiers into Names at lex time
//       make int 32 bits again, this is weird

static Module *load_module(Context *ctx, char *path) {
    assert(path);
    char *file_data = read_file(path);
    if (!file_data) {
        ctx->error_count++;
        return NULL;
    }

    Lexer lexer;
    TokenList tokens;
    Parser parser;
    auto module = (Module *)malloc(sizeof(Module));

    lexer_init(&lexer, path, file_data);
    lexer.string_allocator = &ctx->string_allocator;
    if (!lexer_lex(&lexer, &tokens)) {
        ctx->error_count++;
        return NULL;
    }
    free(file_data);
    module->tokens = tokens;

    parser_init(&parser, module);
    parse(ctx, &parser, path);
    if (ctx->error_count > 0) {
        return NULL;
    }
    return module;
}

// Recursively load all the ASTs of imported modules into memory.
void load_required_modules(Module *in, Context *ctx) {
    for (u64 i = 0; i < in->ast.len; i++) {
        if (ctx->error_count > 0) return;

        AstDecl *decl = (AstDecl *)in->ast.nodes[i];
        if (!decl->expr || decl->expr->tag != Node_IMPORT) continue;
        
        char *path = ((AstImport *)decl->expr)->path;
        int module_index = shgeti(ctx->modules, path);
        if (module_index != -1) continue; // we already loaded this module

        Module *module = load_module(ctx, path);
        if (!module) continue;
        
        stbds_shput(ctx->modules, path, module);
        load_required_modules(module, ctx);
    }
}

static int cleanup_and_die(Context *ctx, Module *root) {
    int code = (ctx->error_count > 0 ? -1 : 0);
    token_list_free(&root->tokens);
    ast_free(&root->ast);
    free_context(ctx);
    free_types(ctx);
    return code;
}

int main(int arg_count, char **args) {
    if (arg_count < 2) {
        printf("sizeof(AstNode) = %ld\nsizeof(AstDecl) = %ld\nsizeof(AstTypeDecl) = %ld\n", sizeof(AstNode), sizeof(AstDecl), sizeof(AstTypeDecl));
        fprintf(stderr, "error: expected root compilation target (a single file path) as argument.\n");
        return -1;
    }
    char *path = args[1];

    Context context;
    Lexer lexer;
    TokenList tokens;
    Parser parser;
    Module module;

    char *file_data = read_file(path);
    if (!file_data) return -1;

    init_context(&context);
    init_types(&context);

    // Load the runtime
    Module *runtime_stuff = load_module(&context, "compiler.lang");
    if (context.error_count > 0) {
        token_list_free(&runtime_stuff->tokens);
        ast_free(&runtime_stuff->ast);
        return cleanup_and_die(&context, &module);
    }
 
    lexer_init(&lexer, path, file_data);
    lexer.string_allocator = &context.string_allocator; 
    if (!lexer_lex(&lexer, &tokens)) {
        free(file_data);
        free_context(&context);
        free_types(&context);
        return -1;
    }
    free(file_data);

    module.tokens = tokens;

    // Initialise the AST.
    parser_init(&parser, &module);
    // And immediately add all the runtimet stuff to it.
    for (u64 i = 0; i < runtime_stuff->ast.len; i++)
        ast_add(&module.ast, runtime_stuff->ast.nodes[i]);

    parse(&context, &parser, path);
    if (context.error_count > 0) return cleanup_and_die(&context, &module);

    load_required_modules(&module, &context);
    if (context.error_count > 0) return cleanup_and_die(&context, &module);

    resolve_module(&context, &module);
    if (context.error_count > 0) return cleanup_and_die(&context, &module);
    
    ensure_main_is_declared(&context);
    if (context.error_count > 0) return cleanup_and_die(&context, &module);

    check_ast(&context, &module.ast);
    if (context.error_count > 0) return cleanup_and_die(&context, &module);

    return cleanup_and_die(&context, &module);
    
    //
    // Make the build command to compile the C output.
    //
    char *output_path = generate_and_write_c_code(&context, &module.ast, path);
    u64 len = strlen("tcc -g -std=c99") + strlen(output_path) + strlen("-o ") + strlen("-Wno-discarded-qualifiers ") + strlen("-Wno-return-local-addr") + strlen("-Wno-builtin-declaration-mismatch") + 1;
    auto command = (char *)malloc(len);
    sprintf(command, "tcc -g -std=c99 %s -Wno-return-local-addr -Wno-discarded-qualifiers -Wno-builtin-declaration-mismatch -o output_bin", output_path);

    if (system(command) != 0) return cleanup_and_die(&context, &module);
    printf("Success.\n");

    return cleanup_and_die(&context, &module);
}

static void ensure_main_is_declared(Context *ctx) {
    if (!ctx->decl_for_main) {
        compile_error(ctx, Token{}, "no entry point found. Please declare \"main\"");
        return;
    } 

    Token main_decl_token = ctx->decl_for_main->token;
    if (ctx->decl_for_main->expr->tag != Node_PROCEDURE) {
        compile_error(ctx, main_decl_token, "entry point 'main' must be a procedure");
    }

    auto proc = (AstProcedure *)ctx->decl_for_main->expr;

    if ((proc->params && proc->params->len) || !proc->params) {
        compile_error(ctx, main_decl_token, "entry point 'main' must not take any arguments");
    }

    else if (ctx->decl_for_main->expr->resolved_type != ctx->type_void) {
        compile_error(ctx, main_decl_token, "entry point 'main' must return void");
    }
}

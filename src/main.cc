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
#include "headers/bytecode.h"

#define STB_DS_IMPLEMENTATION
#include "headers/stb/stb_ds.h"

#define IMPORT_RUNTIME 1

static void ensure_main_is_declared(Context *ctx);

/*
 * using
 * do bytecode VM
 * do polymorphism
 * do LLVM
 * do multiple return values
 * load the runtime into imported modules, not just the root one
 *
*/

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
    if (ctx->error_count > 0) return NULL;
    return module;
}

// Recursively load all the ASTs of imported modules into memory.
void load_required_modules(Module *in, Context *ctx) {
    for (u64 i = 0; i < in->ast.len; i++) {
        if (ctx->error_count > 0) return;

        auto decl = (AstDecl *)in->ast.nodes[i];
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
        printf("sizeof(AstNode) = %ld\nsizeof(AstDecl) = %ld\nsizeof(AstTypeDecl) = %ld\nsizeof(Interp) = %ld", sizeof(AstNode), sizeof(AstDecl), sizeof(AstTypeDecl), sizeof(Interp));
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
    // TODO rename file
#if IMPORT_RUNTIME
    constexpr char *runtime_module_name = "../compiler.lan";
    Module *runtime_stuff = load_module(&context, runtime_module_name);
    {
        // shput(context.modules, runtime_module_name, runtime_stuff);
        if (context.error_count > 0) {
            token_list_free(&runtime_stuff->tokens);
            ast_free(&runtime_stuff->ast);
            return cleanup_and_die(&context, &module);
        }
    }
#endif
 
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
    // And immediately add all the runtimet stuff to it.
    parser_init(&parser, &module);

    // Make the ast for the root/main module/file
    {
        parse(&context, &parser, path);
        if (context.error_count > 0) return cleanup_and_die(&context, &module);
    }

    // Recursively load all modules required by the code.
    // This includes files imported by other files.
    {
        load_required_modules(&module, &context);
        if (context.error_count > 0) return cleanup_and_die(&context, &module);
    }

    // Resolve only the main module, any external symbols that were used will also get resolved.
    {
        resolve_module(&context, &module);
        if (context.error_count > 0) return cleanup_and_die(&context, &module);
    }
    
    // Make sure the entry point is declared and has the proper signature.
    {
        ensure_main_is_declared(&context);
        if (context.error_count > 0) return cleanup_and_die(&context, &module);
    }

    // Do semantic and type-checking on the code.
    {
        check_ast(&context, &module.ast);
        if (context.error_count > 0) return cleanup_and_die(&context, &module);
    }

    // Compile the program to bytecode chunks that we can interpret and/or use to generate C output.
    {
        assert(sizeof(Instruction) == 8);
        Interp interp = compile_to_bytecode(&context, &module.ast, &module);
        assert(context.error_count == 0);
        interp_run(&interp);
        interp_free(&interp);
    }

    {
        // Write the C output to a file and return it's name.
        char *output_path = generate_and_write_c_code(&context, &module.ast, path);
        assert(context.error_count == 0);

        // Generate the command to invoke the C compiler on the output.
        auto command = (char *)malloc(1024);
        auto linker_flags = (char *)malloc(512);
        sprintf(command, "gcc -g -std=c99 %s -Wno-return-local-addr -Wno-discarded-qualifiers -Wno-builtin-declaration-mismatch -o output_bin -L.", output_path);
        for (u64 i = 0; i < context.link_libraries.len; i++) {
            auto lib = static_cast<AstLibrary *>(context.link_libraries.nodes[i]);
            sprintf(linker_flags, " -l%s", lib->library);
            strcat(command, linker_flags);
        }
        printf("comamnd: %s\n", command);

        // Compile the C code.
        // Exit early if it failed.
        if (system(command) != 0) return cleanup_and_die(&context, &module);

        // remove(output_path);
        printf("Success.\n");
    }

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

    else if (ctx->decl_for_main->expr->resolved_type->proc->return_type->resolved_type != ctx->type_void) {
        compile_error(ctx, main_decl_token, "entry point 'main' must return void");
    }
}

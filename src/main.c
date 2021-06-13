#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "headers/arena.h"
#include "headers/lexer.h"
#include "headers/parser.h"
#include "headers/context.h"
#include "headers/passes.h"

#include <assert.h>
#include "headers/stb/stb_ds.h"
void load_used_modules(Ast *module, Context *ctx) {
    for (u64 i = 0; i < module->len; i++) {
        AstNode *node = module->nodes[i];
        assert(is_decl(node));
        AstDecl *decl = (AstDecl *)node;
        if (decl->tag != Decl_VAR) continue;
        AstVar *var = (AstVar *)decl;
        if (!var->value && var->value->tag != Expr_IMPORT) continue;
        
        char *path = var->value->as.import.path;
        char *file_data = read_file(path);
        int module_i = shgeti(ctx->modules, path);
        if (module_i != -1) {
            continue;
        }
        assert(file_data); // TODO: scuffed
        Lexer lexer;
        lexer_init(&lexer, path, file_data);
        lexer.string_allocator = &ctx->string_allocator;
        TokenList tokens;
        assert(lexer_lex(&lexer, &tokens));
        free(file_data);
        Parser parser;
        parser_init(&parser, &tokens);
        Ast *tree = malloc(sizeof(Ast));
        parse(ctx, &parser, tree, path);
        if (ctx->error_count > 0) {
            parser_free(&parser, tree);
            return;
        }
        stbds_shput(ctx->modules, path, tree);
        load_used_modules(tree, ctx);
    }
}

int main(int arg_count, char **args) {
	if (arg_count < 2) {
		fprintf(stderr, "error: expected root compilation target (a single file path) as argument.\n");
		return -1;
	}
    char *path = args[1];

	Context context;
	init_context(&context);
    init_types(&context, NULL);
	
	char *data = read_file(path);
	if (!data) return -1;
 
	Lexer lexer;
	lexer_init(&lexer, path, data);
	lexer.string_allocator = &context.string_allocator;	
    TokenList tokens;
	if (!lexer_lex(&lexer, &tokens)) return -1;

	Parser parser;
    Ast the_ast;
    parser_init(&parser, &tokens);
	parse(&context, &parser, &the_ast, path);
	if (context.error_count > 0) return -1;

    load_used_modules(&the_ast, &context);
	if (context.error_count > 0) {
        // TODO free
        return -1;
    }

    resolve_module(&context, &the_ast);
	if (context.error_count > 0) return -1;

    check_ast(&context, &the_ast);
    if (context.error_count > 0) return -1;

    //
    // Make the build command to compile the C output.
    //
    char *output_path = generate_and_write_c_code(&context, &the_ast, path);
    u64 len = strlen("gcc -g -std=c99") + strlen(output_path) + strlen("-o ") + strlen("-Wno-discarded-qualifiers ") + strlen("-Wno-return-local-addr") + strlen("-Wno-builtin-declaration-mismatch") + 1;
    char *command = arena_alloc(&context.scratch, len);
    sprintf(command, "gcc -g -std=c99 %s -Wno-return-local-addr -Wno-discarded-qualifiers -Wno-builtin-declaration-mismatch -o output_bin", output_path);

    system(command);
	printf("It ran.\n");

	parser_free(&parser, &the_ast);
	free_context(&context);
	// free_types(&context.type_table);
    return 0;
}

//
// TODO: Imports inside imports
//       Function calls from modules
//       'Using' import
//
//       Local imports. Maybe. Could disallow them.
// 
// To query for a function in an external module: lex, parse, iterate scope and check it. Copy declarations 
// into the current Scope.
// Checking and resolution idk yet.
// Do code gen only for main Scope.
//
// For specific imports, just add it to the file scope and do inference and checking.
//   printf := #import "hi.mule" :: printf ??`
//
// Make a fucking Scope abstraction that declarations can point to the scope they're defined in.
// Make some way to know if a declaration is from that file or not, I can make a flag for the variables.
// 
// Basically instead of looking for the imported stuff in the file scope, just insert a thing that says
// "hey go look in this file and check that function"

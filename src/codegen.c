#include "headers/passes.h"
#include "headers/context.h"
#include "headers/ast.h"
#include "headers/type.h"
#include "headers/arena.h"

#include "headers/stb/stb_ds.h"

#include <stdio.h>
#include <string.h>
#include <assert.h>

static FILE *output;
static Name *name_for_main;

static void emit_boilerplate(const char *file_name);
static void emit_c_for_var(AstVar *var);
static void emit_c_for_local_var(AstVar *var);
static void emit_c_for_expr(AstExpr *expr);
static void emit_c_for_stmt(AstNode *stmt);
static void emit_c_for_struct(AstStruct *def, char *name);

static void emit_c_for_type(Type *type) {
    assert(type);

    switch (type->kind) {
    case Type_PRIMITIVE:
        if (type->size == 0) {
            fprintf(output, "void");
            break;
        }
        fprintf(output, "%s", type->name);
        break;
    case Type_STRUCT:
        fprintf(output, "struct %s", type->name);
        return;
    case Type_ENUM:
        fprintf(output, "enum %s", type->name);
        return;
    case Type_ANON_STRUCT:
        emit_c_for_struct(&type->data.user->as.stmt.as._struct, NULL);
        return;
    case Type_POINTER:
        if (!type->data.base) { // generic pointer type, probably from a null literal
            fprintf(output, "void *");
            return;
        }
        emit_c_for_type(type->data.base);
        fprintf(output, "*");
        return;
    case Type_ALIAS:
        emit_c_for_type(type->data.alias_of);
        return;
    case Type_ARRAY:
        emit_c_for_type(type->data.base);
        return;
    case Type_UNRESOLVED:
        fprintf(output, "Internal error: Type_UNRESOLVED is not meant to have code gen'd for it.\n");
        return;
    default:
        fprintf(stderr, "Internal compiler error: type not covered in emit_c_for_type switch; type kind is %d\n", type->kind);
    }
}

static void emit_deferred_stmts(AstBlock *block) {
    for (int i = block->deferred->len-1; i >= 0; i--) {
        AstNode *node = block->deferred->nodes[i];
        emit_c_for_stmt(node);
        fprintf(output, ";\n");
    }
    if (block->parent) emit_deferred_stmts(block->parent);
}

static void emit_c_for_call(AstCall *call) {
    if (call->name->as.name == name_for_main) {
        fprintf(output, "__compiler_main()");
        return;
    }
    if (call->calling->foreign_link_name) {
        Token t = expr_tok(call->calling->foreign_link_name);
        fprintf(output, "%s(", t.text);
    } else {
        if (call->name->tag == Expr_BINARY) {
            AstBinary *selector = (AstBinary *)call->name;
            assert(selector->right->tag == Expr_NAME);
            fprintf(output, "%s(", selector->right->as.name->text);
        } else if (call->name->tag == Expr_NAME) {
            fprintf(output, "%s(", call->name->as.name->text);
        }
    }
    if (call->params) {
        for (int i = 0; i < call->params->len; i++) {
            emit_c_for_expr((AstExpr *)call->params->nodes[i]);
            if (i < call->params->len-1) {
                fprintf(output, ", ");
            }
        }
    }
    fprintf(output, ")");
}

static void emit_c_for_assignment(AstBinary *ass) {
    emit_c_for_expr(ass->left);
    fprintf(output, " ");
    switch (ass->op) {
    case Token_PLUS_EQUAL: fprintf(output, "+"); break;
    case Token_MINUS_EQUAL: fprintf(output, "-"); break;
    case Token_STAR_EQUAL: fprintf(output, "*"); break;
    case Token_SLASH_EQUAL: fprintf(output, "/"); break;
    }
    fprintf(output, "= ");
    emit_c_for_expr(ass->right);
}

static void emit_c_for_expr(AstExpr *expr) {
    Token t = expr_tok(expr);
    switch (expr->tag) {
    case Expr_STRING:
        // -2 because quotes are counted in the length of the token.
        // TODO fix this at lexer level.
        fprintf(output, "__make_string(\"%s\", %d)", t.text, t.length-2);
        return;
    case Expr_NULL:
        fprintf(output, "NULL");
        return;
    case Expr_CALL:
        emit_c_for_call((AstCall *)expr);
        return;
    case Expr_BOOL: {
        bool val = ((AstLiteral *)expr)->data.boolean;
        fprintf(output, val ? "true" : "false");
        return;
    } break;
    case Expr_INDEX: {
        AstArrayIndex *i = (AstArrayIndex *)expr;
        emit_c_for_expr(i->name);
        fprintf(output, "[");
        emit_c_for_expr(i->index);
        fprintf(output, "]");
    } break;
    case Expr_BINARY: {
        AstBinary *binary = (AstBinary *)expr;
        if (binary->left->resolved_type->kind == Type_ENUM) {
            fprintf(output, "%s__%s", binary->left->resolved_type->name, binary->right->as.name->text);
            return;
        }
        emit_c_for_expr(binary->left);
        switch (binary->op) {
        case Token_AMP_AMP: fprintf(output, "&&"); break;
        case Token_BAR_BAR: fprintf(output, "||"); break;
        case Token_LESS: fprintf(output, "<"); break;
        case Token_GREATER: fprintf(output, ">"); break;
        case Token_BANG_EQUAL: fprintf(output, "!="); break;
        case Token_GREATER_EQUAL: fprintf(output, ">="); break;
        case Token_LESS_EQUAL: fprintf(output, "<="); break;
        case Token_EQUAL_EQUAL: fprintf(output, "=="); break;
        case Token_PLUS: fprintf(output, "+"); break;
        case Token_MINUS: fprintf(output, "-"); break;
        case Token_SLASH: fprintf(output, "/"); break;
        case Token_STAR: fprintf(output, "*"); break;
        case Token_DOT: {
            if (binary->left->resolved_type->kind == Type_POINTER)
                fprintf(output, "->");
            else
                fprintf(output, ".");
        } break;
        default: assert(false);
        }
        emit_c_for_expr(binary->right);
        return;
    } break;
    case Expr_UNARY: {
        AstUnary *unary = (AstUnary *)expr;
        switch (unary->op) {
        case Token_MINUS: fprintf(output, "-"); break;
        case Token_CARAT: fprintf(output, "&"); break;
        case Token_STAR: fprintf(output, "*"); break;
        case Token_BANG: fprintf(output, "!"); break;
        default: assert(false);
        }
        emit_c_for_expr(unary->expr);
        return;
    } break;
    case Expr_PAREN:
        fprintf(output, "(");
        emit_c_for_expr(((AstParen *)expr)->sub_expr);
        fprintf(output, ")");
        return;
    case Expr_CAST: {
        AstCast *cast = (AstCast *)expr;
        fprintf(output, "(cast(");
        emit_c_for_type(cast->typename->as.type);
        fprintf(output, ")");
        emit_c_for_expr(cast->expr);
        fprintf(output, ")");
        return;
    } break;
    }
    fprintf(output, "%.*s", t.length, t.text);
}

static void emit_c_for_stmt(AstNode *stmt) {
    switch (stmt->tag) {
    case Node_DEFER:
        // No code is generated for a defer
        // they are accumulated during the resolution phase
        // and then at the end of a block, the deferred
        // statements are appended in reverse order.
        fprintf(output, "// defer'd statement");
        return;
    case Node_VAR:
        emit_c_for_local_var((AstVar *)stmt);
        break;
    case Node_CALL:
        emit_c_for_call((AstCall *)stmt);
        break;
    case Node_ASSIGN:
        emit_c_for_assignment((AstBinary *)stmt);
        break;
    case Node_BLOCK: {
        AstBlock *block = (AstBlock *)stmt;
        for (int i = 0; i < block->statements->len; i++) {
            emit_c_for_stmt(block->statements->nodes[i]);
            fprintf(output, ";\n");
        }
    } break;
    case Node_IF: {
        // if (/*condition*/) {
        //   ...           
        // }
        AstIf *iff = (AstIf *)stmt;
        fprintf(output, "if (");
        emit_c_for_expr(iff->condition);
        fprintf(output, ") {\n");
        emit_c_for_stmt((AstNode *)iff->block_or_stmt);
        fprintf(output, ";\n");
        fprintf(output, "}");

        if (iff->other_branch) {
            fprintf(output, "else ");
            if (iff->other_branch->tag == Stmt_BLOCK) {
                fprintf(output, "{\n");
                emit_c_for_stmt((AstNode *)iff->other_branch);
                fprintf(output, "}");
            } else {
                emit_c_for_stmt((AstNode *)iff->other_branch);
            }
        }
    } break;
    case Node_WHILE: {
        AstWhile *w = (AstWhile *)stmt;
        fprintf(output, "while (");
        emit_c_for_expr(w->condition);
        fprintf(output, ") {\n");
        emit_c_for_stmt((AstNode *)w->block);
        fprintf(output, "}");
    } break;
    case Node_RETURN: {
        AstReturn *ret = (AstReturn *)stmt;
        emit_deferred_stmts(ret->owning);
        fprintf(output, "return");
        if (ret->expr) {
            fprintf(output, " ");
            emit_c_for_expr(ret->expr);
        }
    } break;
    case Node_USING: return;
    default: assert(false);
    }
}

static void emit_c_for_struct(AstStruct *def, char *name) {
    AstBlock *block = (AstBlock *)def->members;
    fprintf(output, "struct");
    if (name) fprintf(output, " %s", name);
    fprintf(output, " {\n");
    for (int i = 0; i < block->statements->len; i++) {
        AstVar *var = (AstVar *)block->statements->nodes[i];
        fprintf(output, "\t");
        emit_c_for_var(var);
        fprintf(output, ";\n");
    }
    fprintf(output, "}");
}

static void emit_c_for_enum(AstEnum *def, char *name) {
    fprintf(output, "enum");
    if (name) fprintf(output, " %s", name);
    fprintf(output, " {\n");
    FlatTable it = table_flatten(&def->fields);
    for (int i = 0; i < it.num_entries; i++) {
        AstVar *var = (AstVar *)it.pairs[i].value;
        fprintf(output, "\t");
        fprintf(output, "%s__%s = %d", name, var->name->text, i);
        fprintf(output, ",\n");
    }
    fprintf(output, "}");
    free(it.pairs);
}

static void emit_c_for_var(AstVar *var) {
    Name *name = var->name;
    Type *type = NULL;
    if (var->flags & VAR_IS_INFERRED) {
        type = var->value->resolved_type;
    } else {
        type = var->typename->as.type;
    }
    if (type->kind == Type_IMPORT) return;
    emit_c_for_type(type);
    fprintf(output, " %s", name->text);
    if (type->kind == Type_ARRAY) {
        fprintf(output, "[1024]"); // TODO dynamic array abstraction, plus store size info on Type
    } else if (type->kind == Type_STRUCT) {
        fprintf(output, ";\n__%s_initer(&%s)", type->name, name->text);
    }
}

static void emit_c_for_local_var(AstVar *var) {
    emit_c_for_var(var);
    if (var->flags & VAR_IS_INITED) {
        fprintf(output, " = ");
        emit_c_for_expr(var->value);
    }
}

static void emit_c_for_proc_header(AstProcedure *proc) {
    emit_c_for_type(proc->return_type->as.type); // TODO arrays will be emitted as arrays on function signatures
    if (proc->flags & PROC_IS_FOREIGN && proc->foreign_link_name) {
        Token link_name = expr_tok(proc->foreign_link_name);
        fprintf(output, " %s(", link_name.text);
    } else {
        fprintf(output, " %s(", proc->name->text);
    }
    if (proc->params) {
        int params_length = proc->params->len;
        for (u64 i = 0; i < params_length; i++) {
            AstDecl *p = (AstDecl *)proc->params->nodes[i];
            if (((AstVar *)p)->flags & VAR_IS_VARARGS) {
                fprintf(output, "...");
                break;
            }
            emit_c_for_var((AstVar *)p);
            if (i < params_length-1 || proc->var_args) {
                fprintf(output, ", ");
            }
        }
        if (proc->var_args) fprintf(output, "...");
    }
    fprintf(output, ")");
}

static void emit_c_for_proc(AstProcedure *proc, bool entry_point) {
    if (entry_point) {
        fprintf(output, "void __compiler_main()");
    } else {
        emit_c_for_proc_header(proc);
    }
    fprintf(output, " {\n");
    if (entry_point) {
        fprintf(output, "__global_initializers();\n");
    }
    AstBlock *block = (AstBlock *)proc->block;
    for (u64 i = 0; i < block->statements->len; i++) {
        AstNode *node = block->statements->nodes[i];
        emit_c_for_stmt(node);
        fprintf(output, ";\n");
    }
    emit_deferred_stmts(block);
    fprintf(output, "}\n");
}

char *generate_and_write_c_code(Context *ctx, Ast *ast, const char *file_name) {
    name_for_main = make_name_from_string(ctx, "main");

    const char *postfix = "_generated.c";
    u64 len = strlen(file_name) + strlen(postfix) + 1;
    char *output_file = arena_alloc(&ctx->scratch, len);
    strcpy(output_file, file_name);
    strcat(output_file, postfix);

    output = fopen(output_file, "w");
    assert(output);

    emit_boilerplate(file_name);

    //
    // Generate forward decls
    //
    for (u64 i = 0; i < ast->len; i++) {
        if (ast->nodes[i]->tag == Node_IMPORT) continue;

        AstDecl *decl = (AstDecl *)ast->nodes[i];
        if (decl->status == Status_UNRESOLVED) continue; // Don't emit code for unused declarations

        switch (decl->tag) {
        case Decl_VAR:
            emit_c_for_var((AstVar *)decl);
            break;
        case Decl_TYPEDEF: {
            char *name = decl->name->text;
            Type *type = decl->as.type;
            if (type->data.user->tag == Node_STRUCT) {
                fprintf(output, "struct %s;\n", name);
                fprintf(output, "void __%s_initer(struct %s*)", name, name);
                break;
            } else {
                continue;
            }
        } break;
        case Decl_PROC: {
            if (decl == ctx->decl_for_main)
                fprintf(output, "void compiler_main()");
            else 
                emit_c_for_proc_header((AstProcedure *)decl);
        } break;
        }
        fprintf(output, ";\n");
    }

    fprintf(output, "\n");

    //
    // Generate struct and enum bodies
    //
    for (u64 i = 0; i < ast->len; i++) {
        if (ast->nodes[i]->tag == Node_IMPORT) continue;

        AstDecl *decl = (AstDecl *)ast->nodes[i];
        if (decl->status == Status_UNRESOLVED) continue;
        if (decl->tag == Decl_TYPEDEF) {
            Type *type = decl->as.type;
            if (type->data.user->tag == Node_STRUCT) {
                emit_c_for_struct((AstStruct *)type->data.user, decl->name->text);
                fprintf(output, ";\n");
            } else if (type->data.user->tag == Node_ENUM) {
                emit_c_for_enum((AstEnum *)type->data.user, decl->name->text);
                fprintf(output, ";\n");
            }
        }
    }

    fprintf(output, "\n");
    
    //
    // Generate implicit initializers for structs.
    //
    for (u64 i = 0; i < ast->len; i++) {
        AstDecl *decl = (AstDecl *)ast->nodes[i];
        if (decl->status == Status_UNRESOLVED) continue;
        if (decl->tag != Decl_TYPEDEF) {
            continue;
        }
        Type *def = decl->as.type;
        if (def->data.user->tag != Node_STRUCT) continue;

        char *name = decl->name->text;
        AstBlock *members = &def->data.user->as.stmt.as._struct.members->as.block;
        fprintf(output, "void __%s_initer(struct %s* s) {\n", name, name);
        for (int i = 0; i < members->statements->len; i++) {
            AstVar *var = (AstVar *)members->statements->nodes[i];
            if (!(var->flags & VAR_IS_INITED)) continue;
            fprintf(output, "s->%s = ", var->name->text);
            emit_c_for_expr(var->value);
            fprintf(output, ";\n");
        }
        fprintf(output, "}\n");
    }

    fprintf(output, "\n");

    //
    // Generate procedure decls
    //
    for (u64 i = 0; i < ast->len; i++) {
        if (ast->nodes[i]->tag == Node_IMPORT) continue;

        AstDecl *decl = (AstDecl *)ast->nodes[i];

        // Don't emit code for unused declarations
        if (decl->status == Status_UNRESOLVED) continue;
        if (decl->tag == Decl_PROC) {
            AstProcedure *proc = (AstProcedure *)decl;
            if (proc->flags & PROC_IS_FOREIGN) continue;
            emit_c_for_proc(proc, (decl == ctx->decl_for_main));
            fprintf(output, "\n");
        }
    }

    //
    // Generate initialization code for global variables
    //
    fprintf(output, "void __global_initializers() {\n");
    for (u64 i = 0; i < ast->len; i++) {
        AstDecl *decl = (AstDecl *)ast->nodes[i];
        if (decl->tag != Decl_VAR) continue;
        AstVar *var = (AstVar *)decl;
        if (var->value->resolved_type->kind == Type_IMPORT) continue;
        if (!(var->flags & VAR_IS_INITED)) continue;
        fprintf(output, "%s = ", var->name->text);
        emit_c_for_expr(var->value);
        fprintf(output, ";\n");
    }
    fprintf(output, "}\n");

    fprintf(output, "int main(int __argcount, char *__args[]) {\n");
    fprintf(output, "__compiler_main();\n");
    fprintf(output, "}\n");

    fclose(output);
    return output_file;
}

static void emit_boilerplate(const char *file_name) {
    fprintf(output, "// This code was generated by a compiler, from %s\n", file_name);
    fprintf(output, "// http://github.com/anthony-y/compiler\n");
    fprintf(output, "typedef signed   char s8;\n");
    fprintf(output, "typedef unsigned char u8;\n");
    fprintf(output, "typedef signed   short s16;\n");
    fprintf(output, "typedef unsigned short u16;\n");
    fprintf(output, "typedef signed   int s32;\n");
    fprintf(output, "typedef unsigned int u32;\n");
    fprintf(output, "typedef signed   long int s64;\n");
    fprintf(output, "typedef unsigned long int u64;\n");
    fprintf(output, "typedef float  f32;\n");
    fprintf(output, "typedef double f64;\n");
    fprintf(output, "typedef struct string {u8 *data; u64 length;} string;\n");
    fprintf(output, "typedef enum bool {false, true} bool;\n");
    fprintf(output, "#define cast \n");
    fprintf(output, "#define NULL (void *)0\n");
    fprintf(output, "\n");
    fprintf(output, "void __global_initializers();\n");
    fprintf(output, "static inline string __make_string(u8 *data, u64 length) {\n");
    fprintf(output, "    return (string){.data=data, .length=length};\n");
    fprintf(output, "}\n");
    fprintf(output, "\n");
}

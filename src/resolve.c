// resolve.c contains code which recursively traverses an abstract syntax tree
// to resolve identifiers to the symbols which declared them, resolve expressions to types
// and to apply type inference to variable declarations which specify it.
#include "headers/common.h"
#include "headers/ast.h"
#include "headers/type.h"
#include "headers/context.h"
#include "headers/passes.h"

#include "headers/stb/stb_ds.h"

#include <assert.h>

static void resolve_procedure(AstDecl *procsym, Context *ctx);
static Type *resolve_var(AstDecl *varsym, Context *ctx);
static Type *resolve_selector(Context *ctx, AstBinary *accessor);
static Type *resolve_expression(AstExpr *expr, Context *ctx);
static Type *resolve_type(Context *ctx, Type *type, bool cyclic_allowed);
static void resolve_block(Context *ctx, AstBlock *block);

// stbds arrays
static AstProcedure **proc_stack = NULL;
static AstBlock **scope_stack = NULL;

static void resolve_assignment_expr(AstExpr *assign, Context *ctx) {
    if (assign->tag == Expr_UNARY) {
        AstUnary *unary = (AstUnary *)assign;
        resolve_assignment_expr(unary->expr, ctx);
    } else if (assign->tag == Expr_BINARY) {
        AstBinary *bin = (AstBinary *)assign;
        if (!is_assignment(*bin)) {
            compile_error(ctx, expr_tok(assign), "expected assignment"); // TODO maybe bad
            return;
        }
        resolve_expression(bin->left, ctx);
        resolve_expression(bin->right, ctx);
    } else {
        compile_error(ctx, expr_tok(assign), "expected assignment");
    }
}

// Resolves the dependencies of an assignment statement,
static void resolve_assignment(AstNode *ass, Context *ctx) {
    assert(ass->tag == Node_ASSIGN);
    AstStmt *stmt = (AstStmt *)ass;
    resolve_assignment_expr(&stmt->as.assign, ctx);
}

static AstProcedure *resolve_call(AstNode *callnode, Context *ctx) {
    Token tok = callnode->token;
    AstCall *call = (AstCall *)callnode;
    char *str_name = call->name->as.name->text;
    u64 symbol_index = shgeti(ctx->symbol_table, str_name);
    if (symbol_index == -1) {
        compile_error(ctx, tok, "call to undeclared procedure \"%s\"", str_name);
        return NULL;
    }
    if (call->params) for (int i = 0; i < call->params->len; i++) {
        AstExpr *arg = (AstExpr *)call->params->nodes[i];
        resolve_expression(arg, ctx);
    }
    AstDecl *hopefully_proc = ctx->symbol_table[symbol_index].value;
    if (hopefully_proc->tag != Decl_PROC) {
        compile_error(ctx, tok, "attempted to call \"%s\", but it's not a procedure", str_name);
        return NULL;
    }
    if (hopefully_proc->status == Status_UNRESOLVED) {
        resolve_procedure(hopefully_proc, ctx);
    }
    AstProcedure *calling = (AstProcedure *)hopefully_proc;
    call->calling = calling;
    return calling;
}

static Type *resolve_expression_1(AstExpr *expr, Context *ctx, AstDecl *target) {
    Token t = expr_tok(expr);
    switch (expr->tag) {
    case Expr_STRING: return ctx->type_string;
    case Expr_INT:    return ctx->type_int;
    case Expr_BOOL:   return ctx->type_bool;
    case Expr_NULL:   return ctx->null_type;

    case Expr_CALL: {
        AstProcedure *resolved = resolve_call((AstNode *)expr, ctx);
        if (!resolved) return NULL;
        return resolved->return_type->as.type;
    } break;
    case Expr_UNARY: {
        AstUnary *unary = (AstUnary *)expr;
        Type *expr_type = resolve_expression(unary->expr, ctx);
        if (unary->op == Token_STAR) {
            if (expr_type->kind != Type_POINTER) {
                compile_error(ctx, t, "expected pointer operand to dereference");
                return NULL;
            }
            return expr_type->data.base;
        }
        if (unary->op == Token_CARAT) {
            return make_pointer_type(expr_type);
        }
        return expr_type;
    } break;
    case Expr_NAME: {
        Name *name = expr->as.name;

        AstProcedure *in = stbds_arrlast(proc_stack);
        AstBlock *scope = stbds_arrlast(scope_stack);

        AstDecl *var = NULL;

        if (!scope) var = shget(ctx->symbol_table, name->text); // global scope
        else var = lookup_local(ctx, in, name, scope);

        if (!var) {
            compile_error(ctx, t, "undeclared identifier \"%s\"", name->text);
            return NULL;
        }

        if (var->tag != Decl_VAR) {
            compile_error(ctx, t, "\"%s\" was used like a variable, but it isn't one", name->text);
            return NULL;
        }

        assert(var->name == name);

        name->resolved_decl = var;

        if (var->status == Status_UNRESOLVED) {
            if (!(var->flags & DECL_IS_TOP_LEVEL)) {
                compile_error(ctx, t, "local variable \"%s\" was used before it was declared", name->text);
                return NULL;
            }
            Type *resolved_type = resolve_var(var, ctx);
            return resolved_type;
        }

        else if (var->status == Status_RESOLVING) {
            compile_error(ctx, t, "initial instantiation of variable \"%s\" mentions itself", name->text);
            return NULL;
        }
        return var->as.var.typename->as.type;
    } break;
    case Expr_CAST: {
        AstCast *cast = (AstCast *)expr;
        resolve_expression(cast->expr, ctx);
        return cast->typename->as.type;
    } break;
    case Expr_INDEX: {
        AstArrayIndex *index = (AstArrayIndex *)expr;
        Type *resolved_name = resolve_expression(index->name, ctx);
        if (!resolved_name) return NULL;
        resolve_expression(index->index, ctx);
        return resolved_name->data.base;
    } break;

    case Expr_BINARY: {
        AstBinary *bin = (AstBinary *)expr;
        if (bin->op == Token_DOT) {
            return resolve_selector(ctx, bin);
        }
        if (is_binary_comparison(*bin)) {
            return ctx->type_bool;
        }
        if (is_assignment(*bin)) {
            resolve_assignment_expr(expr, ctx);
            if (bin->op != Token_EQUAL) { // it's a +=, -=, *= or /=
                return ctx->type_int; // so its type must be integer
            }
        }

        // Should only be maths operators left
        assert(bin->op == Token_PLUS || bin->op == Token_MINUS || bin->op == Token_SLASH || bin->op == Token_STAR);

        Type *left_type = resolve_expression(bin->left, ctx);
        Type *right_type = resolve_expression(bin->right, ctx);

        // You can add and substract to/from pointers
        if (bin->op == Token_PLUS || bin->op == Token_MINUS) {
            if (left_type->kind == Type_POINTER)
                return left_type;
            if (right_type->kind == Type_POINTER)
                return left_type;
        }

        return ctx->type_int;
    } break;

    case Expr_PAREN: {
        return resolve_expression(((AstParen *)expr)->sub_expr, ctx);
    } break;
    }
    assert(false);
    return NULL;
}

static Type *resolve_expression(AstExpr *expr, Context *ctx) {
    if (!expr) return NULL;
    Type *re = resolve_expression_1(expr, ctx, NULL);
    expr->resolved_type = re;
    return re;
}

static Type *resolve_expression_for_variable(Context *ctx, AstExpr *expr, AstDecl *target) {
    if (!expr) return NULL;
    Type *re = resolve_expression_1(expr, ctx, target);
    expr->resolved_type = re;
    return re;
}

void resolve_struct(AstStruct *def, Context *ctx) {
    SymbolTable *table = def->members->as.block.symbols;
    u64 len = shlenu(table);
    for (int i = 0; i < len; i++) {
        AstDecl *field = table[i].value;
        if (field->tag != Decl_VAR) {
            compile_error(ctx, decl_tok(field), "only variable declarations are valid inside a struct body"); // this is semantic checking but we have to do it here otherwise it's a nightmare
            continue;
        }
        resolve_var(field, ctx);
    }
}

// Resolves an unresolved type to it's "real" type
static Type *resolve_type(Context *ctx, Type *type, bool cyclic_allowed) {
    Token t = ((AstNode *)type)->token; // TODO doesnt work lol

    if (type->kind == Type_PRIMITIVE) return type;
    if (type->kind == Type_ANON_STRUCT) {
        resolve_struct(&type->data.user->as._struct, ctx);
        return type;
    }

    //
    // Pointers and arrays might have unresolved types in their sub-types.
    // We resolve those and wrap them back up each time.
    //
    if (type->kind == Type_POINTER)
        return make_pointer_type(resolve_type(ctx, type->data.base, true)); // ... cyclic_allowed should only be passed as true here and below

    if (type->kind == Type_ARRAY)
        return make_array_type(resolve_type(ctx, type->data.base, true));

    #if 0
    if (type->kind != Type_UNRESOLVED) {
        // For types which were declared before they are used, the declaration
        // won't get resolved at top level, so we just do it here.
        AstDecl *types_decl = shget(ctx->symbol_table, type->name);
        assert(types_decl);
        types_decl->status = Status_RESOLVED;
        return type;
    }
    #endif

    // Unresolved types (that is, types which are used before they are defined) are returned
    // as placeholder types, and are not put in the type table. If they then go on to be defined in the source code, they will be placed into the type table.
    // Here, we lookup the type in the type table. If it is not found, then it was never declared.
    // ...
    u64 i = shgeti(ctx->type_table, type->name);
    if (i == -1) {
        compile_error(ctx, t, "undeclared type \"%s\"", type->name);
        return NULL;
    }

    // ... if it was we store it here
    Type *real_type = ctx->type_table[i].value;

    // Next we'll look up the actual declaration of the type.
    u64 type_i = shgeti(ctx->symbol_table, type->name);
    AstDecl *sym = ctx->symbol_table[type_i].value;

    if (sym->status == Status_RESOLVED)
        return real_type;

    // If the declaration is already resolving...
    if (sym->status == Status_RESOLVING) {
        if (cyclic_allowed) {
            sym->status = Status_RESOLVED;
            return real_type;
        }
        compile_error(ctx, decl_tok(sym), "type definition for \"%s\" directly mentions itself", type->name);
        return NULL;
    }

    sym->status = Status_RESOLVING; // the above if statements works because of this

    if (sym->tag != Decl_TYPEDEF) {
        compile_error(ctx, t, "\"%s\" is not a type", type->name);
        return NULL;
    }

    AstTypedef *my_typedef = (AstTypedef *)sym;
    if (my_typedef->of->tag == Node_STRUCT) {
        resolve_struct(&my_typedef->of->as.stmt.as._struct, ctx);
    }

    sym->status = Status_RESOLVED;

    return real_type;
}

// Resolves the dependencies of a selector and returns the type of the field it selects.
static Type *resolve_selector(Context *ctx, AstBinary *accessor) { // TODO rename to resolve_selector
    assert(accessor->op == Token_DOT);

    Name *rhs = accessor->right->as.name;
    Type *lhs_type = resolve_expression(accessor->left, ctx);

    if (!lhs_type) return NULL; // resolve_expression will have already errored, so we can just exit

    if (lhs_type->kind == Type_UNRESOLVED) {
        lhs_type = resolve_type(ctx, lhs_type, false);
        assert(lhs_type);
    }

    // TODO kinda rough
    if (lhs_type == ctx->type_string || (lhs_type->kind == Type_POINTER && lhs_type->data.base == ctx->type_string)) {
        if (rhs == make_namet(ctx, "data")) {
            static Type *data_type;
            data_type = make_pointer_type(ctx->type_u8);
            return data_type;
        } else if (rhs == make_namet(ctx, "length")) {
            return ctx->type_u64;
        } else {
            compile_error(ctx, expr_tok(accessor->left), "type \"string\" has no field \"%s\"", rhs->text);
            return NULL;
        }
    }

    // In English: throw an error if the type of the left hand side is not either:
    //  - a struct or anonymous struct
    //  - a pointer, the base type of which is a struct or anonymous struct
    if (lhs_type->kind != Type_STRUCT && lhs_type->kind != Type_ANON_STRUCT &&
        (lhs_type->kind != Type_POINTER || (lhs_type->data.base->kind != Type_STRUCT && lhs_type->data.base->kind != Type_ANON_STRUCT))) {
        compile_error(ctx, expr_tok(accessor->left), "attempt to access member in non-struct value");
        return NULL;
    }

    // If it was a pointer, unwrap it, but only by one "level",
    // selectors shouldn't be able to reach into far-down structs in pointers.
    if (lhs_type->kind == Type_POINTER) {
        lhs_type = lhs_type->data.base;
    }

    AstStruct *struct_def = &lhs_type->data.user->as._struct;
    AstDecl *field = lookup_struct_field(struct_def, rhs);
    if (!field) {
        compile_error(ctx, expr_tok(accessor->right), "no such field as \"%s\" in struct field access", rhs->text);
        return NULL;
    }
    assert(field->tag == Decl_VAR); // should have been checked by now
    return field->as.var.typename->as.type;
}

// Resolves the dependencies and type of a variable declaration,
// and apply type inference if needed.
static Type *resolve_var(AstDecl *decl, Context *ctx) {
    AstVar *var = (AstVar *)decl;
    Type **specified_type = &var->typename->as.type;
    if (decl->status == Status_RESOLVED)
        return *specified_type;

    if (!(var->flags & VAR_IS_INFERRED)) {
        if (var->typename->as.type->kind == Type_ANON_STRUCT) {
            resolve_struct(&var->typename->as.type->data.user->as._struct, ctx);
        } else {
            *specified_type = resolve_type(ctx, *specified_type, false);
        }
    }

    if (!(var->flags & VAR_IS_INITED)) {
        decl->status = Status_RESOLVED;
        return *specified_type;
    }

    decl->status = Status_RESOLVING;
    Type *inferred_type = resolve_expression_for_variable(ctx, var->value, decl);
    decl->status = Status_RESOLVED;

    if (!inferred_type) return NULL;
    if (var->flags & VAR_IS_INFERRED) {
        if (inferred_type == ctx->null_type) {
            compile_error(ctx, decl_tok(decl), "cannot infer type for \"null\" as it is ambiguous");
            return NULL;
        }
        *specified_type = inferred_type; // type inference!
    }
    return inferred_type;
}

static void resolve_statement(Context *ctx, AstNode *stmt) {
    switch (stmt->tag) {
    case Node_VAR:
        resolve_var((AstDecl *)stmt, ctx);
        break;

    case Node_BLOCK:
        resolve_block(ctx, (AstBlock *)stmt);
        break;
    case Node_CALL:
        resolve_call(stmt, ctx);
        break;
    case Node_ASSIGN:
        resolve_assignment(stmt, ctx);
        break;
    case Node_RETURN:
        resolve_expression(((AstReturn *)stmt)->expr, ctx);
        break;
    case Node_DEFER:
        resolve_statement(ctx, (AstNode *)((AstDefer *)stmt)->statement);
        break;
    case Node_IF: {
        AstIf *iff = (AstIf *)stmt;
        resolve_expression(iff->condition, ctx);
        resolve_statement(ctx, (AstNode *)iff->block_or_stmt);
        if (iff->other_branch) resolve_statement(ctx, (AstNode *)iff->other_branch);
    } break;
    case Node_WHILE: {
        AstWhile *w = (AstWhile *)stmt;
        resolve_expression(w->condition, ctx);
        resolve_block(ctx, (AstBlock *)w->block);
    } break;
    default: printf("resolve_statement tag %d\n", stmt->tag);
    }
}

static void resolve_block(Context *ctx, AstBlock *block) {
    stbds_arrpush(scope_stack, block);
    for (int i = 0; i < block->statements->len; i++) {
        AstNode *stmt = block->statements->nodes[i];
        resolve_statement(ctx, stmt);
    }
    stbds_arrpop(scope_stack);
}

static void resolve_procedure(AstDecl *procsym, Context *ctx) {
    if (procsym->status == Status_RESOLVED) return;
    procsym->status = Status_RESOLVING;

    AstProcedure *proc = (AstProcedure *)procsym;

    if (proc->params) {
        u64 num_params = proc->params->len;
        for (int i = 0; i < num_params; i++) {
            resolve_var((AstDecl *)proc->params->nodes[i], ctx);
        }
    }

    Type **return_type = &proc->return_type->as.type;
    *return_type = resolve_type(ctx, *return_type, false);

    if (proc->flags & PROC_IS_FOREIGN) {
        procsym->status = Status_RESOLVED;
        return;
    }

    stbds_arrpush(proc_stack, proc); // push the new scope

    AstBlock *block = (AstBlock *)proc->block;
    resolve_block(ctx, block);
    procsym->status = Status_RESOLVED;
    stbds_arrpop(proc_stack); // pop the scope
}

void resolve_program(Context *ctx) {
    resolve_procedure(ctx->decl_for_main, ctx);

    // Resolving main will resolve all the symbols in the code
    // that are actually used. There may be some top level symbols
    // that are left as unresolved because they do not get used.
    // The following code exists to validate that these unused decls
    // are still correct, however we will keep their status as Status_UNRESOLVED
    // so that later on the compiler will correctly assert that they were not
    // referred to in the code.

    u64 len = shlenu(ctx->symbol_table);
    for (int i = 0; i < len; i++) {
        AstDecl *decl = ctx->symbol_table[i].value;
        if (decl->status == Status_RESOLVED) continue;

        decl->status = Status_RESOLVING;
        switch (decl->tag) {
        case Decl_PROC:
            resolve_procedure(decl, ctx);
            break;
        case Decl_VAR:
            resolve_var(decl, ctx);
            break;
        case Decl_TYPEDEF: {
            AstTypedef *def = (AstTypedef *)decl;
            if (def->of->tag == Node_STRUCT) {
                resolve_struct((AstStruct *)def->of, ctx);
                // TODO (maybe) types that are only used inside unused structs
                // will still be set as Status_RESOLVED. Maybe this shouldn't be the case?
            }
        } break;
        }
        decl->status = Status_UNRESOLVED;
    }

    stbds_arrfree(proc_stack);
    stbds_arrfree(scope_stack);
}

// Code which recursively traverses an abstract syntax tree to resolve identifiers to the symbols which declared them, resolve expressions to types, and to apply type inference to variable declarations which specify it.
#include "headers/common.h"
#include "headers/ast.h"
#include "headers/type.h"
#include "headers/context.h"
#include "headers/passes.h"
#include "headers/table.h"

#include "headers/stb/stb_ds.h"

#include <assert.h>

static void resolve_procedure(AstDecl *procsym, Context *ctx, Ast *file_scope);
static Type *resolve_var(AstDecl *varsym, Context *ctx, Ast *file_scope);
static Type *resolve_selector(Context *ctx, AstBinary *accessor, Ast *file_scope);
static Type *resolve_expression(AstExpr *expr, Context *ctx, Ast *file_scope);
static Type *resolve_type(Context *ctx, Type *type, AstNode *site, bool cyclic_allowed, Ast *file_scope);
static void resolve_block(Context *ctx, AstBlock *block, Ast *file_scope);
static void resolve_assignment_expr(AstExpr *assign, Context *ctx, Ast *);
static void resolve_assignment(AstNode *ass, Context *ctx, Ast *file_scope);

// stbds arrays
static AstProcedure **proc_stack = NULL;
static AstBlock **scope_stack = NULL;

static AstProcedure *resolve_call(AstNode *callnode, Context *ctx, Ast *file_scope) {
    AstCall *call = (AstCall *)callnode;
    Token tok = callnode->token;

    Name *name = NULL;
    char *module_name = NULL;
    Ast  *scope = file_scope;

    if (call->name->tag == Expr_NAME) {
        name = call->name->as.name;
    }

    if (call->name->tag == Expr_BINARY) {
        AstBinary *sel = (AstBinary *)call->name;
        assert(sel->op == Token_DOT);
        name = sel->right->as.name;
        module_name = sel->left->as.name->text;
        scope = get_module(ctx, sel->left->as.name, file_scope, tok);
        if (!scope) return NULL;
    } 

    assert(name);

    AstDecl *hopefully_proc = find_decl(scope, name);
    if (!hopefully_proc) {
        if (scope == file_scope) {
            compile_error(ctx, tok, "call to undeclared procedure \"%s\"", name->text);
            return NULL;
        }
        assert(module_name);
        compile_error(ctx, tok, "module '%s' has no such procedure '%s'", module_name, name->text);
        return NULL;
    }

    if (hopefully_proc->tag != Decl_PROC) {
        compile_error(ctx, tok, "attempted to call \"%s\", but it's not a procedure", name->text);
        return NULL;
    }

    if (hopefully_proc->status == Status_UNRESOLVED) {
        resolve_procedure(hopefully_proc, ctx, scope);
    }

    // Ensure the module that the requested function was declared in is imported in the module the call was made in.
    if (call->params) for (int i = 0; i < call->params->len; i++) {
        AstExpr *arg = (AstExpr *)call->params->nodes[i];
        resolve_expression(arg, ctx, file_scope);
    }
    
    if (scope != file_scope) ast_add(file_scope, (AstNode *)hopefully_proc);

    AstProcedure *calling = (AstProcedure *)hopefully_proc;
    call->calling = calling;
    return calling;
}

static Type *resolve_name(Context *ctx, Name *name, AstNode *site, Ast *file_scope) {
    AstProcedure *in = stbds_arrlast(proc_stack);
    AstBlock *scope = stbds_arrlast(scope_stack);

    AstDecl *var = NULL;

    if (!scope) var = find_decl(file_scope, name); // global scope
    else var = lookup_local(ctx, in, name, scope, file_scope);
    if (!var) {
        compile_error(ctx, site->token, "undeclared identifier \"%s\"", name->text);
        return NULL;
    }

    if (var->tag != Decl_VAR) {
        compile_error(ctx, site->token, "\"%s\" was used like a variable, but it isn't one", name->text);
        return NULL;
    }

    assert(var->name == name);

    if (var->status == Status_UNRESOLVED) {
        if (!(var->flags & DECL_IS_TOP_LEVEL) && !(var->flags & DECL_IS_STRUCT_FIELD)) {
            compile_error(ctx, site->token, "local variable \"%s\" was used before it was declared", name->text);
            return NULL;
        }
        Type *resolved_type = resolve_var(var, ctx, file_scope);
        return resolved_type;
    }

    else if (var->status == Status_RESOLVING) {
        compile_error(ctx, site->token, "initial instantiation of variable \"%s\" mentions itself", name->text);
        return NULL;
    }
    return var->as.var.typename->as.type;
}

static Type *resolve_expression_1(AstExpr *expr, Context *ctx, Ast *file_scope) {
    Token t = expr_tok(expr);
    switch (expr->tag) {
    case Expr_STRING: return ctx->type_string;
    case Expr_INT:    return ctx->type_int;
    case Expr_BOOL:   return ctx->type_bool;
    case Expr_FLOAT:  return ctx->type_f64;
    case Expr_NULL:   return ctx->null_type;

    case Expr_IMPORT: return ctx->import_type;

    case Expr_CALL: {
        AstProcedure *resolved = resolve_call((AstNode *)expr, ctx, file_scope);
        if (!resolved) return NULL;
        return resolved->return_type->as.type;
    } break;
    case Expr_UNARY: {
        AstUnary *unary = (AstUnary *)expr;
        Type *expr_type = resolve_expression(unary->expr, ctx, file_scope);
        if (unary->op == Token_BANG) {
            return ctx->type_bool;
        }
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
        return resolve_name(ctx, name, (AstNode *)expr, file_scope);
    } break;
    case Expr_CAST: {
        AstCast *cast = (AstCast *)expr;
        resolve_type(ctx, cast->typename->as.type, (AstNode *)expr, false, file_scope);
        resolve_expression(cast->expr, ctx, file_scope);
        return cast->typename->as.type;
    } break;
    case Expr_INDEX: {
        AstArrayIndex *index = (AstArrayIndex *)expr;
        Type *resolved_name = resolve_expression(index->name, ctx, file_scope);
        if (!resolved_name) return NULL;
        resolve_expression(index->index, ctx, file_scope);
        return resolved_name->data.base;
    } break;

    case Expr_BINARY: {
        AstBinary *bin = (AstBinary *)expr;
        if (bin->op == Token_DOT) {
            return resolve_selector(ctx, bin, file_scope);
        }
        if (is_binary_comparison(*bin)) {
            resolve_expression(bin->left, ctx, file_scope);
            resolve_expression(bin->right, ctx, file_scope);
            return ctx->type_bool;
        }
        if (is_assignment(*bin)) {
            resolve_assignment_expr(expr, ctx, file_scope);
            if (bin->op != Token_EQUAL) { // it's a +=, -=, *= or /=
                return ctx->type_int; // so its type must be integer
            }
        }

        // Should only be maths operators left
        assert(bin->op == Token_PLUS || bin->op == Token_MINUS || bin->op == Token_SLASH || bin->op == Token_STAR);

        Type *left_type = resolve_expression(bin->left, ctx, file_scope);
        Type *right_type = resolve_expression(bin->right, ctx, file_scope);

        if (!left_type || !right_type) return NULL;

        bool left = is_type_numeric(left_type) || left_type->kind == Type_POINTER;
        bool right = is_type_numeric(right_type) || right_type->kind == Type_POINTER;
        if (!left || !right) {
            compile_error(ctx, t, "operands of arithmetic must be of pointer or numerical type");
            return NULL;
        }

        // You can add and substract to/from pointers
        if (bin->op == Token_PLUS || bin->op == Token_MINUS) {
            if (left_type->kind == Type_POINTER)
                return left_type;
            if (right_type->kind == Type_POINTER)
                return right_type;
        }

        return ctx->type_int;
    } break;

    case Expr_PAREN: {
        return resolve_expression(((AstParen *)expr)->sub_expr, ctx, file_scope);
    } break;
    }
    assert(false);
    return NULL;
}

static Type *resolve_expression(AstExpr *expr, Context *ctx, Ast *file_scope) {
    if (!expr) return NULL;
    Type *re = resolve_expression_1(expr, ctx, file_scope);
    expr->resolved_type = re;
    return re;
}

void resolve_struct(AstStruct *def, Context *ctx, Ast *file_scope) {
    AstBlock *block = &def->members->as.block;
    for (u64 i = 0; i < block->statements->len; i++) {
        AstNode *node = block->statements->nodes[i];
        assert(is_decl(node));
        AstDecl *field = (AstDecl *)node;
        if (field->tag != Decl_VAR) {
            compile_error(ctx, decl_tok(field), "only variable declarations are valid inside a struct body"); // this is semantic checking but we have to do it here otherwise it's a nightmare
            continue;
        }
        stbds_arrpush(scope_stack, &def->members->as.block);
        resolve_var(field, ctx, file_scope);
        stbds_arrpop(scope_stack);
    }
}

// Resolves an unresolved type to it's "real" type
static Type *resolve_type(Context *ctx, Type *type, AstNode *site, bool cyclic_allowed, Ast *file_scope) {
    if (type->kind == Type_PRIMITIVE) return type;

    if (type->kind == Type_ANON_STRUCT) {
        resolve_struct(&type->data.user->as.stmt.as._struct, ctx, file_scope);
        return type;
    }

    if (type->kind == Type_ALIAS) {
        return resolve_type(ctx, type->data.alias_of, site, false, file_scope);
    }

    //
    // Pointers and arrays might have unresolved types in their sub-types.
    // We resolve those and wrap them back up each time.
    //
    if (type->kind == Type_POINTER)
        return make_pointer_type(resolve_type(ctx, type->data.base, site, true, file_scope)); // ... cyclic_allowed should only be passed as true here and below

    if (type->kind == Type_ARRAY)
        return make_array_type(resolve_type(ctx, type->data.base, site, true, file_scope));

    // Unresolved types (that is, types which are used before they are defined) are returned
    // as placeholder types, and are not put in the type table. If they then go on to be defined in the source code, they will be placed into the type table.
    // Here, we lookup the type in the type table. If it is not found, then it was never declared.
    AstDecl *sym = find_type_decl(file_scope, make_name_from_string(ctx, type->name));
    if (!sym) {
        compile_error(ctx, site->token, "undeclared type \"%s\"", type->name);
        return NULL;
    }
    Type *real_type = sym->as.type;

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
        compile_error(ctx, site->token, "\"%s\" is not a type", type->name);
        return NULL;
    }

    if (real_type->data.user->tag == Node_STRUCT) {
        resolve_struct(&real_type->data.user->as.stmt.as._struct, ctx, file_scope);
    }

    sym->status = Status_RESOLVED;

    return real_type;
}

// Resolves the dependencies of a selector and returns the type of the field it selects.
static Type *resolve_selector(Context *ctx, AstBinary *accessor, Ast *file_scope) {
    assert(accessor->op == Token_DOT);

    Name *rhs = accessor->right->as.name;
    Type *lhs_type = resolve_expression(accessor->left, ctx, file_scope);

    if (!lhs_type) return NULL; // resolve_expression will have already errored, so we can just exit

    if (lhs_type->kind == Type_UNRESOLVED) {
        lhs_type = resolve_type(ctx, lhs_type, (AstNode *)accessor, false, file_scope);
        assert(lhs_type);
    }

    if (lhs_type->kind == Type_ENUM) {
        AstEnum *enum_def = &lhs_type->data.user->as.stmt.as._enum;
        AstDecl *member = table_get(&enum_def->fields, rhs->text);
        if (!member) {
            compile_error(ctx, expr_tok(accessor->left), "enum has no field \"%s\"", rhs->text);
            return NULL;
        }
        accessor->right->resolved_type = enum_def->base_type;
        return lhs_type;
    }

    if (lhs_type == ctx->type_string || (lhs_type->kind == Type_POINTER && lhs_type->data.base == ctx->type_string)) {
        if (rhs == make_name_from_string(ctx, "data")) {
            static Type *data_type;
            data_type = make_pointer_type(ctx->type_u8);
            return data_type;
        } else if (rhs == make_name_from_string(ctx, "length")) {
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

    AstStruct *struct_def = &lhs_type->data.user->as.stmt.as._struct;
    AstDecl *field = lookup_struct_field(struct_def, rhs);
    if (!field) {
        compile_error(ctx, expr_tok(accessor->right), "no such field as \"%s\" in struct field access", rhs->text);
        return NULL;
    }
    if (field->status != Status_RESOLVED) resolve_var(field, ctx, file_scope);
    assert(field->tag == Decl_VAR); // should have been checked by now
    return field->as.var.typename->as.type;
}

static void resolve_assignment_expr(AstExpr *assign, Context *ctx, Ast *file_scope) {
    if (assign->tag == Expr_UNARY) {
        AstUnary *unary = (AstUnary *)assign;
        resolve_assignment_expr(unary->expr, ctx, file_scope);
    } else if (assign->tag == Expr_BINARY) {
        AstBinary *bin = (AstBinary *)assign;
        if (!is_assignment(*bin)) {
            compile_error(ctx, expr_tok(assign), "expected an assignment");
            return;
        }
        resolve_expression(bin->left, ctx, file_scope);
        resolve_expression(bin->right, ctx, file_scope);
    } else {
        compile_error(ctx, expr_tok(assign), "expected an assignment");
    }
}

// Resolves the dependencies of an assignment statement,
static void resolve_assignment(AstNode *ass, Context *ctx, Ast *file_scope) {
    assert(ass->tag == Node_ASSIGN);
    AstStmt *stmt = (AstStmt *)ass;
    resolve_assignment_expr(&stmt->as.assign, ctx, file_scope);
}

static Type *resolve_imported_type(Context *ctx, AstBinary *selector, Ast *file_scope) {
    assert(selector->op == Token_DOT && selector->left->tag == Expr_NAME && selector->right->tag == Expr_NAME);
    Token token = expr_tok((AstExpr *)selector);

    char *package_name = selector->left->as.name->text;
    char *type_name    = selector->right->as.name->text;

    Ast *imported_scope = get_module(ctx, selector->left->as.name, file_scope, token);
    if (!imported_scope) return NULL;

    AstDecl *type_decl = find_type_decl(imported_scope, make_name_from_string(ctx, type_name));
    if (!type_decl) {
        compile_error(ctx, token, "undeclared type '%s' - not defined in target package '%s'", type_name, package_name);
        return NULL;
    }
    
    // TODO: consider adding a dummy/placeholder node instead of directly copying the node itself.
    //       Then, at codegen time we could just go find the node and do the codegen for it instead of adding it to this scope directly.
    ast_add(file_scope, (AstNode *)type_decl);
    return type_decl->as.type;
}

// Resolves the dependencies and type of a variable declaration,
// and apply type inference if needed.
static Type *resolve_var(AstDecl *decl, Context *ctx, Ast *file_scope) {
    AstVar *var = (AstVar *)decl;
    Type **specified_type = &var->typename->as.type;

    if (decl->status == Status_RESOLVED)
        return *specified_type;

    if (var->typename->tag == Node_BINARY) {
        *specified_type = resolve_imported_type(ctx, (AstBinary *)var->typename, file_scope);
        var->typename->tag = Node_TYPENAME;
    } else if (!(var->flags & VAR_IS_INFERRED)) {
        if (var->typename->as.type->kind == Type_ANON_STRUCT) {
            resolve_struct(&var->typename->as.type->data.user->as.stmt.as._struct, ctx, file_scope);
        } else {
            *specified_type = resolve_type(ctx, *specified_type, (AstNode *)decl, false, file_scope);
        }
    }

    if (!(var->flags & VAR_IS_INITED)) {
        decl->status = Status_RESOLVED;
        return *specified_type;
    }

    decl->status = Status_RESOLVING;
    Type *inferred_type = resolve_expression(var->value, ctx, file_scope);
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

static void resolve_statement(Context *ctx, AstNode *stmt, Ast *file_scope) {
    switch (stmt->tag) {
    case Node_VAR:
        resolve_var((AstDecl *)stmt, ctx, file_scope);
        break;
    case Node_BLOCK:
        resolve_block(ctx, (AstBlock *)stmt, file_scope);
        break;
    case Node_CALL:
        resolve_call(stmt, ctx, file_scope);
        break;
    case Node_ASSIGN:
        resolve_assignment(stmt, ctx, file_scope);
        break;
    case Node_RETURN: {
        AstReturn *ret = (AstReturn *)stmt;
        resolve_expression(ret->expr, ctx, file_scope);
        ret->owning = stbds_arrlast(scope_stack);
    } break;
    case Node_DEFER: {
        AstDefer *d = (AstDefer *)stmt;
        AstBlock *current_block = stbds_arrlast(scope_stack);

        ast_add(current_block->deferred, (AstNode *)d->statement);

        resolve_statement(ctx, (AstNode *)d->statement, file_scope);
    } break;
    case Node_USING: {
        // assert(false);
#if 0
        AstUsing *using = (AstUsing *)stmt;
        Type *decl_type = resolve_name(ctx, using->what, stmt);
        if (decl_type->kind != Type_STRUCT && decl_type->kind != Type_ANON_STRUCT) {
            compile_error(ctx, stmt->token, "procedure level using must target struct instances only");
            return;
        }
        AstBlock *source_scope = &decl_type->data.user->as._struct.members->as.block;
        AstBlock *target_scope = stbds_arrlast(scope_stack);
        for (int i = 0; i < source_scope->statements->len; i++) {
            AstDecl *decl = (AstDecl *)source_scope->statements->nodes[i];
            assert(table_add(&target_scope->symbols, decl->name->text, decl));
            ast_add(target_scope->statements, (AstNode *)decl);
        }
#endif
    } break;
    case Node_IF: {
        AstIf *iff = (AstIf *)stmt;
        resolve_expression(iff->condition, ctx, file_scope);
        resolve_statement(ctx, (AstNode *)iff->block_or_stmt, file_scope);
        if (iff->other_branch) resolve_statement(ctx, (AstNode *)iff->other_branch, file_scope);
    } break;
    case Node_WHILE: {
        AstWhile *w = (AstWhile *)stmt;
        resolve_expression(w->condition, ctx, file_scope);
        resolve_block(ctx, (AstBlock *)w->block, file_scope);
    } break;
    default: printf("resolve_statement tag %d\n", stmt->tag);
    }
}

static void resolve_block(Context *ctx, AstBlock *block, Ast *file_scope) {
    stbds_arrpush(scope_stack, block);
    for (int i = 0; i < block->statements->len; i++) {
        AstNode *stmt = block->statements->nodes[i];
        resolve_statement(ctx, stmt, file_scope);
    }
    stbds_arrpop(scope_stack);
}

static void resolve_procedure(AstDecl *procsym, Context *ctx, Ast *file_scope) {
    if (procsym->status == Status_RESOLVED) return;
    procsym->status = Status_RESOLVING;

    AstProcedure *proc = (AstProcedure *)procsym;

    if (proc->params) {
        u64 num_params = proc->params->len;
        for (int i = 0; i < num_params; i++) {
            resolve_var((AstDecl *)proc->params->nodes[i], ctx, file_scope);
        }
    }

    Type **return_type = &proc->return_type->as.type;
    *return_type = resolve_type(ctx, *return_type, (AstNode *)procsym, false, file_scope);

    if (proc->flags & PROC_IS_FOREIGN) {
        procsym->status = Status_RESOLVED;
        return;
    }

    stbds_arrpush(proc_stack, proc); // push the new scope

    AstBlock *block = (AstBlock *)proc->block;
    resolve_block(ctx, block, file_scope);
    procsym->status = Status_RESOLVED;
    stbds_arrpop(proc_stack); // pop the scope
}

void resolve_module(Context *ctx, Ast *module) {
    resolve_procedure(ctx->decl_for_main, ctx, module);

#if 1
    // Resolving main will resolve all the symbols in the code
    // that are actually used. There may be some top level symbols
    // that are left as unresolved because they do not get used.
    // The following code exists to validate that these unused decls
    // are still correct, however we will keep their status as Status_UNRESOLVED
    // so that later on the compiler will correctly assert that they were not
    // referred to in the code.

    for (u64 i = 0; i < module->len; i++) {
        AstNode *node = module->nodes[i];
        assert(is_decl(node));
        AstDecl *decl = (AstDecl *)node;
        if (decl->status == Status_RESOLVED) continue;
        decl->status = Status_RESOLVING;

        switch (decl->tag) {
        case Decl_PROC:
            resolve_procedure(decl, ctx, module);
            break;
        case Decl_VAR:
            resolve_var(decl, ctx, module);
            break;
        case Decl_TYPEDEF: {
            Type *type = decl->as.type;
            if (type->data.user->tag == Node_STRUCT) {
                resolve_struct((AstStruct *)type->data.user, ctx, module);
                // NOTE types that are only used inside unused structs
                // will still be set as Status_RESOLVED. Maybe this shouldn't be the case?
            }
        } break;
        }
        // decl->status = Status_UNRESOLVED;
    }
#endif
    stbds_arrfree(proc_stack);
    stbds_arrfree(scope_stack);
}

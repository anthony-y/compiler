// Code which recursively traverses an abstract syntax tree to resolve identifiers to the symbols which declared them, resolve expressions to types, and to apply type inference to variable declarations which specify it.
#include "headers/common.h"
#include "headers/ast.h"
#include "headers/type.h"
#include "headers/context.h"
#include "headers/passes.h"

#include "headers/stb/stb_ds.h"

#include <assert.h>

static AstTypeDecl  *resolve_procedure(AstProcedure *proc, Context *ctx, Module *module);
static AstTypeDecl  *resolve_name(Context *ctx, Name *name, AstNode *site, Module *module);
static AstTypeDecl  *resolve_selector(Context *ctx, AstBinary *accessor, Module *module);
static AstTypeDecl  *resolve_expression(AstExpr *expr, Context *ctx, Module *module);
static AstTypeDecl  *resolve_expression_to_type(AstExpr *expr, Context *ctx, Module *module);
static AstProcedure *resolve_call(AstCall *call, Context *ctx, Module *module);
static void          resolve_assignment_expr(AstExpr *assign, Context *ctx, Module *);
static void          resolve_struct(AstStruct *def, Context *ctx, Module *module);

static AstTypeDecl *resolve_type_decl(Context *ctx, AstTypeDecl *type, AstNode *site, bool cyclic_allowed, Module *module);
static AstTypeDecl *resolve_imported_type(Context *ctx, AstBinary *selector, Module *module);

static void resolve_block(Context *ctx, AstBlock *block, Module *module);
static void resolve_statement(Context *ctx, AstStmt *stmt, Module *module);



static AstTypeDecl *resolve_typename_in_type_slot(AstTypename *ref, Context *ctx, Module *module) {
    // For pointers, arrays, and built-in types, resolved_type is set at parse time.
    if (ref->resolved_type) {
        // Placeholders only appear as the base of type of a pointer or array type, which get resolved below
        // by calling resolve_type_decl directly. If one gets through here, there's a bug.
        assert(ref->resolved_type->expr_type != TypeDecl_PLACEHOLDER);

        // This should only happen for builtin types since they are flagged as Status_RESOLVED when they're created in type.cc.
        if (ref->resolved_type->status == Status_RESOLVED) {
            return ref->resolved_type;
        }

        if (ref->resolved_type->expr_type == TypeDecl_POINTER || ref->resolved_type->expr_type == TypeDecl_ARRAY) {
            return resolve_type_decl(ctx, ref->resolved_type, ref, false, module);
        }
    }

    if (ref->selector) {
        return resolve_imported_type(ctx, ref->selector, module);
    }

    // For user-defined types, we need the name of the type to find the declaration for it.
    // This asserts that we have one to lookup the declaration with.
    assert(ref->name && !ref->selector);

    AstDecl *decl = find_decl_from_local_scope_upwards(ctx, ref->name, module);
    if (!decl) {
        compile_error(ctx, ref->token, "unknown type '%s'", ref->name->text);
        return NULL;
    }

    if (decl->tag != Node_TYPE_DECL) {
        compile_error(ctx, ref->token, "'%s' was used as a typename, but it's declaration does not create one", decl->name->text);
        return NULL;
    }

    AstTypeDecl *as_type_ref = resolve_type_decl(ctx, (AstTypeDecl *)decl, ref, false, module);
    if (!as_type_ref) return NULL;

    ref->resolved_type = as_type_ref;
    return as_type_ref;
}

// Resolves the dependencies and type of a variable declaration,
// and apply type inference if needed.
AstTypeDecl *resolve_decl(AstDecl *decl, Context *ctx, Module *module) {
    if (decl->status == Status_RESOLVED) return decl->given_type->resolved_type;

    if (!(decl->flags & DECL_IS_INFERRED)) {
        AstTypeDecl *resolved_typename = resolve_typename_in_type_slot(decl->given_type, ctx, module);
        if (!resolved_typename) return NULL;

        if (!decl->expr) {
            decl->given_type->resolved_type = resolved_typename;
            decl->status = Status_RESOLVED;
            return resolved_typename;
        }
    }

    decl->status = Status_RESOLVING;
    AstTypeDecl *inferred_type = resolve_expression(decl->expr, ctx, module);
    decl->status = Status_RESOLVED;

    if (!inferred_type) return NULL;

    if (decl->flags & DECL_IS_CONST && decl->expr) {
        if (decl->expr->tag == Node_IDENT) {
            auto ident = (AstIdent *)decl->expr;
            if (!(ident->resolved_decl->flags & DECL_IS_CONST)) {
                compile_error(ctx, decl->token, "attempt to assign non-constant value to constant declaration");
                return NULL;
            }
        }
    }

    if (decl->flags & DECL_IS_INFERRED) {
        if (inferred_type == ctx->null_type) {
            compile_error(ctx, decl->token, "cannot infer type for \"null\" as it is ambiguous");
            return NULL;
        }
        decl->given_type->resolved_type = inferred_type;
    }

    return inferred_type;
}

// Resolves an unresolved type to it's "real" type
static AstTypeDecl *resolve_type_decl(Context *ctx, AstTypeDecl *type, AstNode *site, bool cyclic_allowed, Module *module) {
    if (type->expr_type > TypeDecl_BUILTIN_START && type->expr_type < TypeDecl_BUILTIN_END) return type;

    if (type->expr_type == TypeDecl_ALIAS) {
        return resolve_typename_in_type_slot(type->alias, ctx, module);
    }

    //
    // Pointers and arrays might have unresolved types in their sub-types.
    // We resolve those and wrap them back up each time.
    //
    if (type->expr_type == TypeDecl_POINTER) {
        type->base_type = resolve_type_decl(ctx, type->base_type, site, true, module); // ... cyclic_allowed should only be passed as true here and below
        return make_pointer_type(type->base_type);
    }

    if (type->expr_type == TypeDecl_ARRAY) {
        type->base_type = resolve_type_decl(ctx, type->base_type, site, true, module);
        return make_array_type(type->base_type);
    }

    assert(type->expr_type == TypeDecl_PLACEHOLDER || type->expr_type == TypeDecl_STRUCT || type->expr_type == TypeDecl_ENUM || type->expr_type == TypeDecl_PROCEDURE);

    if (type->expr_type == TypeDecl_PLACEHOLDER) {
        auto actual_type = find_decl_from_local_scope_upwards(ctx, type->name, module);
        if (!actual_type) {
            compile_error(ctx, site->token, "undeclared base type '%s'", type->name->text);
            return NULL;
        }
        if (actual_type->tag != Node_TYPE_DECL) {
            assert(false);
        }
        return (AstTypeDecl *)actual_type;
    }

    if (type->status == Status_RESOLVED) return type;

    // If the declaration is already resolving...
    if (type->status == Status_RESOLVING) {
        if (cyclic_allowed) {
            type->status = Status_RESOLVED;
            return type;
        }
        compile_error(ctx, type->token, "type definition for \"%s\" depends on itself (try making that field a ^%s)", type->name, type->name);
        return NULL;
    }

    type->status = Status_RESOLVING; // the above if statements works because of this
    if (type->expr_type == TypeDecl_STRUCT) {
        resolve_struct(type->struct_, ctx, module);
    } // TODO enum, procedure
    type->status = Status_RESOLVED;
    return type;
}

static AstTypeDecl *resolve_imported_type(Context *ctx, AstBinary *selector, Module *module) {
    assert(selector->op == Token_DOT && selector->left->tag == Node_IDENT && selector->right->tag == Node_IDENT);
    Token token = selector->token;

    auto module_name = (AstIdent *)selector->left;
    auto type_name   = (AstIdent *)selector->right;

    Module *imported_scope = get_module(ctx, module_name->name, module, token);
    if (!imported_scope) return NULL;

    AstDecl *type_decl = find_top_level_decl(imported_scope, type_name->name);
    if (!type_decl) {
        compile_error(ctx, token, "undeclared type '%s' - not defined in target package '%s'", type_name->name->text, module_name->name->text);
        return NULL;
    }
    if (type_decl->tag != Node_TYPE_DECL) {
        compile_error(ctx, token, "'%s.%s' was used as a typename, but it's declaration does not create one", module_name->name->text, type_name->name->text);
    }
    return (AstTypeDecl *)type_decl;
}



static void resolve_block(Context *ctx, AstBlock *block, Module *module) {
    block_stack_push(&ctx->block_stack, block);
    for (int i = 0; i < block->statements->len; i++) {
        AstNode *stmt = block->statements->nodes[i];
        if (stmt->tag == Node_DECL) resolve_decl((AstDecl *)stmt, ctx, module);
        else if (stmt->tag == Node_TYPE_DECL) continue; // TODO
        else resolve_statement(ctx, (AstStmt *)stmt, module);
    }
    block_stack_pop(&ctx->block_stack);
}

static void resolve_statement(Context *ctx, AstStmt *stmt, Module *module) {
    switch (stmt->tag) {
    case Node_BLOCK: {
        resolve_block(ctx, (AstBlock *)stmt, module);
    } break;
    case Node_CALL: {
        auto call = (AstCallStmt *)stmt;
        resolve_call(call->expr, ctx, module);
    } break;
    case Node_ASSIGN: {
        auto assign = (AstAssignment *)stmt;
        resolve_assignment_expr(assign->expr, ctx, module);
    } break;
    case Node_RETURN: {
        AstReturn *ret = (AstReturn *)stmt;
        resolve_expression(ret->expr, ctx, module);
        ret->owning = block_stack_top(ctx->block_stack);
    } break;
    case Node_DEFER: {
        AstDefer *d = (AstDefer *)stmt;
        AstBlock *current_block = block_stack_top(ctx->block_stack);
        ast_add(current_block->deferred, (AstNode *)d->statement);
        resolve_statement(ctx, d->statement, module);
    } break;
    case Node_USING: {
        // assert(false);
#if 0
        AstUsing *using = (AstUsing *)stmt;
        Type *decl_type = resolve_name(ctx, using->what, stmt);
        if (decl_type->expr_type != Type_STRUCT && decl_type->expr_type != Type_ANON_STRUCT) {
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
        resolve_expression(iff->condition, ctx, module);
        resolve_statement(ctx, iff->block_or_stmt, module);
        if (iff->other_branch) resolve_statement(ctx, iff->other_branch, module);
    } break;
    case Node_WHILE: {
        AstWhile *w = (AstWhile *)stmt;
        resolve_expression(w->condition, ctx, module);
        resolve_block(ctx, (AstBlock *)w->block, module);
    } break;
    default: printf("resolve_statement tag %d\n", stmt->tag);
    }
}




static AstTypeDecl *resolve_procedure(AstProcedure *proc, Context *ctx, Module *module) {
    if (proc->params) {
        u64 num_params = proc->params->len;
        for (int i = 0; i < num_params; i++) {
            resolve_decl((AstDecl *)proc->params->nodes[i], ctx, module);
        }
    }

    AstTypeDecl *resolved_return_type = resolve_typename_in_type_slot(proc->return_type, ctx, module);
    if (!resolved_return_type) return NULL;

    if (proc->flags & PROC_IS_FOREIGN) {
        // return make_procedure_type(resolved_return_type, NULL); // TODO argument types
        return resolved_return_type;
    }

    proc_stack_push(&ctx->proc_stack, proc);
    auto block = (AstBlock *)proc->block;
    resolve_block(ctx, block, module);
    proc_stack_pop(&ctx->proc_stack);

    // return make_procedure_type(resolved_return_type, NULL); // TODO argument types
    return resolved_return_type;
}

// Resolves the dependencies of a selector and returns the type of the field it selects.
static AstTypeDecl *resolve_selector(Context *ctx, AstBinary *accessor, Module *module) {
    assert(accessor->op == Token_DOT);

    auto rhs = (AstIdent *)accessor->right;
    AstTypeDecl *lhs_type = resolve_expression(accessor->left, ctx, module);

    if (!lhs_type) return NULL; // resolve_expression will have already errored, so we can just exit

/*
    if (lhs_type->expr_type == TypeDecl_ENUM) {
        AstEnum *enum_def = lhs_type->enum_;
        AstDecl *member = table_get(&enum_def->fields, rhs->text);
        if (!member) {
            compile_error(ctx, expr_tok(accessor->left), "enum has no field \"%s\"", rhs->text);
            return NULL;
        }
        accessor->right->resolved_type = enum_def->base_type;
        return lhs_type;
    }
*/

    // If it was a pointer, unwrap it, but only by one "level",
    // selectors shouldn't be able to reach into far-down structs in pointers.
    // TODO could do this higher up but it would break some stuff
    if (lhs_type->expr_type == TypeDecl_POINTER) {
        lhs_type = lhs_type->base_type;
    }

    if (lhs_type == ctx->type_string) {
        if (rhs->name == make_name_string(ctx, "data")) {
            static AstTypeDecl *data_type;
            data_type = make_pointer_type(ctx->type_u8);
            return data_type;
        } else if (rhs->name == make_name_string(ctx, "length")) {
            return ctx->type_u64;
        } else {
            compile_error(ctx, accessor->left->token, "type \"string\" has no field \"%s\"", rhs->name->text);
            return NULL;
        }
    }

    if (lhs_type->expr_type == TypeDecl_ARRAY) {
        if (rhs->name == make_name_string(ctx, "data")) {
            return make_pointer_type(lhs_type->base_type);
        } else if (rhs->name == make_name_string(ctx, "length")) {
            return ctx->type_u64;
        } else if (rhs->name == make_name_string(ctx, "capacity")) {
            return ctx->type_u64;
        } else {
            compile_error(ctx, accessor->left->token, "array has no field \"%s\"", rhs->name->text);
            return NULL;
        }
    }

    // In English: throw an error if the type of the left hand side is not either:
    //  - a struct
    //  - a pointer, the base type of which is a struct
    if (lhs_type->expr_type != TypeDecl_STRUCT) {
        compile_error(ctx, accessor->left->token, "attempt to access member '%s' in value that doesn't create a namespace", rhs->name->text);
        return NULL;
    }

    AstStruct *struct_def = lhs_type->struct_;
    AstDecl *field = find_struct_field(struct_def, rhs->name);
    if (!field) {
        compile_error(ctx, accessor->left->token, "no such field as \"%s\" in struct field access", rhs->name->text);
        return NULL;
    }
    if (field->status != Status_RESOLVED) resolve_decl(field, ctx, module); 
    return field->given_type->resolved_type;
}

static void resolve_assignment_expr(AstExpr *assign, Context *ctx, Module *module) {
    if (assign->tag == Node_UNARY) {
        AstUnary *unary = (AstUnary *)assign;
        resolve_assignment_expr(unary->expr, ctx, module);
    } else if (assign->tag == Node_BINARY) {
        AstBinary *bin = (AstBinary *)assign;
        if (!is_assignment(*bin)) {
            compile_error(ctx, assign->token, "expected an assignment");
            return;
        }
        resolve_expression(bin->left, ctx, module);
        resolve_expression(bin->right, ctx, module);
    } else {
        compile_error(ctx, assign->token, "expected an assignment");
    }
}


static AstProcedure *resolve_call(AstCall *call, Context *ctx, Module *module) {
    Token tok = call->token;

    Name   *name        = NULL;
    char   *module_name = NULL;
    Module *scope       = module;

    // TODO: maybe AstIdent should look similar to AstTypename, and use that for the name on calls
    if (call->name->tag == Node_IDENT) {
        name = ((AstIdent *)call->name)->name;
    }

    if (call->name->tag == Node_BINARY) {
        AstBinary *sel = (AstBinary *)call->name;
        assert(sel->op == Token_DOT);

        name = ((AstIdent *)sel->right)->name;
        module_name = ((AstIdent *)sel->left)->name->text;

        scope = get_module(ctx, ((AstIdent *)sel->left)->name, module, tok);
        if (!scope) return NULL;
    } 

    assert(name);

    AstDecl *hopefully_proc = find_decl_from_local_scope_upwards(ctx, name, scope);
    if (!hopefully_proc) {
        if (scope == module) { // the target procedure is expected to have been declared in our local module.
            compile_error(ctx, tok, "call to undeclared procedure '%s'", name->text);
            return NULL;
        }
        assert(module_name);
        compile_error(ctx, tok, "call to undeclared procedure: '%s.%s'", module_name, name->text);
        return NULL;
    }

    if (hopefully_proc->expr->tag != Node_PROCEDURE) {
        compile_error(ctx, tok, "can't invoke non-procedure '%s' as if it was a procedure", name->text);
        return NULL;
    }

    if (hopefully_proc->status == Status_UNRESOLVED) {
        if (!resolve_procedure((AstProcedure *)hopefully_proc->expr, ctx, scope)) {
            // TODO: might need an error
            return NULL;
        }
    }

    // Ensure the module that the requested function was declared in is imported in the module from which the call was made.
    if (call->params) for (int i = 0; i < call->params->len; i++) {
        AstExpr *arg = (AstExpr *)call->params->nodes[i];
        resolve_expression(arg, ctx, module);
    }
    
    AstProcedure *calling = (AstProcedure *)hopefully_proc->expr;
    call->calling = calling;
    return calling;
}

static AstTypeDecl *resolve_typename_as_rvalue(AstTypename *ref, Context *ctx, Module *module) {
    assert(ref->resolved_type); // since we dispatched here, we know it's a typename, so it's resolved_decl should have been set at parse time.
    return ref->resolved_type;
}

static AstTypeDecl *resolve_name(Context *ctx, AstIdent *name, AstNode *site, Module *module) {
    AstDecl *decl = find_decl_from_local_scope_upwards(ctx, name->name, module);

    if (!decl) {
        compile_error(ctx, site->token, "undeclared identifier \"%s\"", name->name->text);
        return NULL;
    }

    if (decl->tag == Node_TYPE_DECL) {
        // TODO: this doesn't work because site is probably just the expression of the identifier itself, and not the declaration that it is on. Maybe add a AstDecl * to AstExpr, which can be non-null in the case that the expression belongs to a declaration. Then, here, I could check that pointer through the site.
        if (site->tag == Node_DECL) {
            auto sitedecl = (AstDecl *)site;
            if (sitedecl->flags & DECL_IS_CONST) {
                printf("we got a type alias over here\n");
            } else {
                printf("we got just a runtime type decl that's fine we don't have to do anything\n");
            }
        }

        // This should be fine but I don't know yet.
        printf("resolve_name() :: decl->tag == Node_TYPE_DECL, site->tag == %d\n", site->tag);
        auto as_type = resolve_type_decl(ctx, (AstTypeDecl *)decl, site, false, module);
        name->resolved_decl = as_type;
        return as_type;
    }

    if (decl->status == Status_UNRESOLVED) {
        // TODO: maybe have a different error for constant and non-constant decls?
        if ((!(decl->flags & DECL_IS_CONST)) && (!(decl->flags & DECL_IS_TOP_LEVEL)) && (!(decl->flags & DECL_IS_STRUCT_FIELD))) {
            // compile_error(ctx, site->token, "non-constant local \"%s\" was used before it was declared", name->name->text);
            compile_error(ctx, site->token, "unable to resolve declaration '%s' (either it creates a circular dependency, or you used it before you declared it, which you can only do with constants)", name->name->text);
            return NULL;
        }
        name->resolved_decl = decl;
        return resolve_decl(decl, ctx, module);
    }

    if (decl->status == Status_RESOLVING) {
        compile_error(ctx, site->token, "initial instantiation of '%s' mentions itself", name->name->text);
        return NULL;
    }

    name->resolved_decl = decl;
    return decl->given_type->resolved_type;
}

static void resolve_struct(AstStruct *def, Context *ctx, Module *module) {
    auto block = (AstBlock *)def->members;
    resolve_block(ctx, block, module);
}

static AstTypeDecl *resolve_expression(AstExpr *expr, Context *ctx, Module *module) {
    if (!expr) return NULL;
    AstTypeDecl *re = resolve_expression_to_type(expr, ctx, module);
    expr->resolved_type = re;
    return re;
}

static AstTypeDecl *resolve_expression_to_type(AstExpr *expr, Context *ctx, Module *module) {
    Token t = expr->token;
    switch (expr->tag) {
    case Node_STRING_LIT: return ctx->type_string;
    case Node_INT_LIT:    return ctx->type_int;
    case Node_BOOL_LIT:   return ctx->type_bool;
    case Node_FLOAT_LIT:  return ctx->type_f64;
    case Node_NULL_LIT:   return ctx->null_type;
    case Node_IMPORT:     return ctx->import_type;
    case Node_LIBRARY:    return ctx->type_library;

    case Node_TYPENAME: {
        return resolve_typename_as_rvalue((AstTypename *)expr, ctx, module);
    } break;

    case Node_PROCEDURE: {
        return resolve_procedure((AstProcedure *)expr, ctx, module);
    } break;

    case Node_IDENT: {
        auto ident = (AstIdent *)expr;
        return resolve_name(ctx, ident, expr, module);
    } break;


    case Node_CALL: {
        AstProcedure *resolved = resolve_call((AstCall *)expr, ctx, module);
        if (!resolved) return NULL;
        return resolved->return_type->resolved_type;
    } break;

    case Node_PAREN: {
        return resolve_expression(((AstParen *)expr)->sub_expr, ctx, module);
    } break;

    case Node_UNARY: {
        AstUnary *unary = (AstUnary *)expr;
        AstTypeDecl *expr_type = resolve_expression(unary->expr, ctx, module);
        if (unary->op == Token_BANG) {
            return ctx->type_bool;
        }
        if (unary->op == Token_STAR) {
            if (expr_type->expr_type != TypeDecl_POINTER) {
                compile_error(ctx, t, "expected pointer operand to dereference");
                return NULL;
            }
            return expr_type->base_type;
        }
        if (unary->op == Token_CARAT) {
            return make_pointer_type(expr_type);
        }
        return expr_type;
    } break;

    case Node_CAST: {
        AstCast *cast = (AstCast *)expr;
        assert(cast->type->tag == Node_TYPENAME);
        resolve_typename_in_type_slot(cast->type, ctx, module);
        resolve_expression(cast->expr, ctx, module);
        return cast->type->resolved_type;
    } break;

    case Node_INDEX: {
        AstArrayIndex *index = (AstArrayIndex *)expr;
        AstTypeDecl *resolved_name = resolve_expression(index->name, ctx, module);
        if (!resolved_name) return NULL;
        resolve_expression(index->index, ctx, module);
        return resolved_name->base_type;
    } break;

    case Node_BINARY: {
        AstBinary *bin = (AstBinary *)expr;
        if (bin->op == Token_DOT) {
            // TODO: this could be a reference to a typename in another module.
            return resolve_selector(ctx, bin, module);
        }
        if (is_binary_comparison(*bin)) {
            resolve_expression(bin->left, ctx, module);
            resolve_expression(bin->right, ctx, module);
            return ctx->type_bool;
        }
        if (is_assignment(*bin)) {
            resolve_assignment_expr(expr, ctx, module);
            if (bin->op != Token_EQUAL) { // it's a +=, -=, *= or /=
                return ctx->type_int; // so its type must be integer
            }
        }

        // Should only be maths operators left
        assert(bin->op == Token_PLUS || bin->op == Token_MINUS || bin->op == Token_SLASH || bin->op == Token_STAR);

        AstTypeDecl *left_type = resolve_expression(bin->left, ctx, module);
        AstTypeDecl *right_type = resolve_expression(bin->right, ctx, module);

        if (!left_type || !right_type) return NULL;

        bool left = is_type_numeric(left_type) || left_type->expr_type == TypeDecl_POINTER;
        bool right = is_type_numeric(right_type) || right_type->expr_type == TypeDecl_POINTER;
        if (!left || !right) {
            compile_error(ctx, t, "operands of arithmetic must be of pointer or numerical type");
            return NULL;
        }

        // You can add and substract to/from pointers
        if (bin->op == Token_PLUS || bin->op == Token_MINUS) {
            if (left_type->expr_type == TypeDecl_POINTER)
                return left_type;
            if (right_type->expr_type == TypeDecl_POINTER)
                return right_type;
        }

        return ctx->type_int;
    } break;
    }
    assert(false);
    return NULL;
}



void resolve_module(Context *ctx, Module *module) {
    resolve_decl(ctx->decl_for_main, ctx, module);

    // Resolving main will resolve all the symbols in the code
    // that are actually used. There may be some top level symbols
    // that are left as unresolved because they do not get used.
    // The following code exists to validate that these unused decls
    // are still correct, however we will keep their status as Status_UNRESOLVED
    // so that later on the compiler will correctly assert that they were not
    // referred to in the code.

#if 1
    Ast *ast = &module->ast;
    for (u64 i = 0; i < ast->len; i++) {
        AstNode *node = ast->nodes[i];
        if (node->tag == Node_DECL) {
            AstDecl *decl = (AstDecl *)node;
            if (decl->status == Status_RESOLVED) continue;
            decl->status = Status_RESOLVING;
            resolve_decl(decl, ctx, module);
            decl->status = Status_UNRESOLVED;
        }

        if (node->tag == Node_TYPE_DECL) {
            AstTypeDecl *decl = (AstTypeDecl *)node;
            if (decl->status == Status_RESOLVED) continue;
            resolve_type_decl(ctx, decl, node, false, module);
            decl->status = Status_UNRESOLVED;
        }

    }
#endif
    assert(ctx->block_stack.top == 0);
}

#include "codegen.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    EXPR_KIND_INT,
    EXPR_KIND_DOUBLE,
    EXPR_KIND_STRING,
    EXPR_KIND_CHAR
} ExprKind;

typedef struct VarInfo {
    char name[256];
    char c_type[32];
    int scope_level;
    struct VarInfo *next;
} VarInfo;

typedef struct {
    FILE *fp;
    int indent;
    int scope_level;
    VarInfo *vars;
} CodegenCtx;

static void emit_indent(CodegenCtx *ctx) {
    int i;
    for (i = 0; i < ctx->indent; i++) {
        fputs("    ", ctx->fp);
    }
}

static void safe_copy(char *dst, size_t dst_size, const char *src) {
    if (!dst || dst_size == 0) return;
    if (!src) {
        dst[0] = '\0';
        return;
    }
    strncpy(dst, src, dst_size - 1);
    dst[dst_size - 1] = '\0';
}

static const char *banglish_type_to_c(const char *type_name) {
    if (!type_name) return "int";
    if (strcmp(type_name, "purno") == 0) return "int";
    if (strcmp(type_name, "dosomik") == 0) return "double";
    if (strcmp(type_name, "torkik") == 0) return "int";
    if (strcmp(type_name, "shunno") == 0) return "void";
    return "int";
}

static void enter_scope(CodegenCtx *ctx) {
    ctx->scope_level++;
}

static void leave_scope(CodegenCtx *ctx) {
    VarInfo *cur = ctx->vars;
    VarInfo *prev = NULL;

    while (cur) {
        if (cur->scope_level == ctx->scope_level) {
            VarInfo *to_delete = cur;
            if (prev) {
                prev->next = cur->next;
            } else {
                ctx->vars = cur->next;
            }
            cur = cur->next;
            free(to_delete);
        } else {
            prev = cur;
            cur = cur->next;
        }
    }

    if (ctx->scope_level > 0) {
        ctx->scope_level--;
    }
}

static void register_var(CodegenCtx *ctx, const char *name, const char *c_type) {
    VarInfo *entry = (VarInfo *)calloc(1, sizeof(VarInfo));
    if (!entry) return;

    safe_copy(entry->name, sizeof(entry->name), name);
    safe_copy(entry->c_type, sizeof(entry->c_type), c_type);
    entry->scope_level = ctx->scope_level;
    entry->next = ctx->vars;
    ctx->vars = entry;
}

static const char *lookup_var_type(CodegenCtx *ctx, const char *name) {
    VarInfo *cur = ctx->vars;
    while (cur) {
        if (strcmp(cur->name, name) == 0) {
            return cur->c_type;
        }
        cur = cur->next;
    }
    return NULL;
}

static void emit_c_escaped_string(FILE *fp, const char *s) {
    const unsigned char *p = (const unsigned char *)s;
    fputc('"', fp);
    while (p && *p) {
        unsigned char ch = *p;
        switch (ch) {
            case '\\': fputs("\\\\", fp); break;
            case '"':  fputs("\\\"", fp); break;
            case '\n': fputs("\\n", fp); break;
            case '\r': fputs("\\r", fp); break;
            case '\t': fputs("\\t", fp); break;
            default:
                if (isprint(ch)) {
                    fputc((int)ch, fp);
                } else {
                    fprintf(fp, "\\x%02X", ch);
                }
                break;
        }
        p++;
    }
    fputc('"', fp);
}

static void emit_c_escaped_char(FILE *fp, char c) {
    fputc('\'', fp);
    switch (c) {
        case '\\': fputs("\\\\", fp); break;
        case '\'': fputs("\\'", fp); break;
        case '\n': fputs("\\n", fp); break;
        case '\r': fputs("\\r", fp); break;
        case '\t': fputs("\\t", fp); break;
        default:
            if (isprint((unsigned char)c)) {
                fputc(c, fp);
            } else {
                fprintf(fp, "\\x%02X", (unsigned char)c);
            }
            break;
    }
    fputc('\'', fp);
}

static const char *op_to_c(Operator op) {
    switch (op) {
        case OP_ADD: return "+";
        case OP_SUB: return "-";
        case OP_MUL: return "*";
        case OP_DIV: return "/";
        case OP_MOD: return "%";
        case OP_LT: return "<";
        case OP_GT: return ">";
        case OP_LE: return "<=";
        case OP_GE: return ">=";
        case OP_EQ: return "==";
        case OP_NE: return "!=";
        case OP_AND: return "&&";
        case OP_OR: return "||";
        case OP_NOT: return "!";
        case OP_NEG: return "-";
        case OP_INC: return "++";
        case OP_DEC: return "--";
        case OP_ASSIGN: return "=";
        case OP_ADD_ASSIGN: return "+=";
        case OP_SUB_ASSIGN: return "-=";
        case OP_MUL_ASSIGN: return "*=";
        case OP_DIV_ASSIGN: return "/=";
        default: return "?";
    }
}

static ExprKind infer_expr_kind(CodegenCtx *ctx, ASTNode *expr) {
    ExprKind lk;
    ExprKind rk;
    const char *vtype;

    if (!expr) return EXPR_KIND_INT;

    switch (expr->type) {
        case NODE_STRING_LIT:
            return EXPR_KIND_STRING;
        case NODE_CHAR_LIT:
            return EXPR_KIND_CHAR;
        case NODE_FLOAT_LIT:
            return EXPR_KIND_DOUBLE;
        case NODE_INT_LIT:
        case NODE_BOOL_LIT:
            return EXPR_KIND_INT;
        case NODE_IDENTIFIER:
            vtype = lookup_var_type(ctx, expr->name);
            if (vtype && strcmp(vtype, "double") == 0) {
                return EXPR_KIND_DOUBLE;
            }
            return EXPR_KIND_INT;
        case NODE_UNARY_EXPR:
            if (expr->op == OP_NOT || expr->op == OP_INC || expr->op == OP_DEC) {
                return EXPR_KIND_INT;
            }
            return infer_expr_kind(ctx, expr->left);
        case NODE_BINARY_EXPR:
            if (expr->op == OP_LT || expr->op == OP_GT || expr->op == OP_LE ||
                expr->op == OP_GE || expr->op == OP_EQ || expr->op == OP_NE ||
                expr->op == OP_AND || expr->op == OP_OR) {
                return EXPR_KIND_INT;
            }
            lk = infer_expr_kind(ctx, expr->left);
            rk = infer_expr_kind(ctx, expr->right);
            if (lk == EXPR_KIND_DOUBLE || rk == EXPR_KIND_DOUBLE) {
                return EXPR_KIND_DOUBLE;
            }
            return EXPR_KIND_INT;
        default:
            return EXPR_KIND_INT;
    }
}

static void emit_expr(CodegenCtx *ctx, ASTNode *expr) {
    ASTNode *arg;

    if (!expr) {
        fputs("0", ctx->fp);
        return;
    }

    switch (expr->type) {
        case NODE_INT_LIT:
        case NODE_BOOL_LIT:
            fprintf(ctx->fp, "%d", expr->int_val);
            break;
        case NODE_FLOAT_LIT:
            fprintf(ctx->fp, "%.17g", expr->float_val);
            break;
        case NODE_STRING_LIT:
            emit_c_escaped_string(ctx->fp, expr->string_val);
            break;
        case NODE_CHAR_LIT:
            emit_c_escaped_char(ctx->fp, expr->string_val[0]);
            break;
        case NODE_IDENTIFIER:
            fputs(expr->name, ctx->fp);
            break;
        case NODE_UNARY_EXPR:
            if (expr->op == OP_INC || expr->op == OP_DEC) {
                emit_expr(ctx, expr->left);
                fputs(op_to_c(expr->op), ctx->fp);
            } else {
                fputs(op_to_c(expr->op), ctx->fp);
                fputc('(', ctx->fp);
                emit_expr(ctx, expr->left);
                fputc(')', ctx->fp);
            }
            break;
        case NODE_BINARY_EXPR:
            fputc('(', ctx->fp);
            emit_expr(ctx, expr->left);
            fprintf(ctx->fp, " %s ", op_to_c(expr->op));
            emit_expr(ctx, expr->right);
            fputc(')', ctx->fp);
            break;
        case NODE_FUNC_CALL:
            fputs(expr->name, ctx->fp);
            fputc('(', ctx->fp);
            for (arg = expr->params; arg; arg = arg->next) {
                emit_expr(ctx, arg);
                if (arg->next) fputs(", ", ctx->fp);
            }
            fputc(')', ctx->fp);
            break;
        default:
            fputs("0", ctx->fp);
            break;
    }
}

static void emit_stmt(CodegenCtx *ctx, ASTNode *node);

static void emit_stmt_list(CodegenCtx *ctx, ASTNode *head) {
    ASTNode *cur;
    for (cur = head; cur; cur = cur->next) {
        emit_stmt(ctx, cur);
    }
}

static void emit_block(CodegenCtx *ctx, ASTNode *block_node) {
    emit_indent(ctx);
    fputs("{\n", ctx->fp);
    ctx->indent++;
    enter_scope(ctx);

    if (block_node && block_node->type == NODE_BLOCK) {
        emit_stmt_list(ctx, block_node->body);
    } else if (block_node) {
        emit_stmt(ctx, block_node);
    }

    leave_scope(ctx);
    ctx->indent--;
    emit_indent(ctx);
    fputs("}\n", ctx->fp);
}

static void emit_switch_case_body(CodegenCtx *ctx, ASTNode *case_body) {
    ASTNode *s;
    ctx->indent++;
    for (s = case_body; s; s = s->next) {
        emit_stmt(ctx, s);
    }
    ctx->indent--;
}

static void emit_print(CodegenCtx *ctx, ASTNode *expr) {
    ExprKind kind = infer_expr_kind(ctx, expr);
    emit_indent(ctx);

    if (kind == EXPR_KIND_STRING) {
        fputs("printf(\"%s\\n\", ", ctx->fp);
        emit_expr(ctx, expr);
        fputs(");\n", ctx->fp);
        return;
    }

    if (kind == EXPR_KIND_CHAR) {
        fputs("printf(\"%c\\n\", ", ctx->fp);
        emit_expr(ctx, expr);
        fputs(");\n", ctx->fp);
        return;
    }

    if (kind == EXPR_KIND_DOUBLE) {
        fputs("printf(\"%g\\n\", (double)", ctx->fp);
        emit_expr(ctx, expr);
        fputs(");\n", ctx->fp);
        return;
    }

    fputs("printf(\"%d\\n\", (int)", ctx->fp);
    emit_expr(ctx, expr);
    fputs(");\n", ctx->fp);
}

static void emit_input(CodegenCtx *ctx, const char *name) {
    const char *ctype = lookup_var_type(ctx, name);
    emit_indent(ctx);
    if (ctype && strcmp(ctype, "double") == 0) {
        fprintf(ctx->fp, "scanf(\"%%lf\", &%s);\n", name);
    } else {
        fprintf(ctx->fp, "scanf(\"%%d\", &%s);\n", name);
    }
}

static void emit_stmt(CodegenCtx *ctx, ASTNode *node) {
    ASTNode *p;
    ASTNode *c;
    const char *ctype;

    if (!node) return;

    switch (node->type) {
        case NODE_VAR_DECL:
            ctype = banglish_type_to_c(node->type_name);
            register_var(ctx, node->name, ctype);
            emit_indent(ctx);
            fprintf(ctx->fp, "%s %s", ctype, node->name);
            if (node->left) {
                fputs(" = ", ctx->fp);
                emit_expr(ctx, node->left);
            }
            fputs(";\n", ctx->fp);
            break;

        case NODE_CONST_DECL:
            ctype = banglish_type_to_c(node->type_name);
            register_var(ctx, node->name, ctype);
            emit_indent(ctx);
            fprintf(ctx->fp, "const %s %s", ctype, node->name);
            if (node->left) {
                fputs(" = ", ctx->fp);
                emit_expr(ctx, node->left);
            }
            fputs(";\n", ctx->fp);
            break;

        case NODE_ASSIGN:
            emit_indent(ctx);
            if (node->op == OP_INC || node->op == OP_DEC) {
                fprintf(ctx->fp, "%s%s;\n", node->name, op_to_c(node->op));
            } else {
                fprintf(ctx->fp, "%s %s ", node->name, op_to_c(node->op));
                emit_expr(ctx, node->left);
                fputs(";\n", ctx->fp);
            }
            break;

        case NODE_IF:
            emit_indent(ctx);
            fputs("if (", ctx->fp);
            emit_expr(ctx, node->left);
            fputs(")\n", ctx->fp);
            emit_block(ctx, node->body);
            if (node->right) {
                emit_indent(ctx);
                fputs("else\n", ctx->fp);
                if (node->right->type == NODE_IF) {
                    emit_indent(ctx);
                    fputs("{\n", ctx->fp);
                    ctx->indent++;
                    emit_stmt(ctx, node->right);
                    ctx->indent--;
                    emit_indent(ctx);
                    fputs("}\n", ctx->fp);
                } else {
                    emit_block(ctx, node->right);
                }
            }
            break;

        case NODE_WHILE:
            emit_indent(ctx);
            fputs("while (", ctx->fp);
            emit_expr(ctx, node->left);
            fputs(")\n", ctx->fp);
            emit_block(ctx, node->body);
            break;

        case NODE_FOR:
            emit_indent(ctx);
            fputs("for (", ctx->fp);
            if (node->init) {
                if (node->init->type == NODE_VAR_DECL) {
                    const char *it = banglish_type_to_c(node->init->type_name);
                    register_var(ctx, node->init->name, it);
                    fprintf(ctx->fp, "%s %s", it, node->init->name);
                    if (node->init->left) {
                        fputs(" = ", ctx->fp);
                        emit_expr(ctx, node->init->left);
                    }
                } else if (node->init->type == NODE_ASSIGN) {
                    fprintf(ctx->fp, "%s %s ", node->init->name, op_to_c(node->init->op));
                    emit_expr(ctx, node->init->left);
                } else {
                    emit_expr(ctx, node->init);
                }
            }
            fputs("; ", ctx->fp);
            if (node->left) emit_expr(ctx, node->left);
            fputs("; ", ctx->fp);
            if (node->update) {
                if (node->update->type == NODE_ASSIGN) {
                    if (node->update->op == OP_INC || node->update->op == OP_DEC) {
                        fprintf(ctx->fp, "%s%s", node->update->name, op_to_c(node->update->op));
                    } else {
                        fprintf(ctx->fp, "%s %s ", node->update->name, op_to_c(node->update->op));
                        emit_expr(ctx, node->update->left);
                    }
                } else {
                    emit_expr(ctx, node->update);
                }
            }
            fputs(")\n", ctx->fp);
            emit_block(ctx, node->body);
            break;

        case NODE_DO_WHILE:
            emit_indent(ctx);
            fputs("do\n", ctx->fp);
            emit_block(ctx, node->body);
            emit_indent(ctx);
            fputs("while (", ctx->fp);
            emit_expr(ctx, node->left);
            fputs(");\n", ctx->fp);
            break;

        case NODE_SWITCH:
            emit_indent(ctx);
            fputs("switch (", ctx->fp);
            emit_expr(ctx, node->left);
            fputs(") {\n", ctx->fp);
            ctx->indent++;
            for (c = node->body; c; c = c->next) {
                if (c->type == NODE_CASE) {
                    emit_indent(ctx);
                    fputs("case ", ctx->fp);
                    emit_expr(ctx, c->left);
                    fputs(":\n", ctx->fp);
                    emit_switch_case_body(ctx, c->body);
                } else if (c->type == NODE_DEFAULT_CASE) {
                    emit_indent(ctx);
                    fputs("default:\n", ctx->fp);
                    emit_switch_case_body(ctx, c->body);
                }
            }
            ctx->indent--;
            emit_indent(ctx);
            fputs("}\n", ctx->fp);
            break;

        case NODE_PRINT:
            emit_print(ctx, node->left);
            break;

        case NODE_INPUT:
            emit_input(ctx, node->name);
            break;

        case NODE_RETURN:
            emit_indent(ctx);
            fputs("return", ctx->fp);
            if (node->left) {
                fputc(' ', ctx->fp);
                emit_expr(ctx, node->left);
            }
            fputs(";\n", ctx->fp);
            break;

        case NODE_BREAK:
            emit_indent(ctx);
            fputs("break;\n", ctx->fp);
            break;

        case NODE_CONTINUE:
            emit_indent(ctx);
            fputs("continue;\n", ctx->fp);
            break;

        case NODE_FUNC_DEF: {
            const char *ret = banglish_type_to_c(node->type_name);
            emit_indent(ctx);
            fprintf(ctx->fp, "%s %s(", ret, node->name);
            for (p = node->params; p; p = p->next) {
                fprintf(ctx->fp, "%s %s", banglish_type_to_c(p->type_name), p->name);
                if (p->next) fputs(", ", ctx->fp);
            }
            fputs(")\n", ctx->fp);
            emit_indent(ctx);
            fputs("{\n", ctx->fp);
            ctx->indent++;
            enter_scope(ctx);
            for (p = node->params; p; p = p->next) {
                register_var(ctx, p->name, banglish_type_to_c(p->type_name));
            }
            if (node->body && node->body->type == NODE_BLOCK) {
                emit_stmt_list(ctx, node->body->body);
            }
            leave_scope(ctx);
            ctx->indent--;
            emit_indent(ctx);
            fputs("}\n\n", ctx->fp);
            break;
        }

        case NODE_EXPR_STMT:
            emit_indent(ctx);
            emit_expr(ctx, node->left);
            fputs(";\n", ctx->fp);
            break;

        case NODE_BLOCK:
            emit_block(ctx, node);
            break;

        default:
            break;
    }
}

static void emit_function_prototypes(CodegenCtx *ctx, ASTNode *head) {
    ASTNode *n;
    ASTNode *p;

    for (n = head; n; n = n->next) {
        if (n->type != NODE_FUNC_DEF) continue;
        fprintf(ctx->fp, "%s %s(", banglish_type_to_c(n->type_name), n->name);
        for (p = n->params; p; p = p->next) {
            fprintf(ctx->fp, "%s %s", banglish_type_to_c(p->type_name), p->name);
            if (p->next) fputs(", ", ctx->fp);
        }
        fputs(");\n", ctx->fp);
    }
}

static void clear_var_info(CodegenCtx *ctx) {
    VarInfo *cur = ctx->vars;
    while (cur) {
        VarInfo *nxt = cur->next;
        free(cur);
        cur = nxt;
    }
    ctx->vars = NULL;
}

int generate_c_code(ASTNode *root, const char *c_output_path, FILE *log) {
    FILE *fp;
    CodegenCtx ctx;
    ASTNode *top;

    if (!root || !c_output_path) return 0;

    fp = fopen(c_output_path, "w");
    if (!fp) {
        if (log) {
            fprintf(log, "\n[CODEGEN ERROR] C file khola gelo na: %s\n", c_output_path);
        }
        return 0;
    }

    memset(&ctx, 0, sizeof(ctx));
    ctx.fp = fp;

    fputs("#include <stdio.h>\n", fp);
    fputs("#include <stdlib.h>\n\n", fp);

    if (root->type != NODE_PROGRAM) {
        fclose(fp);
        if (log) {
            fprintf(log, "\n[CODEGEN ERROR] AST root NODE_PROGRAM noy.\n");
        }
        return 0;
    }

    emit_function_prototypes(&ctx, root->body);
    fputs("\n", fp);

    for (top = root->body; top; top = top->next) {
        if (top->type == NODE_FUNC_DEF) {
            emit_stmt(&ctx, top);
        }
    }

    fputs("int main(void)\n", fp);
    fputs("{\n", fp);
    ctx.indent = 1;
    ctx.scope_level = 1;

    emit_indent(&ctx);
    fputs("freopen(\"NUL\", \"r\", stdin);\n", fp);

    for (top = root->body; top; top = top->next) {
        if (top->type != NODE_FUNC_DEF) {
            emit_stmt(&ctx, top);
        }
    }

    emit_indent(&ctx);
    fputs("return 0;\n", fp);
    fputs("}\n", fp);

    clear_var_info(&ctx);
    fclose(fp);

    if (log) {
        fprintf(log, "\n[CODEGEN] Banglish theke C code toiri hoyeche: %s\n", c_output_path);
    }

    return 1;
}

static int write_combined_report(const char *c_path,
                                 const char *runtime_output_path,
                                 const char *compile_error_path,
                                 const char *report_path,
                                 int compile_ok,
                                 FILE *log) {
    FILE *report;
    FILE *in;
    char line[1024];

    report = fopen(report_path, "w");
    if (!report) {
        if (log) {
            fprintf(log, "\n[CODEGEN ERROR] Report file khola gelo na: %s\n", report_path);
        }
        return 0;
    }

    fputs("===== GENERATED C CODE =====\n\n", report);
    in = fopen(c_path, "r");
    if (in) {
        while (fgets(line, sizeof(line), in)) {
            fputs(line, report);
        }
        fclose(in);
    } else {
        fputs("(C file porte parini)\n", report);
    }

    if (compile_ok) {
        fputs("\n===== PROGRAM OUTPUT =====\n\n", report);
        in = fopen(runtime_output_path, "r");
        if (in) {
            while (fgets(line, sizeof(line), in)) {
                fputs(line, report);
            }
            fclose(in);
        } else {
            fputs("(Runtime output file porte parini)\n", report);
        }
    } else {
        fputs("\n===== C COMPILATION ERROR =====\n\n", report);
        in = fopen(compile_error_path, "r");
        if (in) {
            while (fgets(line, sizeof(line), in)) {
                fputs(line, report);
            }
            fclose(in);
        } else {
            fputs("(Compilation error file porte parini)\n", report);
        }
    }

    fclose(report);
    return 1;
}

int compile_and_run_c_code(const char *c_path,
                           const char *exe_path,
                           const char *runtime_output_path,
                           const char *report_path,
                           FILE *log) {
    char compile_cmd[2048];
    char run_cmd[2048];
    char compile_error_path[1024];
    int compile_status;
    int run_status;

    if (!c_path || !exe_path || !runtime_output_path || !report_path) {
        return 0;
    }

    snprintf(compile_error_path, sizeof(compile_error_path), "%s.compile_errors.txt", c_path);

    snprintf(compile_cmd, sizeof(compile_cmd),
             "gcc \"%s\" -o \"%s\" 2> \"%s\"",
             c_path, exe_path, compile_error_path);

    compile_status = system(compile_cmd);
    if (compile_status != 0) {
        if (log) {
            fprintf(log, "\n[CODEGEN ERROR] Generated C code compile hoyni.\n");
        }
        write_combined_report(c_path, runtime_output_path, compile_error_path, report_path, 0, log);
        return 0;
    }

    snprintf(run_cmd, sizeof(run_cmd),
             "%s > \"%s\" 2>&1",
             exe_path, runtime_output_path);

    run_status = system(run_cmd);
    if (run_status != 0) {
        if (log) {
            fprintf(log, "\n[CODEGEN WARNING] Program run korar somoy non-zero status peyechi.\n");
        }
    }

    write_combined_report(c_path, runtime_output_path, compile_error_path, report_path, 1, log);

    if (log) {
        fprintf(log, "[CODEGEN] Runtime output: %s\n", runtime_output_path);
        fprintf(log, "[CODEGEN] Combined report: %s\n", report_path);
    }

    return 1;
}

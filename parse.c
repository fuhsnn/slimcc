// This file contains a recursive descent parser for C.
//
// Most functions in this file are named after the symbols they are
// supposed to read from an input token list. For example, stmt() is
// responsible for reading a statement from a token list. The function
// then construct an AST node representing a statement.
//
// Each function conceptually returns two values, an AST node and
// remaining part of the input tokens. Since C doesn't support
// multiple return values, the remaining tokens are returned to the
// caller via a pointer argument.
//
// Input tokens are represented by a linked list. Unlike many recursive
// descent parsers, we don't have the notion of the "input token stream".
// Most parsing functions don't change the global state of the parser.
// So it is very easy to lookahead arbitrary number of tokens in this
// parser.

#include "slimcc.h"

// Scope for local variables, global variables, typedefs
// or enum constants
typedef struct {
  Obj *var;
  Type *type_def;
  Type *enum_ty;
  int64_t enum_val;
} VarScope;

// Variable attributes such as typedef or extern.
typedef struct {
  bool is_typedef;
  bool is_static;
  bool is_extern;
  bool is_inline;
  bool is_register;
  bool is_tls;
  bool is_constexpr;
  bool is_weak;
  bool is_gnu_inline;
  bool is_returns_twice;
  bool is_ctor;
  bool is_dtor;
  uint16_t ctor_prior;
  uint16_t dtor_prior;
  Obj *cleanup_fn;
  char *alias;
  char *section;
  char *visibility;
  int align;
} VarAttr;

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
typedef struct Initializer Initializer;
struct Initializer {
  Type *ty;

  enum {
    INIT_NONE = 0,
    INIT_FLEXIBLE,
    INIT_LIST,
    INIT_EXPR,
    INIT_TOK
  } kind;

  int mem_cnt;
  Initializer *mem_arr;
ANON_UNION_START
    Token *tok;
    Node *expr;
    Member *initmem;
ANON_UNION_END
};

// For local variable initializer.
typedef struct InitDesg InitDesg;
struct InitDesg {
  InitDesg *next;
  int idx;
  Member *member;
  Obj *var;
};

typedef enum {
  EV_CONST,     // constant expression
  EV_LABEL,     // relocation label
  EV_AGGREGATE, // struct/union/array
} EvalKind;

typedef struct {
  EvalKind kind;
  char **label;
  Obj *var;
  int deref_cnt;
  bool let_array;
  bool let_atomic;
  bool let_volatile;
} EvalContext;

typedef struct JumpContext JumpContext;
struct JumpContext {
  JumpContext *next;
  char *brk_label;
  char *cont_label;
  DeferStmt *defr_end;
  Token *labels;
  Node *switch_node;
};

static JumpContext *jump_ctx;

// Likewise, global variables are accumulated to this list.
static Obj *globals;

static Scope *scope = &(Scope){0};

// Points to the function object the parser is currently parsing.
static Obj *current_fn;

// Lists of all goto statements and labels in the curent function.
static Node *gotos;
static Node *labels;

static DeferStmt *current_defr;

static bool fn_use_vla;
static bool dont_dealloc_vla;
static bool is_global_init_context;
static bool *eval_recover;

static bool is_typename(Token *tok);
static Type *declspec(Token **rest, Token *tok, VarAttr *attr);
static Type *typename(Token **rest, Token *tok);
static Type *enum_specifier(Token **rest, Token *tok);
static Type *typeof_specifier(Token **rest, Token *tok);
static Type *type_suffix(Token **rest, Token *tok, Type *ty);
static Type *declarator(Token **rest, Token *tok, Type *ty, Token **name_tok);
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr);
static void list_initializer(Token **rest, Token *tok, Initializer *init, int i);
static void initializer2(Token **rest, Token *tok, Initializer *init);
static Type *initializer(Token **rest, Initializer *init, Token *tok, Type *ty);
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var);
static void gvar_initializer(Token **rest, Token *tok, Obj *var);
static void constexpr_initializer(Token **rest, Token *tok, Obj *init_var, Obj *var);
static Node *compound_stmt(Token **rest, Token *tok, NodeKind kind);
static Node *stmt(Token **rest, Token *tok, Token *label_list);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static int64_t eval(Node *node);
static int64_t eval2(Node *node, EvalContext *ctx);
static Obj *eval_var(Node *node, int *ofs, bool let_volatile);
static Node *assign(Token **rest, Token *tok);
static Node *log_or(Token **rest, Token *tok);
static long double eval_double(Node *node);
static uint64_t *eval_bitint(Node *node);
static Node *conditional(Token **rest, Token *tok);
static Node *log_and(Token **rest, Token *tok);
static Node *bit_or(Token **rest, Token *tok);
static Node *bit_xor(Token **rest, Token *tok);
static Node *bit_and(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *shift(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *new_add(Node *lhs, Node *rhs, Token *tok);
static Node *new_sub(Node *lhs, Node *rhs, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *cast(Token **rest, Token *tok);
static Member *get_struct_member(Type *ty, Token *tok);
static Type *struct_union_decl(Token **rest, Token *tok, TypeKind kind);
static Type *struct_decl(Type *ty, int alt_align);
static Type *union_decl(Type *ty, int alt_align);
static Node *postfix(Node *node, Token **rest, Token *tok);
static Node *funcall(Token **rest, Token *tok, Node *node);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);
static Node *parse_typedef(Token **rest, Token *tok, Type *basety, VarAttr *attr);
static Obj *func_prototype(Type *ty, VarAttr *attr, Token *name);
static void global_declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr);
static Node *calc_vla(Type *ty, Token *tok);
static int64_t const_expr2(Token **rest, Token *tok, Type **ty);
static Node *new_node(NodeKind kind, Token *tok);

static int align_down(int n, int align) {
  return align_to(n - align + 1, align);
}

static void enter_scope(void) {
  Scope *sc = ast_arena_calloc(sizeof(Scope));
  sc->parent = scope;
  sc->sibling_next = scope->children;
  scope = scope->children = sc;
}

static void enter_tmp_scope(void) {
  enter_scope();
  scope->is_temporary = true;
}

static void leave_scope(void) {
  if (current_fn) {
    free(scope->vars.buckets);
    free(scope->tags.buckets);
  }
  scope = scope->parent;
}

static DeferStmt *new_block_scope(void) {
  enter_scope();
  return current_defr;
}

static Node *leave_block_scope(DeferStmt *defr, Node *stmt_node) {
  leave_scope();

  if (stmt_node->kind == ND_RETURN || stmt_node->kind == ND_GOTO)
    current_defr = defr;

  if (defr == current_defr && !stmt_node->next)
    return stmt_node;

  Node *blk = new_node(ND_BLOCK, stmt_node->tok);
  blk->defr_start = current_defr;
  blk->defr_end = current_defr = defr;
  blk->body = stmt_node;
  return blk;
}

static bool is_constant_context(void) {
  return scope->parent == NULL || is_global_init_context;
}

// Find a variable by name.
static VarScope *find_var(Token *tok) {
  for (Scope *sc = scope; sc; sc = sc->parent) {
    VarScope *sc2 = hashmap_get2(&sc->vars, tok->loc, tok->len);
    if (sc2)
      return sc2;
  }
  return NULL;
}

static Type *find_tag(Token *tok) {
  for (Scope *sc = scope; sc; sc = sc->parent) {
    Type *ty = hashmap_get2(&sc->tags, tok->loc, tok->len);
    if (ty)
      return ty;
  }
  return NULL;
}

static Type *get_first_64bit_int(bool is_unsigned) {
  if (ty_long->size == 8)
    return is_unsigned ? ty_ulong : ty_long;
  if (ty_llong->size == 8)
    return is_unsigned ? ty_ullong : ty_llong;
  internal_error();
}

static int32_t bitfield_footprint(Member *mem) {
  return align_to(mem->bit_width + mem->bit_offset, 8) / 8;
}

static bool less_eq(Type *ty, int64_t lhs, int64_t rhs) {
  if (ty->is_unsigned && ty->size == 8)
    return (uint64_t)lhs <= rhs;
  return lhs <= rhs;
}

bool is_const_var(Obj *var) {
  Type *ty = var->ty;
  for (; ty && ty->kind == TY_ARRAY; ty = ty->base)
    if (ty->is_const)
      return true;
  return ty->is_const;
}

static bool is_int_class(Type *ty) {
  return is_integer(ty) || ty->kind == TY_BITINT;
}

bool equal_tok(Token *a, Token *b) {
  return a->len == b->len && !memcmp(a->loc, b->loc, b->len);
}

static Node *new_node(NodeKind kind, Token *tok) {
  Node *node = arena_calloc(&node_arena, sizeof(Node));
  node->kind = kind;
  node->tok = tok;
  return node;
}

static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {
  Node *node = new_node(kind, tok);
  node->lhs = lhs;
  node->rhs = rhs;
  return node;
}

static Node *new_unary(NodeKind kind, Node *expr, Token *tok) {
  Node *node = new_node(kind, tok);
  node->lhs = expr;
  return node;
}

static Node *new_num(int64_t val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  return node;
}

static Node *new_intptr_t(int64_t val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  node->ty = ty_intptr_t;
  return node;
}

static Node *new_size_t(int64_t val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  node->ty = ty_size_t;
  return node;
}

static Node *new_boolean(bool val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  node->ty = ty_bool;
  return node;
}

static Node *new_var_node(Obj *var, Token *tok) {
  Node *node = new_node(ND_VAR, tok);
  node->var = var;
  return node;
}

static bool invalid_cast(Node *node, Type *to) {
  if (to->kind == TY_NULLPTR)
    return !is_nullptr(node);

  if (node->ty->kind == TY_NULLPTR) {
    switch (to->kind) {
    case TY_VOID:
    case TY_BOOL:
    case TY_PTR:
      return false;
    }
    return true;
  }

  if (to->kind != TY_VOID) {
    switch (node->ty->kind) {
    case TY_VOID:
    case TY_STRUCT:
    case TY_UNION:
      return true;
    }
  }
  if (node->ty->base && is_flonum(to))
    return true;

  return false;
}

Node *new_cast(Node *expr, Type *ty) {
  add_type(expr);
  ty = unqual(ty);

  if (invalid_cast(expr, ty))
    error_tok(expr->tok, "invalid cast");

  Node tmp_node = {.kind = ND_CAST, .tok = expr->tok, .lhs = expr, .ty = ty};
  if (opt_optimize) {
    if ((is_integer(ty) && is_const_expr(&tmp_node, &tmp_node.val)) ||
      (is_flonum(ty) && is_const_double(&tmp_node, &tmp_node.fval))) {
      expr->kind = ND_NUM;
      expr->val = tmp_node.val;
      expr->fval = tmp_node.fval;
      expr->ty = ty;
      return expr;
    }

    if (!is_bitfield(expr)) {
      if (expr->ty == ty)
        return expr;

      if (expr->ty->origin && expr->ty->origin == ty) {
        expr->ty = ty;
        return expr;
      }

      if (expr->ty->kind == TY_BITINT && ty->kind == TY_BITINT &&
        expr->ty->is_unsigned == ty->is_unsigned &&
        expr->ty->bit_cnt == ty->bit_cnt)
        return expr;
    }

    if (is_redundant_cast(expr, ty)) {
      expr->ty = ty;
      return expr;
    }
  }
  Node *node = arena_malloc(&node_arena, sizeof(Node));
  *node = tmp_node;
  return node;
}

static Node *assign_cast(Type *to, Node *expr) {
  Node ty_node = {.kind = ND_NULL_EXPR, .ty = to, .tok = expr->tok};
  Node tmp_node = {.kind = ND_ASSIGN, .lhs = &ty_node, .rhs = expr, .tok = expr->tok};
  add_type(&tmp_node);
  return tmp_node.rhs;
}

static Node *cond_cast(Node *expr) {
  switch (expr->kind) {
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
  case ND_LOGAND:
  case ND_LOGOR:
  case ND_NOT:
    return expr;
  }
  return new_cast(expr, ty_bool);
}

static void apply_cv_qualifier(Node *node, Type *ty2) {
  add_type(node);
  Type *ty = node->ty;
  if (ty->is_const < ty2->is_const || ty->is_volatile < ty2->is_volatile) {
    node->ty = new_qualified_type(ty);
    node->ty->is_const = ty->is_const | ty2->is_const;
    node->ty->is_volatile = ty->is_volatile | ty2->is_volatile;
  }
}

static VarScope *push_scope(char *name) {
  VarScope *sc = ast_arena_calloc(sizeof(VarScope));
  hashmap_put(&scope->vars, name, sc);
  return sc;
}

static void free_initializers(Initializer *init) {
  if (init->mem_cnt) {
    for (int i = 0; i < init->mem_cnt; i++)
      free_initializers(&init->mem_arr[i]);
    free(init->mem_arr);
  }
}

static void new_initializer(Initializer *init, Type *ty, bool is_flexible) {
  init->ty = ty;

  if (ty->kind == TY_ARRAY) {
    if (is_flexible && ty->size < 0) {
      init->kind = INIT_FLEXIBLE;
      return;
    }
    init->kind = INIT_LIST;
    init->mem_arr = calloc(ty->array_len, sizeof(Initializer));
    init->mem_cnt = ty->array_len;

    for (int i = 0; i < ty->array_len; i++)
      new_initializer(&init->mem_arr[i], ty->base, false);
    return;
  }
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    // Count the number of struct members.
    int len = 0;
    for (Member *mem = ty->members; mem_iter(&mem); mem = mem->next)
      mem->idx = len++;

    init->kind = INIT_LIST;
    init->mem_arr = calloc(len, sizeof(Initializer));
    init->mem_cnt = len;

    for (Member *mem = ty->members; mem_iter(&mem); mem = mem->next) {
      Initializer *child = &init->mem_arr[mem->idx];

      if (!mem->next && ty->kind == TY_STRUCT && is_flexible && ty->is_flexible) {
        child->ty = mem->ty;
        child->kind = INIT_FLEXIBLE;
        break;
      }
      new_initializer(child, mem->ty, false);
    }
    return;
  }
}

static Obj *new_var(char *name, Type *ty, Obj *var) {
  var->name = name;
  var->ty = ty;
  var->align = ty->align;
  if (name)
    push_scope(name)->var = var;
  return var;
}

Obj *new_lvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty, ast_arena_calloc(sizeof(Obj)));
  var->is_local = true;
  var->next = scope->locals;
  scope->locals = var;
  return var;
}

static Obj *new_gvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty, calloc(1, sizeof(Obj)));
  var->next = globals;
  globals = var;
  return var;
}

static char *new_unique_name(void) {
  static int64_t id = 0;
  return format(".L..%"PRIi64, id++);
}

static Obj *new_static_lvar(Type *ty) {
  Obj *var = new_var(NULL, ty, ast_arena_calloc(sizeof(Obj)));
  var->name = new_unique_name();
  var->is_definition = true;
  var->is_static = true;

  var->next = current_fn->static_lvars;
  current_fn->static_lvars = var;
  return var;
}

static Obj *new_anon_gvar(Type *ty) {
  if (current_fn)
    return new_static_lvar(ty);
  Obj *var = new_gvar(NULL, ty);
  var->name = new_unique_name();
  var->is_definition = true;
  var->is_static = true;
  return var;
}

static DeferStmt *new_defr(DeferKind kind) {
  DeferStmt *defr = arena_calloc(&ast_arena, sizeof(DeferStmt));
  defr->kind = kind;
  if (current_defr) {
    defr->vla = current_defr->vla;
    defr->next = current_defr;
  }
  current_defr = defr;
  return defr;
}

static char *get_ident(Token *tok) {
  if (tok->kind != TK_IDENT)
    error_tok(tok, "expected an identifier");
  return strndup(tok->loc, tok->len);
}

static Type *find_typedef(Token *tok) {
  if (tok->kind == TK_IDENT) {
    VarScope *sc = find_var(tok);
    if (sc)
      return sc->type_def;
  }
  return NULL;
}

static Token *ident_tok(Token **rest, Token *tok) {
  if (tok->kind != TK_IDENT)
    error_tok(tok, "expected an identifier");
  *rest = tok->next;
  return tok;
}

static Token *str_tok(Token **rest, Token *tok) {
  if (tok->kind != TK_STR)
    error_tok(tok, "expected string literal");
  *rest = tok->next;
  return tok;
}

static void push_tag_scope(Token *tok, Type *ty) {
  hashmap_put2(&scope->tags, tok->loc, tok->len, ty);
}

static void chain_expr(Node **lhs, Node *rhs) {
  if (rhs)
    *lhs = !*lhs ? rhs : new_binary(ND_CHAIN, *lhs, rhs, rhs->tok);
}

static bool comma_list(Token **rest, Token **tok_rest, char *end, bool skip_comma) {
  Token *tok = *tok_rest;
  if (consume(rest, tok, end))
    return false;

  if (skip_comma) {
    tok = skip(tok, ",");

    // curly brackets allow trailing comma
    if (!strcmp(end, "}") && consume(rest, tok, "}"))
      return false;

    *tok_rest = tok;
  }
  return true;
}

static bool equal_kw(Token *tok, char *op) {
  return tok->kind == TK_KEYWORD && equal(tok, op);
}

static bool equal_tykw(Token *tok, char *op) {
  return tok->kind == TK_TYPEKW && equal(tok, op);
}

static bool get_attr_val(Token *tok, int64_t *val) {
  if (equal(tok, "(")) {
    *val = const_expr(&tok, tok);
    return true;
  }
  return false;
}

static void attr_aligned(Token *loc, int *align, TokenKind kind) {
  for (Token *tok = loc->attr_next; tok; tok = tok->attr_next) {
    if (tok->kind != kind)
      continue;
    if (equal_ext(tok, "aligned")) {
      Token *tok2;
      if (consume(&tok2, tok->next, "(")) {
        int align2 = const_expr(&tok2, tok2);
        *align = MAX(*align, align2);
        continue;
      }
      *align = MAX(*align, 16);
    }
  }
}

static void attr_cleanup(Token *loc, TokenKind kind, Obj **fn) {
  if (*fn)
    return;
  for (Token *tok = loc->attr_next; tok; tok = tok->attr_next) {
    if (tok->kind != kind)
      continue;
    if (equal_ext(tok, "cleanup")) {
      VarScope *sc = find_var(skip(tok->next, "("));
      if (!(sc && sc->var && sc->var->ty->kind == TY_FUNC))
        error_tok(tok, "cleanup function not found");
      strarray_push(&current_fn->refs, sc->var->name);
      *fn = sc->var;
      return;
    }
  }
}

static void apply_cdtor_attr(char *attr_name, Token *tok, bool *is_cdtor, uint16_t *priority, uint16_t pri, bool apply) {
  if (apply) {
    if (*is_cdtor && *priority != pri)
      error_tok(tok, "%s priority conflict", attr_name);
    *is_cdtor = true;
    *priority = pri;
  }
}

static void cdtor_attr(Token *loc, TokenKind kind, char *name, bool *is_cdtor, uint16_t *priority) {
  for (Token *tok = loc->attr_next; tok; tok = tok->attr_next) {
    if (tok->kind != kind)
      continue;
    if (equal_ext(tok, name)) {
      uint16_t pri = 0;
      int64_t val;
      if (get_attr_val(tok->next, &val))
        pri = MIN(val, 65535L) + 1;
      apply_cdtor_attr(name, tok, is_cdtor, priority, pri, true);
    }
  }
}

static void bool_attr(Token *loc, TokenKind kind, char *name, bool *b) {
  for (Token *tok = loc->attr_next; tok; tok = tok->attr_next) {
    if (tok->kind == kind && equal_ext(tok, name)) {
      *b = true;
      return;
    }
  }
}

static void apply_str_attr(char *attr_name, Token *tok, char **var_str, char *attr_str) {
  if (!*var_str) {
    *var_str = attr_str;
    return;
  }
  if (attr_str && strcmp(*var_str, attr_str))
    error_tok(tok, "conflict of attribute \'%s\'", attr_name);
}

static void str_attr(Token *loc, TokenKind kind, char *name, char **str) {
  for (Token *tok = loc->attr_next; tok; tok = tok->attr_next) {
    if (tok->kind == kind && equal_ext(tok, name)) {
      Token *t;
      apply_str_attr(name, tok, str, str_tok(&t, skip(tok->next, "("))->str);
      skip(t, ")");
      return;
    }
  }
}

static void tyspec_attr(Token *tok, VarAttr *attr, TokenKind kind) {
  attr_aligned(tok, &attr->align, kind);
  attr_cleanup(tok, kind, &attr->cleanup_fn);
  bool_attr(tok, kind, "weak", &attr->is_weak);
  bool_attr(tok, kind, "gnu_inline", &attr->is_gnu_inline);
  bool_attr(tok, kind, "returns_twice", &attr->is_returns_twice);
  cdtor_attr(tok, kind, "constructor", &attr->is_ctor, &attr->ctor_prior);
  cdtor_attr(tok, kind, "destructor", &attr->is_dtor, &attr->dtor_prior);
  str_attr(tok, kind, "alias", &attr->alias);
  str_attr(tok, kind, "section", &attr->section);
  str_attr(tok, kind, "visibility", &attr->visibility);
}

#define DeclAttr(Fn, ...)                \
  Fn(name, TK_ATTR, __VA_ARGS__);        \
  Fn(name->next, TK_BATTR, __VA_ARGS__); \
  Fn(tok, TK_ATTR, __VA_ARGS__)

static void symbol_attr(Token **rest, Token *tok, Obj *var, VarAttr *attr, Token *name) {
  if (equal_kw(tok, "asm") || equal(tok, "__asm") || equal(tok, "__asm__")) {
    var->asm_name = str_tok(&tok, skip(tok->next, "("))->str;
    *rest = skip(tok, ")");
  }

  apply_str_attr("alias", name, &var->alias_name, attr->alias);
  DeclAttr(str_attr, "alias", &var->alias_name);

  apply_str_attr("section", name, &var->section_name, attr->section);
  DeclAttr(str_attr, "section", &var->section_name);

  apply_str_attr("visibility", name, &var->visibility, attr->visibility);
  DeclAttr(str_attr, "visibility", &var->visibility);

  var->is_weak |= attr->is_weak;
  DeclAttr(bool_attr, "weak", &var->is_weak);
}

static void func_attr(Obj *fn, VarAttr *attr, Token *name, Token *tok) {
  apply_cdtor_attr("constructor", name, &fn->is_ctor, &fn->ctor_prior, attr->ctor_prior, attr->is_ctor);
  DeclAttr(cdtor_attr, "constructor", &fn->is_ctor, &fn->ctor_prior);

  apply_cdtor_attr("destructor", name, &fn->is_dtor, &fn->dtor_prior, attr->dtor_prior, attr->is_dtor);
  DeclAttr(cdtor_attr, "destructor", &fn->is_dtor, &fn->dtor_prior);

  fn->returns_twice |= attr->is_returns_twice;
  DeclAttr(bool_attr, "returns_twice", &fn->returns_twice);

  if (equal(tok, "{")) {
    bool is_gnu_inline = attr->is_gnu_inline;
    DeclAttr(bool_attr, "gnu_inline", &is_gnu_inline);
    fn->only_inline = is_gnu_inline && attr->is_inline && attr->is_extern;
  }
}

// declspec = ("void" | "_Bool" | "char" | "short" | "int" | "long"
//             | "typedef" | "static" | "extern" | "inline"
//             | "_Thread_local" | "__thread"
//             | "signed" | "unsigned"
//             | struct-decl | union-decl | typedef-name
//             | enum-specifier | typeof-specifier
//             | "const" | "volatile" | "auto" | "register" | "restrict"
//             | "__restrict" | "__restrict__" | "_Noreturn")+
//
// The order of typenames in a type-specifier doesn't matter. For
// example, `int long static` means the same as `static long int`.
// That can also be written as `static long` because you can omit
// `int` if `long` or `short` are specified. However, something like
// `char int` is not a valid type specifier. We have to accept only a
// limited combinations of the typenames.
//
// In this function, we count the number of occurrences of each typename
// while keeping the "current" type object that the typenames up
// until that point represent. When we reach a non-typename token,
// we returns the current type object.
static Type *declspec(Token **rest, Token *tok, VarAttr *attr) {
  // We use a single integer as counters for all typenames.
  // For example, bits 0 and 1 represents how many times we saw the
  // keyword "void" so far. With this, we can use a switch statement
  // as you can see below.
  enum {
    VOID     = 1 << 0,
    BOOL     = 1 << 2,
    CHAR     = 1 << 4,
    SHORT    = 1 << 6,
    INT      = 1 << 8,
    LONG     = 1 << 10,
    FLOAT    = 1 << 12,
    DOUBLE   = 1 << 14,
    OTHER    = 1 << 16,
    SIGNED   = 1 << 17,
    UNSIGNED = 1 << 18,
    BITINT   = 1 << 19,
  };

  if (attr)
    tyspec_attr(tok, attr, TK_BATTR);

  Type *ty = NULL;
  int counter = 0;
  bool is_atomic = false;
  bool is_const = false;
  bool is_restrict = false;
  bool is_volatile = false;
  bool is_auto = false;
  for (;;) {
    if (attr)
      tyspec_attr(tok, attr, TK_ATTR);

    if (!is_typename(tok))
      break;

    // Handle storage class specifiers.
    if (equal(tok, "typedef") || equal(tok, "static") || equal(tok, "extern") ||
        equal(tok, "inline") || equal(tok, "_Thread_local") || equal(tok, "__thread") ||
        equal_tykw(tok, "thread_local")) {
      if (!attr)
        error_tok(tok, "storage class specifier is not allowed in this context");

      if (equal(tok, "typedef"))
        attr->is_typedef = true;
      else if (equal(tok, "static"))
        attr->is_static = true;
      else if (equal(tok, "extern"))
        attr->is_extern = true;
      else if (equal(tok, "inline"))
        attr->is_inline = true;
      else
        attr->is_tls = true;

      if (attr->is_typedef &&
          attr->is_static + attr->is_extern + attr->is_inline + attr->is_tls > 1)
        error_tok(tok, "typedef may not be used together with static,"
                  " extern, inline, __thread or _Thread_local");
      tok = tok->next;
      continue;
    }

    if (equal_tykw(tok, "constexpr")) {
      if (!attr)
        error_tok(tok, "constexpr not allowed in this context");
      attr->is_constexpr = true;
      is_const = true;
      tok = tok->next;
      continue;
    }

    if (consume(&tok, tok, "register") && attr) {
      attr->is_register = true;
      continue;
    }
    // These keywords are recognized but ignored.
    if (consume(&tok, tok, "_Noreturn"))
      continue;

    if (equal(tok, "__auto_type") || equal(tok, "auto")) {
      if (equal(tok, "__auto_type") || opt_std >= STD_C23)
        is_auto = true;
      tok = tok->next;
      continue;
    }

    if (consume(&tok, tok, "_Atomic")) {
      if (consume(&tok, tok , "(")) {
        ty = typename(&tok, tok);
        tok = skip(tok, ")");
      }
      is_atomic = true;
      continue;
    }

    if (consume(&tok, tok, "const")) {
      is_const = true;
      continue;
    }

    if (consume(&tok, tok, "volatile") || consume(&tok, tok, "__volatile") ||
        consume(&tok, tok, "__volatile__")) {
      is_volatile = true;
      continue;
    }

    if (consume(&tok, tok, "restrict") || consume(&tok, tok, "__restrict") ||
        consume(&tok, tok, "__restrict__")) {
      is_restrict = true;
      continue;
    }

    if (equal(tok, "_Alignas") || equal_tykw(tok, "alignas")) {
      if (!attr)
        error_tok(tok, "_Alignas is not allowed in this context");
      tok = skip(tok->next, "(");
      int align;
      if (is_typename(tok))
        align = typename(&tok, tok)->align;
      else
        align = const_expr(&tok, tok);
      attr->align = MAX(attr->align, align);
      tok = skip(tok, ")");
      continue;
    }

    Type *ty2 = find_typedef(tok);
    if (ty2) {
      if (counter || is_auto)
        break;
      ty = ty2;
      tok = tok->next;
      counter += OTHER;
      continue;
    }

    if (equal(tok, "struct") || equal(tok, "union") || equal(tok, "enum") ||
        equal(tok, "__typeof") || equal(tok, "__typeof__") ||
        equal_tykw(tok, "typeof") || equal_tykw(tok, "typeof_unqual")) {
      if (counter)
        error_tok(tok, "invalid type");

      if (equal(tok, "struct"))
        ty = struct_union_decl(&tok, tok->next, TY_STRUCT);
      else if (equal(tok, "union"))
        ty = struct_union_decl(&tok, tok->next, TY_UNION);
      else if (equal(tok, "enum"))
        ty = enum_specifier(&tok, tok->next);
      else if (equal(tok, "typeof_unqual"))
        ty = unqual(typeof_specifier(&tok, tok->next));
      else
        ty = typeof_specifier(&tok, tok->next);

      counter += OTHER;
      continue;
    }

    // Handle built-in types.
    if (equal(tok, "void"))
      counter += VOID;
    else if (equal(tok, "_Bool") || equal_tykw(tok, "bool"))
      counter += BOOL;
    else if (equal(tok, "char"))
      counter += CHAR;
    else if (equal(tok, "short"))
      counter += SHORT;
    else if (equal(tok, "int"))
      counter += INT;
    else if (equal(tok, "long"))
      counter += LONG;
    else if (equal(tok, "float"))
      counter += FLOAT;
    else if (equal(tok, "double"))
      counter += DOUBLE;
    else if (equal(tok, "signed"))
      counter |= SIGNED;
    else if (equal(tok, "unsigned"))
      counter |= UNSIGNED;
    else if (equal(tok, "_BitInt")) {
      counter |= BITINT;
      int64_t width = const_expr(&tok, skip(tok->next, "("));
      ty = new_bitint(width, tok);
      skip(tok, ")");
    } else {
      internal_error();
    }

    switch (counter) {
    case VOID:
      ty = ty_void;
      break;
    case BOOL:
      ty = ty_bool;
      break;
    case CHAR:
      ty = ty_pchar;
      break;
    case SIGNED + CHAR:
      ty = ty_char;
      break;
    case UNSIGNED + CHAR:
      ty = ty_uchar;
      break;
    case SHORT:
    case SHORT + INT:
    case SIGNED + SHORT:
    case SIGNED + SHORT + INT:
      ty = ty_short;
      break;
    case UNSIGNED + SHORT:
    case UNSIGNED + SHORT + INT:
      ty = ty_ushort;
      break;
    case INT:
    case SIGNED:
    case SIGNED + INT:
      ty = ty_int;
      break;
    case UNSIGNED:
    case UNSIGNED + INT:
      ty = ty_uint;
      break;
    case LONG:
    case LONG + INT:
    case SIGNED + LONG:
    case SIGNED + LONG + INT:
      ty = ty_long;
      break;
    case LONG + LONG:
    case LONG + LONG + INT:
    case SIGNED + LONG + LONG:
    case SIGNED + LONG + LONG + INT:
      ty = ty_llong;
      break;
    case UNSIGNED + LONG:
    case UNSIGNED + LONG + INT:
      ty = ty_ulong;
      break;
    case UNSIGNED + LONG + LONG:
    case UNSIGNED + LONG + LONG + INT:
      ty = ty_ullong;
      break;
    case FLOAT:
      ty = ty_float;
      break;
    case DOUBLE:
      ty = ty_double;
      break;
    case LONG + DOUBLE:
      ty = ty_ldouble;
      break;
    case BITINT:
    case BITINT + SIGNED:
      break;
    case BITINT + UNSIGNED:
      ty->is_unsigned = true;
      break;
    default:
      error_tok(tok, "invalid type");
    }

    tok = tok->next;
  }

  *rest = tok;

  if (!ty && is_auto) {
    if (tok->kind != TK_IDENT || !equal(tok->next, "="))
      error_tok(tok, "unsupported form for type inference");
    enter_scope();
    Node *node = assign(&(Token *){0}, tok->next->next);
    add_type(node);
    leave_scope();
    ty = unqual(ptr_decay(node->ty));
  }

  if (!ty)
    ty = ty_int;

  if (ty->kind == TY_BITINT)
    if (ty->bit_cnt < (1 + !ty->is_unsigned))
      error_tok(tok, "invalid bit width for _BitInt");

  if (is_atomic || is_const || is_volatile || is_restrict) {
    Type *ty2 = new_qualified_type(ty);
    ty2->is_atomic = is_atomic;
    ty2->is_const = is_const;
    ty2->is_volatile = is_volatile;
    ty2->is_restrict = is_restrict;
    return ty2;
  }

  return ty;
}

static Type *func_params_old_style(Token **rest, Token *tok, Type *fn_ty) {
  Token *start = tok;
  tok = skip_paren(tok);

  enter_scope();
  fn_ty->scopes = scope;
  Node *expr = NULL;

  while (is_typename(tok)) {
    Type *basety = declspec(&tok, tok, NULL);
    do {
      Token *name = NULL;
      Type *ty = declarator(&tok, tok, basety, &name);
      if (!name)
        error_tok(tok, "expected identifier");

      Obj *promoted = NULL;
      if (is_integer(ty) && ty->size < ty_int->size)
        promoted = new_lvar(NULL, ty_int);
      else if (ty->kind == TY_FLOAT)
        promoted = new_lvar(NULL, ty_double);
      else
        ty = ptr_decay(ty);

      Obj *var = new_lvar(get_ident(name), ty);
      if (promoted) {
        var->param_promoted = promoted;
        chain_expr(&expr, new_binary(ND_ASSIGN, new_var_node(var, tok),
                                     new_var_node(promoted, tok), tok));
      }
      chain_expr(&expr, calc_vla(ty, tok));
    } while (comma_list(&tok, &tok, ";", true));
  }
  *rest = tok;

  Obj head = {0};
  Obj *cur = &head;

  for (tok = start; comma_list(&tok, &tok, ")", cur != &head);) {
    VarScope *sc = hashmap_get2(&fn_ty->scopes->vars, tok->loc, tok->len);

    Obj *nxt;
    if (!sc)
      nxt = new_lvar(get_ident(tok), ty_int);
    else if (sc->var->param_promoted)
      nxt = sc->var->param_promoted;
    else
      nxt = sc->var;

    cur = cur->param_next = nxt;
    tok = tok->next;
  }
  leave_scope();
  add_type(expr);
  fn_ty->param_list = head.param_next;
  fn_ty->is_oldstyle = true;
  fn_ty->pre_calc = expr;
  return fn_ty;
}

// func-params = ("void" | param ("," param)* ("," "...")?)? ")"
// param       = declspec declarator
static Type *func_params(Token **rest, Token *tok, Type *ty) {
  Type *fn_ty = func_type(ty, tok);

  if (equal(tok, "...") && consume(rest, tok->next, ")")) {
    fn_ty->is_variadic = true;
    return fn_ty;
  }
  if (equal(tok, "void") && consume(rest, tok->next, ")"))
    return fn_ty;

  if (consume(rest, tok, ")")) {
    if (opt_std < STD_C23)
      fn_ty->is_oldstyle = true;
    return fn_ty;
  }
  if (!is_typename(tok))
    return func_params_old_style(rest, tok, fn_ty);

  Obj head = {0};
  Obj *cur = &head;
  Node *expr = NULL;

  enter_scope();
  fn_ty->scopes = scope;

  while (comma_list(rest, &tok, ")", cur != &head)) {
    if (equal(tok, "...")) {
      fn_ty->is_variadic = true;
      *rest = skip(tok->next, ")");
      break;
    }

    Type *ty2 = declspec(&tok, tok, NULL);
    Token *name = NULL;
    ty2 = declarator(&tok, tok, ty2, &name);

    chain_expr(&expr, calc_vla(ty2, tok));

    if (is_array(ty2)) {
      // "array of T" is converted to "pointer to T" only in the parameter
      // context. For example, *argv[] is converted to **argv by this.
      Type *ty3 = pointer_to(ty2->base);
      ty3->is_atomic = ty2->is_atomic;
      ty3->is_const = ty2->is_const;
      ty3->is_volatile = ty2->is_volatile;
      ty3->is_restrict = ty2->is_restrict;
      ty2 = ty3;
    } else if (ty2->kind == TY_FUNC) {
      // Likewise, a function is converted to a pointer to a function
      // only in the parameter context.
      ty2 = pointer_to(ty2);
    }
    char *var_name = name ? get_ident(name) : NULL;
    cur = cur->param_next = new_lvar(var_name, ty2);
  }
  leave_scope();
  add_type(expr);
  fn_ty->param_list = head.param_next;
  fn_ty->pre_calc = expr;
  return fn_ty;
}

// array-dimensions = ("static" | "restrict")* const-expr? "]" type-suffix
static Type *array_dimensions(Token **rest, Token *tok, Type *ty) {
  if (consume(&tok, tok, "]") ||
      (equal(tok, "*") && consume(&tok, tok->next, "]"))) {
    if (equal(tok, "["))
      ty = array_dimensions(&tok, tok->next, ty);
    *rest = tok;
    return array_of(ty, -1);
  }

  Node *expr = assign(&tok, tok);
  add_type(expr);
  if (!is_integer(expr->ty))
    error_tok(tok, "size of array not integer");
  tok = skip(tok, "]");

  if (equal(tok, "["))
    ty = array_dimensions(&tok, tok->next, ty);
  *rest = tok;

  int64_t array_len;
  if (ty->kind != TY_VLA && is_const_expr(expr, &array_len))
    return array_of(ty, array_len);

  if (is_constant_context())
    error_tok(tok, "variably-modified type in constant context");
  return vla_of(ty, expr);
}

static void pointer_qualifiers(Token **rest, Token *tok, Type *ty) {
  for (;; tok = tok->next) {
    if (equal(tok, "_Atomic"))
      ty->is_atomic = true;
    else if (equal(tok, "const"))
      ty->is_const = true;
    else if (equal(tok, "volatile") || equal(tok, "__volatile") || equal(tok, "__volatile__"))
      ty->is_volatile = true;
    else if (equal(tok, "restrict") || equal(tok, "__restrict") || equal(tok, "__restrict__"))
      ty->is_restrict = true;
    else
      break;
  }
  *rest = tok;
}

// type-suffix = "(" func-params
//             | "[" array-dimensions
//             | ε
static Type *type_suffix(Token **rest, Token *tok, Type *ty) {
  if (equal(tok, "("))
    return func_params(rest, tok->next, ty);

  if (consume(&tok, tok, "[")) {
    if (tok->kind == TK_TYPEKW) {
      Token *start = tok;
      pointer_qualifiers(&tok, tok, &(Type){0});
      consume(&tok, tok, "static");
      Type *ty2 = array_dimensions(rest, tok, ty);
      pointer_qualifiers(&(Token *){0}, start, ty2);
      return ty2;
    }
    return array_dimensions(rest, tok, ty);
  }
  *rest = tok;
  return ty;
}

// pointers = ("*" ("const" | "volatile" | "restrict")*)*
static Type *pointers(Token **rest, Token *tok, Type *ty) {
  while (consume(&tok, tok, "*")) {
    ty = pointer_to(ty);
    pointer_qualifiers(&tok, tok, ty);
  }
  *rest = tok;
  return ty;
}

Token *skip_paren(Token *tok) {
  int level = 0;
  Token *start = tok;
  for (;;) {
    if (level == 0 && equal(tok, ")"))
      break;

    if (tok->kind == TK_EOF)
      error_tok(start, "unterminated list");

    if (equal(tok, "("))
      level++;
    else if (equal(tok, ")"))
      level--;

    tok = tok->next;
  }
  return tok->next;
}

static Type *declarator(Token **rest, Token *tok, Type *ty, Token **name_tok) {
  ty = pointers(&tok, tok, ty);

  if (consume(&tok, tok, "(")) {
    if (is_typename(tok) || equal(tok, ")"))
      return func_params(rest, tok, ty);

    ty = type_suffix(rest, skip_paren(tok), ty);
    return declarator(&(Token *){NULL}, tok, ty, name_tok);
  }

  if (name_tok && tok->kind == TK_IDENT) {
    *name_tok = tok;
    tok = tok->next;
  }
  return type_suffix(rest, tok, ty);
}

static Type *declarator2(Token **rest, Token *tok, Type *basety, Token **name, int *align) {
  Type *ty = declarator(&tok, tok, basety, name);

  if (*name) {
    attr_aligned(*name, align, TK_ATTR);
    attr_aligned((**name).next, align, TK_BATTR);
  }
  attr_aligned(tok, align, TK_ATTR);
  *rest = tok;
  return ty;
}

// type-name = declspec abstract-declarator
static Type *typename(Token **rest, Token *tok) {
  Type *ty = declspec(&tok, tok, NULL);
  return declarator(rest, tok, ty, NULL);
}

static void update_enum_ty(Type *decl_ty, Type *ty, bool is_unspec) {
  for (Type *ty2 = decl_ty; ty2; ty2 = ty2->decl_next) {
    ty2->kind = ty->kind;
    ty2->is_unsigned = ty->is_unsigned;
    ty2->size = ty->size;
    ty2->align = MAX(ty2->align, ty->align);
    ty2->origin = ty;
    ty2->is_unspec_enum = is_unspec;
  }
}

static Type *enum_specifier(Token **rest, Token *tok) {
  // Read a tag.
  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  Type *ty = NULL;
  if (consume(&tok, tok, ":"))
    ty = unqual(typename(&tok, tok));

  if (tag && !equal(tok, "{")) {
    *rest = tok;
    Type *ty2 = find_tag(tag);
    if (ty2) {
      if (ty2->kind == TY_STRUCT || ty2->kind == TY_UNION)
        error_tok(tag, "not an enum tag");
      return ty2;
    }
    if (!ty)
      ty = new_type(TY_ENUM, -1, 1);
    push_tag_scope(tag, ty);
    return ty;
  }
  tok = skip(tok, "{");

  if (tag) {
    Type *ty2 = hashmap_get2(&scope->tags, tag->loc, tag->len);
    if (ty2) {
      if (ty2->kind == TY_STRUCT || ty2->kind == TY_UNION)
        error_tok(tag, "not an enum tag");
      if ((!ty && ty2->kind != TY_ENUM) ||
        (ty && (ty->kind != ty2->kind || ty->is_unsigned != ty2->is_unsigned)))
        error_tok(tag, "enum redeclared with incompatible type");
      ty = ty2;
    }
  }
  if (!ty)
    ty = new_type(TY_ENUM, -1, 1);

  bool has_type = (ty->kind != TY_ENUM);
  if (!has_type)
    update_enum_ty(ty, ty_uint, true);

  bool need_u32 = false;
  bool need_u64 = false;
  bool need_i64 = false;
  bool been_neg = false;

  uint64_t val = 0;
  bool is_neg = false;
  bool is_ovf = false;
  bool first = true;
  for (; comma_list(rest, &tok, "}", !first); first = false) {
    char *name = get_ident(tok);
    tok = tok->next;

    if (consume(&tok, tok, "=")) {
      Type *val_ty = NULL;
      val = const_expr2(&tok, tok, &val_ty);

      if (!val_ty->is_unsigned && (int64_t)val < 0) {
        need_i64 = (int64_t)val < INT32_MIN;
        is_neg = been_neg = true;
      }
    } else if (is_ovf) {
      error_tok(tok, "enum value overflowed");
    }

    if (!is_neg && (val > INT32_MAX)) {
      need_u64 = val > UINT32_MAX;
      need_u32 = true;
    }
    VarScope *sc = push_scope(name);
    sc->enum_ty = ty;
    sc->enum_val = val++;
    is_ovf = !is_neg && val == 0;
    is_neg = (int64_t)val < 0;
  }

  if (first)
    error_tok(tok, "empty enum specifier");

  if (has_type) {
    if ((ty->is_unsigned && (been_neg || (ty->size < 8 && need_u64))) ||
      (!ty->is_unsigned && (need_u64 || (ty->size < 8 && (need_u32 || need_i64)))))
      error_tok(tok, "enum value out of type range");
  } else {
    Type *enum_ty;
    bool is_unspec = false;
    if (been_neg)
      enum_ty = (need_u64 || need_u32 || need_i64) ? get_first_64bit_int(false) : ty_int;
    else if (need_u64)
      enum_ty = get_first_64bit_int(true);
    else if (need_u32)
      enum_ty = ty_uint;
    else
      enum_ty = ty_uint, is_unspec = true;

    update_enum_ty(ty, enum_ty, is_unspec);
  }
  if (tag)
    push_tag_scope(tag, ty);
  return ty;
}

// typeof-specifier = "(" (expr | typename) ")"
static Type *typeof_specifier(Token **rest, Token *tok) {
  tok = skip(tok, "(");

  Type *ty;
  if (is_typename(tok)) {
    ty = typename(&tok, tok);
  } else {
    Node *node = expr(&tok, tok);
    add_type(node);
    ty = node->ty;
  }
  *rest = skip(tok, ")");
  return ty;
}

static Node *vla_count(Type *ty, Token *tok, bool is_void) {
  int64_t val;
  if (is_const_expr(ty->vla_len, &val))
    return is_void ? NULL : new_size_t(val, tok);

  if (ty->vla_cnt)
    return is_void ? NULL : new_var_node(ty->vla_cnt, tok);

  ty->vla_cnt = new_lvar(NULL, ty_size_t);
  return new_binary(ND_ASSIGN, new_var_node(ty->vla_cnt, tok), ty->vla_len, tok);
}

static Node *vla_size(Type *ty, Token *tok) {
  Node *base_sz;
  if (ty->base->kind == TY_VLA)
    base_sz = vla_size(ty->base, tok);
  else
    base_sz = new_num(ty->base->size, tok);

  return new_binary(ND_MUL, vla_count(ty, tok, false), base_sz, tok);
}

static Node *calc_vla(Type *ty, Token *tok) {
  Node *n = NULL;
  if (ty->kind == TY_VLA)
    n = vla_count(ty, tok, true);

  if (ty->base)
    chain_expr(&n, calc_vla(ty->base, tok));
  return n;
}

static Node *new_vla(Node *sz, Obj *var) {
  Node *node = new_unary(ND_ALLOCA, sz, sz->tok);
  node->ty = pointer_to(ty_void);
  node->var = var;
  add_type(sz);
  return node;
}

static void defr_cleanup(Obj *var, Obj *fn, Token *tok) {
  Node *n = new_unary(ND_FUNCALL, new_var_node(fn, tok), tok);
  n->lhs->ty = fn->ty;
  n->ty = ty_void;

  Node *arg = new_unary(ND_ADDR, new_var_node(var, tok), tok);
  add_type(arg);

  n->args = new_var(NULL, arg->ty, ast_arena_calloc(sizeof(Obj)));
  n->args->arg_expr = arg;
  prepare_funcall(n, scope);

  DeferStmt *defr2 = new_defr(DF_CLEANUP_FN);
  defr2->cleanup_fn = n;
}

static Node *declaration2(Token **rest, Token *tok, Type *basety, VarAttr *attr, Obj **cond_var) {
  Node *expr = NULL;
  Token *name = NULL;
  int alt_align = attr ? attr->align : 0;

  Type *ty = declarator2(&tok, tok, basety, &name, &alt_align);

  if (ty->kind == TY_FUNC) {
    if (!name)
      error_tok(tok, "function name omitted");
    Obj *fn = func_prototype(ty, attr, name);
    func_attr(fn, attr, name, tok);
    symbol_attr(&tok, tok, fn, attr, name);
    *rest = tok;
    return expr;
  }
  if (ty->kind == TY_VOID)
    error_tok(tok, "variable declared void");
  if (!name)
    error_tok(tok, "variable name omitted");

  Obj *cleanup_fn = attr ? attr->cleanup_fn : NULL;
  DeclAttr(attr_cleanup, &cleanup_fn);

  if (attr && attr->is_static) {
    if (ty->kind == TY_VLA)
      error_tok(tok, "variable length arrays cannot be 'static'");

    // static local variable
    Obj *var = new_static_lvar(ty);
    var->is_tls = attr->is_tls;
    if (alt_align)
      var->align = alt_align;
    push_scope(get_ident(name))->var = var;

    if (attr->is_constexpr) {
      constexpr_initializer(&tok, skip(tok, "="), var, var);
      *rest = tok;
      return expr;
    }
    if (equal(tok, "=")) {
      bool ctx = is_global_init_context;
      is_global_init_context = true;
      gvar_initializer(&tok, tok->next, var);
      is_global_init_context = ctx;
    }
    *rest = tok;
    return expr;
  }

  if (ty->kind == TY_VLA) {
    fn_use_vla = true;

    Node *node = new_vla(vla_size(ty, name), new_lvar(get_ident(name), ty));

    DeferStmt *defr = new_defr(DF_VLA_DEALLOC);
    defr->vla = node->var;

    if (cleanup_fn)
      defr_cleanup(node->var, cleanup_fn, tok);
    if (alt_align)
      node->var->align = alt_align;

    if (equal(tok, "=")) {
      tok = skip(skip(tok->next, "{"), "}");
      node->kind = ND_ALLOCA_ZINIT;
    }
    *rest = tok;
    chain_expr(&expr, node);
    return expr;
  }

  Obj *var = new_lvar(get_ident(name), ty);
  if (alt_align)
    var->align = alt_align;

  if (cleanup_fn)
    defr_cleanup(var, cleanup_fn, tok);

  if (attr->is_register &&
    (equal_kw(tok, "asm") || equal(tok, "__asm") || equal(tok, "__asm__"))) {
    var->asm_str = str_tok(&tok, skip(tok->next, "("));
    tok = skip(tok, ")");
  }

  if (attr && attr->is_constexpr) {
    Obj *init_var = new_static_lvar(ty);
    constexpr_initializer(&tok, skip(tok, "="), init_var, var);
    chain_expr(&expr, new_binary(ND_ASSIGN,
      new_var_node(var, tok), new_var_node(init_var, tok), tok));
    *cond_var = var;
    *rest = tok;
    return expr;
  }
  if (equal(tok, "=")) {
    chain_expr(&expr, lvar_initializer(&tok, tok->next, var));
    *cond_var = var;
  }
  if (var->ty->size < 0)
    error_tok(name, "variable has incomplete type");
  if (var->ty->kind == TY_VOID)
    error_tok(name, "variable declared void");

  chain_expr(&expr, calc_vla(ty, tok));
  *rest = tok;
  return expr;
}

static Node *cond_declaration(Token **rest, Token *tok, char *stopper, int clause) {
  Node *n = NULL;
  Obj *var = NULL;

  for (; is_typename(tok); var = NULL) {
    clause++;

    VarAttr attr = {0};
    Type *basety = declspec(&tok, tok, &attr);

    chain_expr(&n, declaration2(&tok, tok, basety, &attr, &var));
    for (; consume(&tok, tok, ","); var = NULL)
      chain_expr(&n, declaration2(&tok, tok, basety, &attr, &(Obj *){0}));

    if (!(clause == 1 && consume(&tok, tok, ";")))
      break;
  }

  if (clause < 2 && !equal(tok, stopper)) {
    chain_expr(&n, expr(&tok, tok));
  } else {
    if (!var)
      error_tok(tok, "invalid condition");
    chain_expr(&n, new_var_node(var, tok));
  }
  *rest = skip(tok, stopper);
  return n;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr) {
  Node *expr = NULL;

  bool first = true;
  for (; comma_list(rest, &tok, ";", !first); first = false)
    chain_expr(&expr, declaration2(&tok, tok, basety, attr, &(Obj *){0}));

  return expr;
}

static Token *skip_excess_element(Token *tok) {
  if (equal(tok, "{")) {
    tok = skip_excess_element(tok->next);
    return skip(tok, "}");
  }

  assign(&tok, tok);
  return tok;
}

// string-initializer = string-literal
static void string_initializer(Token *tok, Initializer *init) {
  if (init->kind == INIT_FLEXIBLE)
    new_initializer(init, array_of(init->ty->base, tok->ty->array_len), false);

  int len = MIN(init->ty->array_len, tok->ty->array_len);

  switch (init->ty->base->size) {
  case 1: {
    char *str = tok->str;
    for (int i = 0; i < len; i++) {
      init->mem_arr[i].kind = INIT_EXPR;
      init->mem_arr[i].expr = new_num(str[i], tok);
    }
    break;
  }
  case 2: {
    uint16_t *str = (uint16_t *)tok->str;
    for (int i = 0; i < len; i++) {
      init->mem_arr[i].kind = INIT_EXPR;
      init->mem_arr[i].expr = new_num(str[i], tok);
    }
    break;
  }
  case 4: {
    uint32_t *str = (uint32_t *)tok->str;
    for (int i = 0; i < len; i++) {
      init->mem_arr[i].kind = INIT_EXPR;
      init->mem_arr[i].expr = new_num(str[i], tok);
    }
    break;
  }
  default:
    internal_error();
  }
}

static bool is_str_tok(Token **rest, Token *tok, Token **str_tok) {
  if (equal(tok, "(") && is_str_tok(&tok, tok->next, str_tok) &&
    consume(rest, tok, ")"))
    return true;

  if (tok->kind == TK_STR) {
    *str_tok = tok;
    *rest = tok->next;
    return true;
  }
  return false;
}

// array-designator = "[" const-expr "]"
//
// C99 added the designated initializer to the language, which allows
// programmers to move the "cursor" of an initializer to any element.
// The syntax looks like this:
//
//   int x[10] = { 1, 2, [5]=3, 4, 5, 6, 7 };
//
// `[5]` moves the cursor to the 5th element, so the 5th element of x
// is set to 3. Initialization then continues forward in order, so
// 6th, 7th, 8th and 9th elements are initialized with 4, 5, 6 and 7,
// respectively. Unspecified elements (in this case, 3rd and 4th
// elements) are initialized with zero.
//
// Nesting is allowed, so the following initializer is valid:
//
//   int x[5][10] = { [5][8]=1, 2, 3 };
//
// It sets x[5][8], x[5][9] and x[6][0] to 1, 2 and 3, respectively.
//
// Use `.fieldname` to move the cursor for a struct initializer. E.g.
//
//   struct { int a, b, c; } x = { .c=5 };
//
// The above initializer sets x.c to 5.
static void array_designator(Token **rest, Token *tok, Type *ty, int *begin, int *end) {
  *begin = const_expr(&tok, tok->next);
  if (*begin >= ty->array_len)
    error_tok(tok, "array designator index exceeds array bounds");

  if (equal(tok, "...")) {
    *end = const_expr(&tok, tok->next);
    if (*end >= ty->array_len)
      error_tok(tok, "array designator index exceeds array bounds");
    if (*end < *begin)
      error_tok(tok, "array designator range [%d, %d] is empty", *begin, *end);
  } else {
    *end = *begin;
  }

  *rest = skip(tok, "]");
}

// struct-designator = "." ident
static Member *struct_designator(Token **rest, Token *tok, Type *ty) {
  if (tok->kind != TK_IDENT)
    error_tok(tok, "expected a field designator");

  Member *mem = get_struct_member(ty, tok);
  if (!mem)
    error_tok(tok, "struct has no such member");
  if (mem->name)
    *rest = tok->next;
  return mem;
}

static void designation(Token **rest, Token *tok, Initializer *init) {
  if (equal(tok, "[")) {
    if (init->ty->kind != TY_ARRAY)
      error_tok(tok, "array index in non-array initializer");
    init->kind = INIT_LIST;

    int begin, end;
    array_designator(&tok, tok, init->ty, &begin, &end);

    Token *start = tok;
    for (int i = begin; i <= end; i++)
      designation(&tok, start, &init->mem_arr[i]);

    list_initializer(rest, tok, init, end + 1);
    return;
  }

  if (equal(tok, ".")) {
    Member *mem = struct_designator(&tok, tok->next, init->ty);
    init->kind = INIT_LIST;

    if (init->ty->kind == TY_UNION) {
      init->initmem = mem;
      designation(rest, tok, &init->mem_arr[mem->idx]);
    } else {
      designation(&tok, tok, &init->mem_arr[mem->idx]);
      list_initializer(rest, tok, init, mem->idx + 1);
    }
    return;
  }

  initializer2(rest, skip(tok, "="), init);
}

// An array length can be omitted if an array has an initializer
// (e.g. `int x[] = {1,2,3}`). If it's omitted, count the number
// of initializer elements.
static int count_array_init_elements(Token *tok, Type *ty) {
  Initializer dummy = {0};
  new_initializer(&dummy, ty->base, true);

  int i = 0, max = 0;
  while (comma_list(&tok, &tok, "}", i)) {
    if (equal(tok, "[")) {
      i = const_expr(&tok, tok->next);
      if (equal(tok, "..."))
        i = const_expr(&tok, tok->next);
      tok = skip(tok, "]");
      designation(&tok, tok, &dummy);
    } else {
      initializer2(&tok, tok, &dummy);
    }

    i++;
    max = MAX(max, i);
  }
  free_initializers(&dummy);
  return max;
}

static void braced_initializer(Token **rest, Token *tok, Initializer *init) {
  Token *start = tok;
  list_initializer(&tok, tok, init, 0);

  for (; comma_list(rest, &tok, "}", tok != start);) {
    if (equal(tok, ".") || equal(tok, "[")) {
      designation(&tok, tok, init);
      continue;
    }
    tok = skip_excess_element(tok);
  }
}

static void list_initializer(Token **rest, Token *tok, Initializer *init, int idx) {
  for (; idx < init->mem_cnt && !equal(tok, "}"); idx++) {
    Token *tok2 = (idx == 0) ? tok : skip(tok, ",");

    if (equal(tok2, "}") || equal(tok2, "[") || equal(tok2, "."))
      break;

    initializer2(&tok, tok2, &init->mem_arr[idx]);
  }
  *rest = tok;
}

// initializer = string-initializer | array-initializer
//             | struct-initializer | union-initializer
//             | assign
static void initializer2(Token **rest, Token *tok, Initializer *init) {
  if (init->ty->kind == TY_ARRAY && is_integer(init->ty->base)) {
    Token *start = tok;
    Token *str_tok;
    if (equal(tok, "{") && is_str_tok(&tok, tok->next, &str_tok)) {
      if (consume(rest, tok, "}")) {
        string_initializer(str_tok, init);
        return;
      }
      tok = start;
    }
    if (is_str_tok(rest, tok, &str_tok)) {
      string_initializer(str_tok, init);
      return;
    }
  }

  if (init->ty->kind == TY_ARRAY) {
    bool has_brace = consume(&tok, tok, "{");
    if (init->kind == INIT_FLEXIBLE) {
      int len = count_array_init_elements(tok, init->ty);
      new_initializer(init, array_of(init->ty->base, len), false);
    }

    if (has_brace)
      braced_initializer(rest, tok, init);
    else
      list_initializer(rest, tok, init, 0);
    return;
  }

  if (init->ty->kind == TY_STRUCT) {
    if (consume(&tok, tok, "{")) {
      braced_initializer(rest, tok, init);
      return;
    }

    Node *expr = assign(rest, tok);
    add_type(expr);
    if (is_compatible(expr->ty, init->ty)) {
      init->kind = INIT_EXPR;
      init->expr = expr;
      return;
    }

    list_initializer(rest, tok, init, 0);
    return;
  }

  if (init->ty->kind == TY_UNION) {
    if (consume(&tok, tok, "{")) {
      init->initmem = init->ty->members;
      mem_iter(&init->initmem);
      braced_initializer(rest, tok, init);
      return;
    }

    Node *expr = assign(rest, tok);
    add_type(expr);
    if (is_compatible(expr->ty, init->ty)) {
      init->kind = INIT_EXPR;
      init->expr = expr;
      return;
    }

    init->initmem = init->ty->members;
    mem_iter(&init->initmem);
    initializer2(rest, tok, &init->mem_arr[0]);
    return;
  }

  if (equal(tok, "{")) {
    if (consume(rest, tok->next, "}"))
      return;
    initializer2(&tok, tok->next, init);
    *rest = skip(tok, "}");
    return;
  }

  if ((tok->kind == TK_INT_NUM || tok->kind == TK_PP_NUM) && equal(tok->next, ",")) {
    init->kind = INIT_TOK;
    init->tok = tok;
    *rest = tok->next;
    return;
  }

  init->kind = INIT_EXPR;
  init->expr = assign(rest, tok);
}

static Type *initializer(Token **rest, Initializer *init, Token *tok, Type *ty) {
  new_initializer(init, ty, true);
  initializer2(rest, tok, init);

  if (ty->kind == TY_STRUCT && ty->is_flexible) {
    ty = copy_type(ty);

    Member head = {0};
    Member *cur = &head;
    for (Member *mem = ty->members; mem; mem = mem->next) {
      cur = cur->next = ast_arena_malloc(sizeof(Member));
      *cur = *mem;
    }
    cur->ty = init->mem_arr[cur->idx].ty;
    ty->size += cur->ty->size;
    ty->members = head.next;

    return ty;
  }
  return init->ty;
}

static Node *init_desg_expr(InitDesg *desg, Token *tok) {
  if (desg->var)
    return new_var_node(desg->var, tok);

  if (desg->member) {
    Node *node = new_unary(ND_MEMBER, init_desg_expr(desg->next, tok), tok);
    node->member = desg->member;
    return node;
  }

  Node *lhs = init_desg_expr(desg->next, tok);
  Node *rhs = new_num(desg->idx, tok);
  return new_unary(ND_DEREF, new_add(lhs, rhs, tok), tok);
}

static Node *init_num_tok(Initializer *init, Node *node) {
  if (init->kind == INIT_TOK) {
    convert_pp_number(init->tok, node);
    return node;
  }
  add_type(init->expr);
  return init->expr;
}

static Node *create_lvar_init(Node *expr, Initializer *init, Type *ty, InitDesg *desg, Token *tok) {
  if (init->kind == INIT_NONE || init->kind == INIT_FLEXIBLE)
    return expr;

  if (init->kind == INIT_LIST) {
    if (ty->kind == TY_ARRAY) {
      for (int i = 0; i < ty->array_len; i++) {
        InitDesg desg2 = {desg, i};
        expr = create_lvar_init(expr, &init->mem_arr[i], ty->base, &desg2, tok);
      }
      return expr;
    }
    if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
      for (Member *mem = ty->members; mem_iter(&mem); mem = mem->next) {
        if (ty->kind == TY_UNION && init->initmem != mem)
          continue;

        InitDesg desg2 = {desg, 0, mem};
        expr = create_lvar_init(expr, &init->mem_arr[mem->idx], mem->ty, &desg2, tok);
      }
      return expr;
    }
  }

  if (init->kind == INIT_EXPR || init->kind == INIT_TOK) {
    if (init->kind == INIT_TOK) {
      init->expr = init_num_tok(init, new_node(ND_NUM, init->tok));
      init->kind = INIT_EXPR;
    }
    if (ty->kind == TY_ARRAY)
      error_tok(init->expr->tok, "array initializer must be an initializer list");

    Node *n = new_binary(ND_ASSIGN, init_desg_expr(desg, tok), init->expr, tok);
    add_type(n);
    return expr->next = n;
  }

  internal_error();
}

// A variable definition with an initializer is a shorthand notation
// for a variable definition followed by assignments. This function
// generates assignment expressions for an initializer. For example,
// `int x[2][2] = {{6, 7}, {8, 9}}` is converted to the following
// expressions:
//
//   x[0][0] = 6;
//   x[0][1] = 7;
//   x[1][0] = 8;
//   x[1][1] = 9;
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var) {
  Initializer init = {0};
  var->ty = initializer(rest, &init, tok, var->ty);
  InitDesg desg = {NULL, 0, NULL, var};

  Node head = {0};
  create_lvar_init(&head, &init, var->ty, &desg, tok);
  free_initializers(&init);

  if (opt_optimize && init.kind == INIT_EXPR)
    return head.next;

  // If a partial initializer list is given, the standard requires
  // that unspecified elements are set to 0. Here, we simply
  // zero-initialize the entire memory region of a variable before
  // initializing it with user-supplied values.
  Node *node = new_node(ND_INIT_SEQ, tok);
  node->var = var;
  node->lhs = head.next;
  node->ty = ty_void;
  return node;
}

static uint64_t read_buf(char *buf, int sz) {
  if (sz == 1)
    return BUFF_CAST(uint8_t, buf);
  if (sz == 2)
    return BUFF_CAST(uint16_t, buf);
  if (sz == 4)
    return BUFF_CAST(uint32_t, buf);
  if (sz == 8)
    return BUFF_CAST(uint64_t, buf);
  internal_error();
}

static long double read_double_buf(char *buf, Type *ty) {
  if (ty->kind == TY_FLOAT)
    return BUFF_CAST(float, buf);
  if (ty->kind == TY_DOUBLE)
    return BUFF_CAST(double, buf);
  if (ty->kind == TY_LDOUBLE)
    return BUFF_CAST(long double, buf);
  internal_error();
}

static Relocation *
write_gvar_data(Relocation *cur, Initializer *init, Type *ty, char *buf, int offset, EvalKind kind) {
  if (init->kind == INIT_NONE || init->kind == INIT_FLEXIBLE)
    return cur;

  if (init->kind == INIT_LIST) {
    if (ty->kind == TY_ARRAY) {
      int sz = ty->base->size;
      for (int i = 0; i < ty->array_len; i++)
        cur = write_gvar_data(cur, &init->mem_arr[i], ty->base, buf, offset + sz * i, kind);
      return cur;
    }

    if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
      for (Member *mem = ty->members; mem_iter(&mem); mem = mem->next) {
        if (ty->kind == TY_UNION && init->initmem != mem)
          continue;

        Initializer *init2 = &init->mem_arr[mem->idx];
        if (mem->is_bitfield &&
          ((init2->kind == INIT_EXPR || init2->kind == INIT_TOK))) {
          Node *node = init_num_tok(init2, &(Node){.kind = ND_NUM, .tok = init2->tok});
          node = assign_cast(mem->ty, node);

          char *loc = buf + offset + mem->offset;

          if (mem->ty->kind == TY_BITINT) {
            uint64_t *val = eval_bitint(node);
            eval_bitint_bitfield_save(mem->ty->bit_cnt, val, loc, mem->bit_width, mem->bit_offset);
            free(val);
            continue;
          }
          uint64_t oldval = read_buf(loc, mem->ty->size);
          uint64_t newval = eval(node);
          uint64_t mask = (1L << mem->bit_width) - 1;
          uint64_t combined = oldval | ((newval & mask) << mem->bit_offset);
          memcpy(loc, &combined, mem->ty->size);
          continue;
        }
        cur = write_gvar_data(cur, init2, mem->ty, buf, offset + mem->offset, kind);
      }
      return cur;
    }
  }

  if (init->kind == INIT_EXPR || init->kind == INIT_TOK) {
    Node *node = init_num_tok(init, &(Node){.kind = ND_NUM, .tok = init->tok});
    node = assign_cast(ty, node);

    if (ty->kind == TY_ARRAY)
      error_tok(node->tok, "array initializer must be an initializer list");

    // Direct initialization with a const variable
    if (init->kind == INIT_EXPR) {
      int sofs;
      Obj *var = NULL;
      if (is_compatible(ty, init->expr->ty))
        var = eval_var(init->expr, &sofs, false);

      if (var && var->init_data && !var->is_weak &&
        (is_const_var(var) || var->is_compound_lit)) {
        Relocation *srel= var->rel;
        while (srel && srel->offset < sofs)
          srel = srel->next;

        for (int pos = 0; pos < ty->size && (pos + sofs) < var->ty->size;) {
          if (srel && srel->offset == (pos + sofs)) {
            cur = cur->next = ast_arena_calloc(sizeof(Relocation));
            cur->offset = (pos + offset);
            cur->label = srel->label;
            cur->addend = srel->addend;

            srel = srel->next;
            pos += 8;
          } else {
            buf[(pos + offset)] = var->init_data[(pos + sofs)];
            pos++;
          }
        }
        return cur;
      }
    }

    // Pointer or equivalent sized integer may be relocation
    if (ty->kind == TY_PTR || ty->kind == TY_NULLPTR ||
      (is_integer(ty) && ty->size == ty_intptr_t->size)) {
      int64_t val;
      if (is_const_expr(node, &val)) {
        memcpy(buf + offset, &val, ty->size);
        return cur;
      }
      if (kind == EV_LABEL) {
        EvalContext ctx = {.kind = EV_LABEL};
        int64_t addend = eval2(node, &ctx);
        if (ctx.label) {
          Relocation *rel = ast_arena_calloc(sizeof(Relocation));
          rel->offset = offset;
          rel->label = ctx.label;
          rel->addend = addend;
          cur->next = rel;
          return cur->next;
        }
      }
      error_tok(node->tok, "invalid initializer");
    }

    switch (ty->kind) {
    case TY_FLOAT:
      BUFF_CAST(float, (buf + offset)) = eval_double(node);
      return cur;
    case TY_DOUBLE:
      BUFF_CAST(double, (buf + offset)) = eval_double(node);
      return cur;
    case TY_LDOUBLE:
      BUFF_CAST(long double, (buf + offset)) = eval_double(node);
      return cur;
    case TY_BITINT: {
      uint64_t *data = eval_bitint(node);
      if (ty->bit_cnt < ty->size * 8)
        eval_bitint_sign_ext(ty->bit_cnt, data, ty->size * 8, ty->is_unsigned);
      memcpy(buf + offset, data, ty->size);
      free(data);
      return cur;
    }
    }
    if (is_integer(ty)) {
      memcpy(buf + offset, &(int64_t){eval(node)}, ty->size);
      return cur;
    }
    error_tok(node->tok, "unknown initializer");
  }
  internal_error();
}

// Initializers for global variables are evaluated at compile-time and
// embedded to .data section. This function serializes Initializer
// objects to a flat byte array. It is a compile error if an
// initializer list contains a non-constant expression.
static void gvar_initializer(Token **rest, Token *tok, Obj *var) {
  Initializer init = {0};
  var->ty = initializer(rest, &init, tok, var->ty);

  if (var->ty->size < 0)
    error_tok(tok, "variable has incomplete type");

  Relocation head = {0};
  char *buf = calloc(1, var->ty->size);
  write_gvar_data(&head, &init, var->ty, buf, 0, EV_LABEL);
  free_initializers(&init);

  var->init_data = buf;
  var->rel = head.next;
}

static void constexpr_initializer(Token **rest, Token *tok, Obj *init_var, Obj *var) {
  Initializer init = {0};
  init_var->ty = initializer(rest, &init, tok, init_var->ty);

  Relocation head = {0};
  char *buf = calloc(1, init_var->ty->size);
  write_gvar_data(&head, &init, init_var->ty, buf, 0, EV_CONST);
  free_initializers(&init);

  init_var->init_data = var->constexpr_data = buf;
  init_var->rel = head.next;
  var->ty = init_var->ty;
}

// Returns true if a given token represents a type.
static bool is_typename(Token *tok) {
  return tok->kind == TK_TYPEKW || find_typedef(tok);
}

static bool is_typename_paren(Token **rest, Token *tok, Type **ty) {
  if (equal(tok, "(") && is_typename(tok->next) &&
    !equal(skip_paren(tok->next->next), "{")) {
    *ty = typename(&tok, tok->next);
    *rest = skip(tok, ")");
    return true;
  }
  return false;
}

static void static_assertion(Token **rest, Token *tok) {
  tok = skip(tok, "(");
  int64_t result = const_expr(&tok, tok);
  if (!result)
    error_tok(tok, "static assertion failed");

  if (equal(tok, ",")) {
    if (tok->next->kind != TK_STR)
      error_tok(tok, "expected string literal");
    tok = tok->next->next;
  }
  tok = skip(tok, ")");
  *rest = skip(tok, ";");
}

static AsmParam *asm_params(Token **rest, Token *tok) {
  AsmParam head = {0};
  AsmParam *cur = &head;
  while (!equal(tok, ":") && !equal(tok, ")")) {
    if (cur != &head)
      tok = skip(tok, ",");
    cur = cur->next = arena_calloc(&ast_arena, sizeof(AsmParam));

    if (consume(&tok, tok, "[")) {
      cur->name = tok;
      tok = skip(tok->next, "]");
    }
    cur->constraint = str_tok(&tok, tok);
    cur->arg = expr(&tok, skip(tok, "("));
    tok = skip(tok, ")");
  }
  *rest = tok;
  return head.next;
}

static Token *asm_clobbers(Token **rest, Token *tok) {
  Token *first = NULL;
  while (!equal(tok, ":") && !equal(tok, ")")) {
    if (!first) {
      first = str_tok(&tok, tok);
      continue;
    }
    tok = skip(tok, ",");
    str_tok(&tok, tok);
  }
  *rest = tok;
  return first;
}

static AsmParam *asm_labels(Token **rest, Token *tok) {
  AsmParam head = {0};
  AsmParam *cur = &head;
  while (comma_list(rest, &tok, ")", cur != &head)) {
    Node *node = new_node(ND_GOTO, tok);
    node->labels = ident_tok(&tok, tok);
    node->goto_next = gotos;
    gotos = node;

    cur = cur->next = arena_calloc(&ast_arena, sizeof(AsmParam));
    cur->arg = node;
  }
  return head.next;
}

// asm-stmt = "__asm__" ("volatile" | "inline")* "(" string-literal ")"
static Node *asm_stmt(Token **rest, Token *tok) {
  Node *node = new_node(ND_ASM, tok);
  tok = tok->next;

  bool is_asm_goto = false;
  for (;;) {
    if (equal(tok, "inline") || equal(tok, "volatile") ||
      equal(tok, "__volatile") || equal(tok, "__volatile__")) {
      tok = tok->next;
      continue;
    } else if (equal(tok, "goto")) {
      tok = tok->next;
      is_asm_goto = true;
      continue;
    }
    break;
  }
  node->asm_str = str_tok(&tok, skip(tok, "("));
  if (consume(rest, tok, ")"))
    return node;

  node->asm_outputs = asm_params(&tok, skip(tok, ":"));
  if (consume(rest, tok, ")"))
    return node;

  node->asm_inputs = asm_params(&tok, skip(tok, ":"));
  if (consume(rest, tok, ")"))
    return node;

  node->asm_clobbers = asm_clobbers(&tok, skip(tok, ":"));
  if (!is_asm_goto) {
    *rest = skip(tok, ")");
    return node;
  }
  node->asm_labels = asm_labels(rest, skip(tok, ":"));
  return node;
}

static Token *label_stmt(Node **cur_node, Token **rest, Token *tok) {
  Node *node = NULL;
  Node *active_sw = NULL;

  Token head = {0};
  Token *cur = &head;

  for (;;) {
    if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
      if (!node) {
        node = new_node(ND_LABEL, tok);
        node->unique_label = new_unique_name();;
      }
      cur = cur->label_next = tok;
      tok = tok->next->next;
      continue;
    }

    if (equal(tok, "case") || equal(tok, "default")) {
      if (!node) {
        node = new_node(ND_LABEL, tok);
        node->unique_label = new_unique_name();;
      }
      if (!active_sw) {
        JumpContext *ctx = jump_ctx;
        for (; ctx; ctx = ctx->next)
          if (ctx->switch_node)
            break;
        if (!ctx)
          error_tok(tok, "stray case");
        if (jump_ctx->defr_end != current_defr)
          error_tok(tok, "illegal jump");
        active_sw = ctx->switch_node;
      }

      if (equal(tok, "default")) {
        if (active_sw->default_case)
          error_tok(tok, "duplicated defualt");
        active_sw->default_case = ast_arena_malloc(sizeof(CaseRange));
        active_sw->default_case->label = node->unique_label;
        tok = skip(tok->next, ":");
        continue;
      }

      int64_t lo = const_expr(&tok, tok->next);
      int64_t hi;
      if (equal(tok, "..."))
        hi = const_expr(&tok, tok->next);
      else
        hi = lo;

      Type *ty = active_sw->cond->ty;
      if (ty->size <= 4) {
        if (!ty->is_unsigned)
          lo = (int32_t)lo, hi = (int32_t)hi;
        else
          lo = (uint32_t)lo, hi = (uint32_t)hi;
      }
      if (hi != lo && less_eq(ty, hi, lo))
        error_tok(tok, "empty case range specified");

      for (CaseRange *cr = active_sw->cases; cr; cr = cr->next)
        if ((less_eq(ty, cr->lo, lo) && less_eq(ty, lo, cr->hi)) ||
          (less_eq(ty, lo, cr->lo) && less_eq(ty, cr->lo, hi)))
          error_tok(tok, "duplicated case");

      tok = skip(tok, ":");

      // Merge adjacent ranges
      if (opt_optimize) {
        CaseRange *lo_adj = NULL, *hi_adj = NULL;
        for (CaseRange *cr = active_sw->cases; cr; cr = cr->next) {
          if (!cr->label)
            continue;
          if (cr->label != node->unique_label)
            break;
          if (less_eq(ty, cr->hi, lo) && (uint64_t)cr->hi + 1 == lo)
            lo_adj = cr;
          else if (less_eq(ty, hi, cr->lo) && (uint64_t)hi + 1 == cr->lo)
            hi_adj = cr;
        }
        if (lo_adj && hi_adj) {
          lo_adj->hi = hi_adj->hi;
          hi_adj->label = NULL;
          continue;
        } else if (lo_adj) {
          lo_adj->hi = hi;
          continue;
        } else if (hi_adj) {
          hi_adj->lo = lo;
          continue;
        }
      }

      CaseRange *cr = ast_arena_malloc(sizeof(CaseRange));
      cr->label = node->unique_label;
      cr->lo = lo;
      cr->hi = hi;
      cr->next = active_sw->cases;
      active_sw->cases = cr;
      continue;
    }
    if (node) {
      node->labels = head.label_next;
      node->defr_start = current_defr;
      node->goto_next = labels;
      (*cur_node) = (*cur_node)->next = labels = node;
    }
    cur->label_next = NULL;
    *rest = tok;
    return head.label_next;
  }
}

static Node *secondary_block(Token **rest, Token *tok) {
  if (equal(tok, "{"))
    return compound_stmt(rest, tok->next, ND_BLOCK);

  DeferStmt *dfr = new_block_scope();
  Node head = {0};
  Node *cur = &head;
  Token *label_list = label_stmt(&cur, &tok, tok);
  cur->next = stmt(rest, tok, label_list);
  return leave_block_scope(dfr, head.next);
}

static void loop_body(Token **rest, Token *tok, Node *node, Token *label_list) {
  JumpContext ctx = {.next = jump_ctx};
  jump_ctx = &ctx;

  ctx.labels = label_list;
  ctx.brk_label = node->brk_label = new_unique_name();
  ctx.cont_label = node->cont_label = new_unique_name();
  ctx.defr_end = current_defr;

  node->then = secondary_block(rest, tok);

  jump_ctx = ctx.next;
}

static JumpContext *resolve_labeled_jump(Token **rest, Token *tok, bool is_cont) {
  Token *name = NULL;
  if (tok->next->kind == TK_IDENT)
    name = tok = tok->next;
  *rest = skip(tok->next, ";");

  for (JumpContext *ctx = jump_ctx; ctx; ctx = ctx->next) {
    if (is_cont && ctx->switch_node)
      continue;
    if (!name)
      return ctx;
    for (Token *t = ctx->labels; t; t = t->label_next)
      if (equal_tok(t, name))
        return ctx;
  }
  error_tok(tok, "cannot resolve jump");
}

// stmt = "return" expr? ";"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "switch" "(" expr ")" stmt
//      | "case" const-expr ("..." const-expr)? ":" stmt
//      | "default" ":" stmt
//      | "for" "(" expr-stmt expr? ";" expr? ")" stmt
//      | "while" "(" expr ")" stmt
//      | "do" stmt "while" "(" expr ")" ";"
//      | "__asm__" asm-stmt
//      | "goto" (ident | "*" expr) ";"
//      | "break" ";"
//      | "continue" ";"
//      | ident ":" stmt
//      | "{" compound-stmt
//      | expr-stmt
static Node *stmt(Token **rest, Token *tok, Token *label_list) {
  if (equal(tok, "return")) {
    Node *node = new_node(ND_RETURN, tok);
    node->defr_start = current_defr;
    if (consume(rest, tok->next, ";"))
      return node;

    Node *exp = expr(&tok, tok->next);
    *rest = skip(tok, ";");

    add_type(exp);
    Type *ty = current_fn->ty->return_ty;
    if (ty->kind != TY_STRUCT && ty->kind != TY_UNION)
      exp = new_cast(exp, current_fn->ty->return_ty);

    node->lhs = exp;
    return node;
  }

  if (equal(tok, "if")) {
    DeferStmt *dfr = new_block_scope();

    Node *node = new_node(ND_IF, tok);
    node->cond = cond_cast(cond_declaration(&tok, skip(tok->next, "("), ")", 0));
    node->then = secondary_block(&tok, tok);
    if (consume(&tok, tok, "else"))
      node->els = secondary_block(&tok, tok);

    *rest = tok;
    return leave_block_scope(dfr, node);
  }

  if (equal(tok, "switch")) {
    DeferStmt *dfr = new_block_scope();

    Node *node = new_node(ND_SWITCH, tok);
    node->cond = cond_declaration(&tok, skip(tok->next, "("), ")", 0);
    add_type(node->cond);
    if (!is_integer(node->cond->ty))
      error_tok(tok, "controlling expression not integer");

    JumpContext ctx = {.next = jump_ctx, .switch_node = node};
    jump_ctx = &ctx;

    ctx.labels = label_list;
    ctx.brk_label = node->brk_label = new_unique_name();
    ctx.defr_end = current_defr;

    node->then = secondary_block(rest, tok);

    jump_ctx = ctx.next;
    return leave_block_scope(dfr, node);
  }

  if (equal(tok, "for")) {
    DeferStmt *dfr = new_block_scope();

    Node *node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");

    if (is_typename(tok)) {
      VarAttr attr = {0};
      Type *basety = declspec(&tok, tok, &attr);
      Node *expr = declaration(&tok, tok, basety, &attr);
      if (expr)
        node->init = new_unary(ND_EXPR_STMT, expr, tok);
    } else if (equal(tok, "_Static_assert") || equal_kw(tok, "static_assert")) {
      static_assertion(&tok, tok->next);
    } else {
      node->init = expr_stmt(&tok, tok);
    }

    if (!consume(&tok, tok, ";")) {
      node->defr_end = current_defr;
      node->cond = cond_cast(cond_declaration(&tok, tok, ";", 1));
      node->defr_start = current_defr;
    }

    if (!equal(tok, ")"))
      node->inc = expr(&tok, tok);
    tok = skip(tok, ")");

    loop_body(rest, tok, node, label_list);

    return leave_block_scope(dfr, node);
  }

  if (equal(tok, "while")) {
    DeferStmt *dfr = new_block_scope();

    Node *node = new_node(ND_FOR, tok);
    node->defr_end = current_defr;
    node->cond = cond_cast(cond_declaration(&tok, skip(tok->next, "("), ")", 1));
    node->defr_start = current_defr;

    loop_body(rest, tok, node, label_list);

    return leave_block_scope(dfr, node);
  }

  if (equal(tok, "do")) {
    Node *node = new_node(ND_DO, tok);

    loop_body(&tok, tok->next, node, label_list);

    tok = skip(tok, "while");
    tok = skip(tok, "(");
    node->cond = cond_cast(expr(&tok, tok));
    tok = skip(tok, ")");
    *rest = skip(tok, ";");
    return node;
  }

  if (equal_kw(tok, "asm") || equal(tok, "__asm") || equal(tok, "__asm__")) {
    Node *node = asm_stmt(&tok, tok);

    enter_tmp_scope();
    prepare_inline_asm(node);
    leave_scope();

    *rest = skip(tok, ";");
    return node;
  }

  if (equal(tok, "goto")) {
    if (equal(tok->next, "*")) {
      // [GNU] `goto *ptr` jumps to the address specified by `ptr`.
      Node *node = new_node(ND_GOTO_EXPR, tok);
      node->lhs = expr(&tok, tok->next->next);
      *rest = skip(tok, ";");
      return node;
    }

    Node *node = new_node(ND_GOTO, tok);
    node->labels = ident_tok(&tok, tok->next);
    node->goto_next = gotos;
    node->defr_start = current_defr;
    gotos = node;
    *rest = skip(tok, ";");
    return node;
  }

  if (equal(tok, "break") || equal(tok, "continue")) {
    Node *node = new_node(ND_GOTO, tok);

    bool is_cont = equal(tok, "continue");
    JumpContext *ctx = resolve_labeled_jump(rest, tok, is_cont);

    node->unique_label = is_cont ? ctx->cont_label : ctx->brk_label;
    node->defr_end = ctx->defr_end;
    node->defr_start = current_defr;
    return node;
  }

  if (equal_kw(tok, "defer") || equal(tok, "_Defer")) {
    Node *node = secondary_block(rest, tok->next);
    add_type(node);
    new_defr(DF_DEFER_STMT)->stmt = node;
    return new_node(ND_NULL_STMT, tok);
  }

  if (equal(tok, "{"))
    return compound_stmt(rest, tok->next, ND_BLOCK);

  return expr_stmt(rest, tok);
}

// compound-stmt = (typedef | declaration | stmt)* "}"
static Node *compound_stmt(Token **rest, Token *tok, NodeKind kind) {
  Node *node = new_node(kind, tok);
  node->defr_end = current_defr;
  enter_scope();

  Node head = {0};
  Node *cur = &head;

  for (;;) {
    Token *label_list = label_stmt(&cur, &tok, tok);

    if (equal(tok, "}"))
      break;

    if (equal(tok, "_Static_assert") || equal_kw(tok, "static_assert")) {
      static_assertion(&tok, tok->next);
      continue;
    }

    if (!is_typename(tok)) {
      cur = cur->next = stmt(&tok, tok, label_list);
      add_type(cur);
      continue;
    }

    VarAttr attr = {0};
    Type *basety = declspec(&tok, tok, &attr);

    Node *init_expr = NULL;
    if (attr.is_extern)
      global_declaration(&tok, tok, basety, &attr);
    else if (attr.is_typedef)
      init_expr = parse_typedef(&tok, tok, basety, &attr);
    else
      init_expr = declaration(&tok, tok, basety, &attr);

    if (init_expr) {
      cur = cur->next = new_unary(ND_EXPR_STMT, init_expr, tok);
      add_type(cur);
    }
  }

  if (cur->kind == ND_RETURN || cur->kind == ND_GOTO)
    current_defr = node->defr_end;

  node->defr_start = current_defr;
  current_defr = node->defr_end;
  leave_scope();

  if (kind == ND_STMT_EXPR && cur->kind == ND_EXPR_STMT) {
    add_type(cur->lhs);
    Type *ty = cur->lhs->ty;
    if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
      Obj *var = new_lvar(NULL, ty);
      Node *expr = new_binary(ND_ASSIGN, new_var_node(var, tok), cur->lhs, tok);
      chain_expr(&expr, new_var_node(var, tok));
      cur->lhs = expr;
    }
  }

  node->body = head.next;
  *rest = tok->next;
  return node;
}

// expr-stmt = expr? ";"
static Node *expr_stmt(Token **rest, Token *tok) {
  if (consume(rest, tok, ";"))
    return new_node(ND_NULL_STMT, tok);

  Node *node = new_node(ND_EXPR_STMT, tok);
  node->lhs = expr(&tok, tok);
  *rest = skip(tok, ";");
  return node;
}

// expr = assign ("," expr)?
static Node *expr(Token **rest, Token *tok) {
  Node *node = assign(&tok, tok);

  if (equal(tok, ","))
    return new_binary(ND_COMMA, node, expr(rest, tok->next), tok);

  *rest = tok;
  return node;
}

static int64_t eval_error(Token *tok, char *fmt, ...) {
  if (eval_recover) {
    *eval_recover = true;
    return 0;
  }
  va_list ap;
  va_start(ap, fmt);
  verror_at_tok(tok, fmt, ap);
  va_end(ap);
  exit(1);
}

static bool eval_ctx(Node *node, EvalContext *ctx, int64_t *val) {
  bool failed = false;
  bool *prev = eval_recover;
  eval_recover = &failed;

  int64_t v = eval2(node, ctx);
  if (val)
    *val = v;
  eval_recover = prev;
  return !failed;
}

static bool eval_non_var_ofs(Node *node, int64_t *ofs) {
  if (node->kind == ND_MEMBER || node->kind == ND_DEREF) {
    EvalContext ctx = {.kind = EV_AGGREGATE,
      .let_array = true, .let_atomic = true, .let_volatile = true};
    if (eval_ctx(node, &ctx, ofs) && !ctx.var)
      return true;
  }
  return false;
}

static Obj *eval_var_ofs(Node *node, int *ofs, bool let_array, bool let_volatile, bool let_atomic) {
  if (node->kind == ND_VAR && node->ty->kind != TY_VLA) {
    if ((let_volatile || !node->ty->is_volatile) &&
      (let_atomic || !node->ty->is_atomic)) {
      *ofs = 0;
      return node->var;
    }
  }
  if (node->kind == ND_MEMBER || node->kind == ND_DEREF) {
    int64_t offset;
    EvalContext ctx = {.kind = EV_AGGREGATE, .let_array = let_array,
      .let_volatile = let_volatile, .let_atomic = let_atomic};
    if (eval_ctx(node, &ctx, &offset) && ctx.var) {
      *ofs = offset;
      return ctx.var;
    }
  }
  return NULL;
}

static Obj *eval_var(Node *node, int *ofs, bool let_volatile) {
  return eval_var_ofs(node, ofs, true, let_volatile, true);
}

Obj *eval_var_opt(Node *node, int *ofs, bool let_subarray, bool let_atomic) {
  return eval_var_ofs(node, ofs, let_subarray, true, let_atomic);
}

static bool is_static_const_var(Obj *var, int ofs, int read_sz) {
  if (!opt_optimize || !var->init_data || var->is_weak || !is_const_var(var))
    return false;
  for (Relocation *rel = var->rel; rel; rel = rel->next) {
    if ((rel->offset + ty_intptr_t->size) <= ofs)
      continue;
    if ((ofs + read_sz) <= rel->offset)
      break;
    return false;
  }
  return true;
}

static char *eval_constexpr_data(Node *node) {
  int32_t ofs;
  Obj *var = eval_var(node, &ofs, false);

  if (!var || !(var->constexpr_data || is_static_const_var(var, ofs, node->ty->size)))
    return (char *)eval_error(node->tok, "not a compile-time constant");

  int32_t access_sz = !is_bitfield(node) ? node->ty->size : bitfield_footprint(node->member);

  if (ofs < 0 || (var->ty->size < (ofs + access_sz)))
    return (char *)eval_error(node->tok, "constexpr access out of bounds");

  if (var->constexpr_data)
    return var->constexpr_data + ofs;

  return var->init_data + ofs;
}

static int64_t eval_sign_extend(Type *ty, int64_t val) {
  if (ty->size == 8 || !is_integer(ty))
    return val;

  switch (ty->size) {
  case 1: return ty->is_unsigned ? (uint8_t)val : (int64_t)(int8_t)val;
  case 2: return ty->is_unsigned ? (uint16_t)val : (int64_t)(int16_t)val;
  case 4: return ty->is_unsigned ? (uint32_t)val : (int64_t)(int32_t)val;
  }
  internal_error();
}

static void eval_void(Node *node) {
  if (node->kind == ND_VAR)
    return;
  if (node->ty->kind == TY_BITINT)
    free(eval_bitint(node));
  else if (is_flonum(node->ty))
    eval_double(node);
  else
    eval(node);
}

static int64_t eval_cmp(Node *node) {
  Node *lhs = node->lhs;
  Node *rhs = node->rhs;

  if (lhs->ty->kind == TY_BITINT) {
    uint64_t *ldata = eval_bitint(lhs);
    uint64_t *rdata = eval_bitint(rhs);
    if (eval_recover && *eval_recover)
      return free(ldata), free(rdata), 0;

    int res = eval_bitint_cmp(lhs->ty->bit_cnt, ldata, rdata, lhs->ty->is_unsigned);
    free(ldata), free(rdata);

    switch (node->kind) {
    case ND_EQ: return res == 0;
    case ND_NE: return res != 0;
    case ND_LT: return res == 1;
    case ND_LE: return res != 2;
    case ND_GT: return res == 2;
    case ND_GE: return res != 1;
    }
  } else if (is_flonum(lhs->ty)) {
    switch (node->kind) {
    case ND_EQ: return eval_double(lhs) == eval_double(rhs);
    case ND_NE: return eval_double(lhs) != eval_double(rhs);
    case ND_LT: return eval_double(lhs) < eval_double(rhs);
    case ND_LE: return eval_double(lhs) <= eval_double(rhs);
    case ND_GT: return eval_double(lhs) > eval_double(rhs);
    case ND_GE: return eval_double(lhs) >= eval_double(rhs);
    }
  } else if (lhs->ty->is_unsigned) {
    switch (node->kind) {
    case ND_EQ: return (uint64_t)eval(lhs) == (uint64_t)eval(rhs);
    case ND_NE: return (uint64_t)eval(lhs) != (uint64_t)eval(rhs);
    case ND_LT: return (uint64_t)eval(lhs) < (uint64_t)eval(rhs);
    case ND_LE: return (uint64_t)eval(lhs) <= (uint64_t)eval(rhs);
    case ND_GT: return (uint64_t)eval(lhs) > (uint64_t)eval(rhs);
    case ND_GE: return (uint64_t)eval(lhs) >= (uint64_t)eval(rhs);
    }
  } else {
    switch (node->kind) {
    case ND_EQ: return eval(lhs) == eval(rhs);
    case ND_NE: return eval(lhs) != eval(rhs);
    case ND_LT: return eval(lhs) < eval(rhs);
    case ND_LE: return eval(lhs) <= eval(rhs);
    case ND_GT: return eval(lhs) > eval(rhs);
    case ND_GE: return eval(lhs) >= eval(rhs);
    }
  }
  internal_error();
}

static int64_t eval(Node *node) {
  return eval2(node, &(EvalContext){.kind = EV_CONST});
}

// Evaluate a given node as a constant expression.
//
// A constant expression is either just a number or ptr+n where ptr
// is a pointer to a global variable and n is a postiive/negative
// number. The latter form is accepted only as an initialization
// expression for a global variable.
static int64_t eval2(Node *node, EvalContext *ctx) {
  if (eval_recover && *eval_recover)
    return 0;

  Type *ty = node->ty;
  Node *lhs = node->lhs;
  Node *rhs = node->rhs;

  switch (node->kind) {
  case ND_ADD:
    return eval_sign_extend(ty, eval2(lhs, ctx) + eval2(rhs, ctx));
  case ND_SUB:
    return eval_sign_extend(ty, eval2(lhs, ctx) - eval(rhs));
  case ND_MUL:
    return eval_sign_extend(ty, eval(lhs) * eval(rhs));
  case ND_DIV: {
    int64_t lval = eval(lhs);
    int64_t rval = eval(rhs);
    if (!rval)
      return eval_error(rhs->tok, "division by zero during constant evaluation");
    if (ty->is_unsigned)
      return (uint64_t)lval / rval;
    if (lval == INT64_MIN && rval == -1)
      return INT64_MIN;
    return lval / rval;
  }
  case ND_MOD: {
    int64_t lval = eval(lhs);
    int64_t rval = eval(rhs);
    if (!rval)
      return eval_error(rhs->tok, "remainder by zero during constant evaluation");
    if (ty->is_unsigned)
      return (uint64_t)lval % rval;
    if (lval == INT64_MIN && rval == -1)
      return 0;
    return lval % rval;
  }
  case ND_POS:
    return eval(lhs);
  case ND_NEG:
    return eval_sign_extend(ty, -eval(lhs));
  case ND_BITAND:
    return eval(lhs) & eval(rhs);
  case ND_BITOR:
    return eval(lhs) | eval(rhs);
  case ND_BITXOR:
    return eval(lhs) ^ eval(rhs);
  case ND_SHL:
    return eval_sign_extend(ty, eval(lhs) << eval(rhs));
  case ND_SHR:
    if (ty->size == 4)
      return (uint32_t)eval(lhs) >> eval(rhs);
    return (uint64_t)eval(lhs) >> eval(rhs);
  case ND_SAR:
    if (ty->size == 4)
      return (int32_t)eval(lhs) >> eval(rhs);
    return eval(lhs) >> eval(rhs);
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
    return eval_cmp(node);
  case ND_COND:
    return eval(node->cond) ? eval2(node->then, ctx) : eval2(node->els, ctx);
  case ND_COMMA:
    eval_void(lhs);
    return eval2(rhs, ctx);
  case ND_NOT:
    return !eval(lhs);
  case ND_BITNOT:
    return eval_sign_extend(ty, ~eval(lhs));
  case ND_LOGAND:
    return eval(lhs) && eval(rhs);
  case ND_LOGOR:
    return eval(lhs) || eval(rhs);
  case ND_CAST: {
    if (lhs->ty->kind == TY_BITINT) {
      uint64_t *val = eval_bitint(lhs);
      if (eval_recover && *eval_recover)
        return free(val), 0;

      if (ty->kind == TY_BOOL) {
        bool res = eval_bitint_to_bool(lhs->ty->bit_cnt, val);
        free(val);
        return res;
      }
      int64_t ival = val[0];
      free(val);
      if (lhs->ty->bit_cnt < ty->size * 8) {
        int shft = 64 - lhs->ty->bit_cnt;
        if (lhs->ty->is_unsigned)
          ival = (uint64_t)(ival << shft) >> shft;
        else
          ival = (int64_t)(ival << shft) >> shft;
      }
      return eval_sign_extend(ty, ival);
    }

    if (is_flonum(lhs->ty)) {
      if (ty->kind == TY_BOOL)
        return !!eval_double(lhs);
      if (ty->size == 8 && ty->is_unsigned)
        return (uint64_t)eval_double(lhs);
      return eval_sign_extend(ty, eval_double(lhs));
    }

    if (ty->kind == TY_BOOL && lhs->kind == ND_VAR &&
      !lhs->var->is_weak && (is_array(lhs->ty)))
      return true;

    if (ty->size != ty_intptr_t->size) {
      EvalContext ctx2= {.kind = EV_LABEL};
      if (eval_ctx(lhs, &ctx2, NULL) && ctx2.label) {
        if (ty->kind == TY_BOOL && !ctx2.var->is_weak)
          return true;
        return eval_error(node->tok, "pointer cast to different size");
      }
    }

    int64_t val = eval2(lhs, ctx);
    if (ty->kind == TY_BOOL)
      return !!val;
    if (is_integer(ty))
      return eval_sign_extend(ty, val);
    return val;
  }
  case ND_NUM:
    return node->val;
  }

  if (ctx->kind == EV_AGGREGATE) {
    if ((ty->is_atomic && !ctx->let_atomic) ||
      (ty->is_volatile && !ctx->let_volatile))
      return eval_error(node->tok, "not a compile-time constant");

    if (node->kind == ND_DEREF) {
      ctx->deref_cnt++;
      return eval2(lhs, ctx);
    }

    if (node->kind == ND_ADDR && ctx->deref_cnt) {
      ctx->deref_cnt--;
      return eval2(lhs, ctx);
    }

    for (Type *t = ty; t && t->kind == TY_ARRAY; t = t->base)
      ctx->deref_cnt--;

    if (ctx->deref_cnt < 0) {
      if (!ctx->let_array)
        return eval_error(node->tok, "not a compile-time constant");
      ctx->let_array = false;
      ctx->deref_cnt = 0;
    }

    bool is_agg = ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION;

    if (node->kind == ND_MEMBER && (!ctx->deref_cnt || is_agg))
      return eval2(lhs, ctx) + node->member->offset;

    if (node->kind == ND_VAR && (!ctx->deref_cnt && is_agg)) {
      ctx->var = node->var;
      return 0;
    }
  }

  if (ctx->kind == EV_LABEL) {
    if (!ctx->label) {
      if (node->kind == ND_LABEL_VAL) {
        ctx->label = &node->unique_label;
        return 0;
      }

      int ofs;
      Obj *var = NULL;
      if (node->kind == ND_ADDR)
        var = eval_var(lhs, &ofs, true);
      else if (node->ty->kind == TY_ARRAY || node->ty->kind == TY_FUNC)
        var = eval_var(node, &ofs, true);

      if (var) {
        ctx->var = var;
        ctx->label = var->asm_name ? &var->asm_name : &var->name;
        return ofs;
      }
    }
    return eval_error(node->tok, "invalid initializer");
  }

  if (ctx->kind == EV_CONST) {
    if (node->kind == ND_ADDR) {
      int64_t ofs;
      if (eval_non_var_ofs(lhs, &ofs))
        return ofs;
    }
    if (is_integer(ty) || ty->kind == TY_PTR || ty->kind == TY_NULLPTR) {
      char *data = eval_constexpr_data(node);
      if (data) {
        int64_t val = eval_sign_extend(ty, read_buf(data, ty->size));
        if (is_bitfield(node)) {
          int unused_msb = 64 - node->member->bit_width;
          val <<= (unused_msb - node->member->bit_offset);

          if (ty->is_unsigned)
            return (uint64_t)val >> unused_msb;
          return val >> unused_msb;
        }
        return val;
      }
    }
  }
  return eval_error(node->tok, "not a compile-time constant");
}

bool is_const_expr(Node *node, int64_t *val) {
  add_type(node);
  bool failed = false;
  bool *prev = eval_recover;
  eval_recover = &failed;

  int64_t v = eval(node);
  if (val)
    *val = v;
  eval_recover = prev;
  return !failed;
}

bool is_const_double(Node *node, long double *fval) {
  add_type(node);
  bool failed = false;
  bool *prev = eval_recover;
  eval_recover = &failed;

  long double v = eval_double(node);
  if (fval)
    *fval = v;
  eval_recover = prev;
  return !failed;
}

static int64_t const_expr2(Token **rest, Token *tok, Type **ty) {
  Node *node = conditional(rest, tok);
  add_type(node);
  if (!is_integer(node->ty))
    error_tok(tok, "constant expression not integer");
  if (ty)
    *ty = node->ty;
  return eval(node);
}

int64_t const_expr(Token **rest, Token *tok) {
  return const_expr2(rest, tok, NULL);
}

static long double eval_fp_cast(long double fval, Type *ty) {
  switch (ty->kind) {
  case TY_FLOAT: return (float)fval;
  case TY_DOUBLE: return (double)fval;
  case TY_LDOUBLE: return fval;
  }
  internal_error();
}

static long double eval_double(Node *node) {
  if (eval_recover && *eval_recover)
    return false;

  Type *ty = node->ty;
  Node *lhs = node->lhs;
  Node *rhs = node->rhs;

  switch (node->kind) {
  case ND_ADD: return eval_fp_cast(eval_double(lhs) + eval_double(rhs), ty);
  case ND_SUB: return eval_fp_cast(eval_double(lhs) - eval_double(rhs), ty);
  case ND_MUL: return eval_fp_cast(eval_double(lhs) * eval_double(rhs), ty);
  case ND_DIV: return eval_fp_cast(eval_double(lhs) / eval_double(rhs), ty);
  case ND_POS:
    return eval_double(lhs);
  case ND_NEG:
    return -eval_double(lhs);
  case ND_COND:
    return eval(node->cond) ? eval_double(node->then) : eval_double(node->els);
  case ND_COMMA:
    eval_void(lhs);
    return eval_double(rhs);
  case ND_CAST:
    if (is_flonum(lhs->ty))
      return eval_fp_cast(eval_double(lhs), ty);
    if (lhs->ty->size == 8 && lhs->ty->is_unsigned)
      return (uint64_t)eval(lhs);
    if (is_integer(lhs->ty))
      return eval(lhs);
    error_tok(node->tok, "unimplemented cast");
  case ND_NUM:
    return node->fval;
  }

  char *data = eval_constexpr_data(node);
  if (data)
    return read_double_buf(data, ty);

  return eval_error(node->tok, "not a compile-time constant");
}

static uint64_t *eval_bitint(Node *node) {
  if (eval_recover && *eval_recover)
    return NULL;

  Type *ty = node->ty;
  Node *lhs = node->lhs;
  Node *rhs = node->rhs;

  switch (node->kind) {
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
  case ND_ADD:
  case ND_SUB:
  case ND_MUL:
  case ND_DIV:
  case ND_MOD: {
    uint64_t *lval = eval_bitint(lhs);
    uint64_t *rval = eval_bitint(rhs);
    if (eval_recover && *eval_recover)
      return free(lval), free(rval), NULL;

    switch (node->kind) {
    case ND_BITAND: eval_bitint_bitand(ty->bit_cnt, lval, rval); break;
    case ND_BITOR: eval_bitint_bitor(ty->bit_cnt, lval, rval); break;
    case ND_BITXOR: eval_bitint_bitxor(ty->bit_cnt, lval, rval); break;
    case ND_ADD: eval_bitint_add(ty->bit_cnt, lval, rval); break;
    case ND_SUB: eval_bitint_sub(ty->bit_cnt, lval, rval); break;
    case ND_MUL: eval_bitint_mul(ty->bit_cnt, lval, rval); break;
    case ND_DIV:
    case ND_MOD: {
      bool res = eval_bitint_to_bool(ty->bit_cnt, rval);
      if (!res)
        return (void *)eval_error(node->tok, "division by zero during constant evaluation");
      eval_bitint_div(ty->bit_cnt, lval, rval, ty->is_unsigned, node->kind == ND_DIV);
      break;
    }
    }
    return free(lval), rval;
  }
  case ND_SHL:
  case ND_SHR:
  case ND_SAR: {
    uint64_t *val = eval_bitint(lhs);
    uint64_t amount = eval(rhs);
    if (eval_recover && *eval_recover)
      return free(val), NULL;

    if (amount < 0 || amount >= ty->bit_cnt)
      return (void *)eval_error(rhs->tok, "invalid shift amount");

    if (node->kind == ND_SHL)
      eval_bitint_shl(ty->bit_cnt, val, val, amount);
    else
      eval_bitint_shr(ty->bit_cnt, val, val, amount, ty->is_unsigned);
    return val;
  }
  case ND_BITNOT:
  case ND_POS:
  case ND_NEG: {
    uint64_t *val = eval_bitint(lhs);
    if (eval_recover && *eval_recover)
      return NULL;
    switch (node->kind) {
    case ND_BITNOT: eval_bitint_bitnot(ty->bit_cnt, val); break;
    case ND_NEG: eval_bitint_neg(ty->bit_cnt, val); break;
    }
    return val;
  }
  case ND_COND:
    return eval(node->cond) ? eval_bitint(node->then) : eval_bitint(node->els);
  case ND_COMMA:
    eval_void(lhs);
    return eval_bitint(rhs);
  case ND_CAST:
    if (lhs->ty->kind == TY_BITINT) {
      uint64_t *val = eval_bitint(lhs);
      if (eval_recover && *eval_recover)
        return NULL;

      val = realloc(val, MAX(ty->size, 8));
      if (ty->bit_cnt > lhs->ty->bit_cnt)
        eval_bitint_sign_ext(lhs->ty->bit_cnt, val, ty->bit_cnt, lhs->ty->is_unsigned);
      return val;
    }
    if (is_integer(lhs->ty)) {
      uint64_t *val = malloc(MAX(ty->size, 8));
      val[0] = eval(lhs);

      if (ty->bit_cnt > lhs->ty->size * 8)
        eval_bitint_sign_ext(lhs->ty->size * 8, val, ty->bit_cnt, lhs->ty->is_unsigned);
      return val;
    }
    error_tok(node->tok, "unimplemented cast");
  case ND_NUM: {
    uint64_t *val = malloc(MAX(ty->size, 8));
    memcpy(val, node->bitint_data, ty->size);
    return val;
  }
  }

  char *data = eval_constexpr_data(node);
  if (data) {
    uint64_t *val = malloc(MAX(ty->size, 8));
    if (is_bitfield(node)) {
      memcpy(val, data, bitfield_footprint(node->member));
      eval_bitint_bitfield_load(ty->bit_cnt, val, val,
        node->member->bit_width, node->member->bit_offset, ty->is_unsigned);
      return val;
    }
    memcpy(val, data, ty->size);
    return val;
  }

  return (void *)eval_error(node->tok, "not a compile-time constant");
}

static Node *atomic_op(Node *binary, bool return_old) {
  // ({
  //   T *addr = &obj; T old = *addr; T new;
  //   do {
  //    new = old op val;
  //   } while (!atomic_compare_exchange_strong(addr, &old, new));
  //
  //   return_old ? old : new;
  // })
  Token *tok = binary->tok;
  Node head = {0};
  Node *cur = &head;

  Obj *addr = new_lvar(NULL, pointer_to(binary->lhs->ty));
  Obj *val = new_lvar(NULL, binary->rhs->ty);
  Obj *old = new_lvar(NULL, binary->lhs->ty);
  Obj *new = new_lvar(NULL, binary->lhs->ty);

  cur = cur->next =
    new_unary(ND_EXPR_STMT,
              new_binary(ND_ASSIGN, new_var_node(addr, tok),
                         new_unary(ND_ADDR, binary->lhs, tok), tok),
              tok);

  cur = cur->next =
    new_unary(ND_EXPR_STMT,
              new_binary(ND_ASSIGN, new_var_node(val, tok), binary->rhs, tok),
              tok);

  cur = cur->next =
    new_unary(ND_EXPR_STMT,
              new_binary(ND_ASSIGN, new_var_node(old, tok),
                         new_unary(ND_DEREF, new_var_node(addr, tok), tok), tok),
              tok);

  Node *loop = new_node(ND_DO, tok);
  loop->brk_label = new_unique_name();
  loop->cont_label = new_unique_name();

  Node *body = new_binary(ND_ASSIGN,
                          new_var_node(new, tok),
                          new_binary(binary->kind, new_var_node(old, tok),
                                     new_var_node(val, tok), tok),
                          tok);

  loop->then = new_node(ND_BLOCK, tok);
  loop->then->body = new_unary(ND_EXPR_STMT, body, tok);

  Node *cas = new_node(ND_CAS, tok);
  cas->cas_addr = new_var_node(addr, tok);
  cas->cas_old = new_unary(ND_ADDR, new_var_node(old, tok), tok);
  cas->cas_new = new_var_node(new, tok);
  loop->cond = new_unary(ND_NOT, cas, tok);

  cur = cur->next = loop;

  if (return_old)
    cur->next = new_unary(ND_EXPR_STMT, new_var_node(old, tok), tok);
  else
    cur->next = new_unary(ND_EXPR_STMT, new_var_node(new, tok), tok);

  Node *node = new_node(ND_STMT_EXPR, tok);
  node->body = head.next;
  return node;
}

static Node *to_assign(Node *binary) {
  add_type(binary->lhs);

  if (binary->lhs->ty->is_atomic)
    return atomic_op(binary, false);

  binary->arith_kind = binary->kind;
  binary->kind = ND_ARITH_ASSIGN;
  binary->ty = binary->lhs->ty;
  return binary;
}

// assign    = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//           | "<<=" | ">>="
static Node *assign(Token **rest, Token *tok) {
  Node *node = conditional(&tok, tok);
  add_type(node);

  // Convert A = B to (tmp = B, atomic_exchange(&A, tmp), tmp)
  if (equal(tok, "=") && node->ty->is_atomic) {
    Node *rhs = assign(rest, tok->next);
    add_type(rhs);
    Obj *tmp = new_lvar(NULL, rhs->ty);
    Node *expr = new_binary(ND_ASSIGN, new_var_node(tmp, tok), rhs, tok);
    chain_expr(&expr, new_binary(ND_EXCH, new_unary(ND_ADDR, node, tok),
                                          new_var_node(tmp, tok),
                                          tok));
    chain_expr(&expr, new_var_node(tmp, tok));
    return expr;
  }

  if (equal(tok, "="))
    return new_binary(ND_ASSIGN, node, assign(rest, tok->next), tok);

  if (equal(tok, "+="))
    return to_assign(new_add(node, assign(rest, tok->next), tok));

  if (equal(tok, "-="))
    return to_assign(new_sub(node, assign(rest, tok->next), tok));

  if (equal(tok, "*="))
    return to_assign(new_binary(ND_MUL, node, assign(rest, tok->next), tok));

  if (equal(tok, "/="))
    return to_assign(new_binary(ND_DIV, node, assign(rest, tok->next), tok));

  if (equal(tok, "%="))
    return to_assign(new_binary(ND_MOD, node, assign(rest, tok->next), tok));

  if (equal(tok, "&="))
    return to_assign(new_binary(ND_BITAND, node, assign(rest, tok->next), tok));

  if (equal(tok, "|="))
    return to_assign(new_binary(ND_BITOR, node, assign(rest, tok->next), tok));

  if (equal(tok, "^="))
    return to_assign(new_binary(ND_BITXOR, node, assign(rest, tok->next), tok));

  if (equal(tok, "<<="))
    return to_assign(new_binary(ND_SHL, node, assign(rest, tok->next), tok));

  if (equal(tok, ">>=")) {
    if (node->ty->is_unsigned)
      return to_assign(new_binary(ND_SHR, node, assign(rest, tok->next), tok));
    else
      return to_assign(new_binary(ND_SAR, node, assign(rest, tok->next), tok));
  }

  *rest = tok;
  return node;
}

// conditional = logor ("?" expr? ":" conditional)?
static Node *conditional(Token **rest, Token *tok) {
  Node *cond = log_or(&tok, tok);

  if (!equal(tok, "?")) {
    *rest = tok;
    return cond;
  }

  if (equal(tok->next, ":")) {
    // [GNU] Compile `a ?: b` as `tmp = a, tmp ? tmp : b`.
    add_type(cond);

    int64_t val;
    Node n = *cond;
    if (!is_const_expr(cond_cast(&n), &val)) {
      enter_tmp_scope();
      Obj *var = new_lvar(NULL, cond->ty);
      Node *lhs = new_binary(ND_ASSIGN, new_var_node(var, tok), cond, tok);
      Node *rhs = new_node(ND_COND, tok);
      rhs->cond = cond_cast(new_var_node(var, tok));
      rhs->then = new_var_node(var, tok);
      rhs->els = conditional(rest, tok->next->next);
      leave_scope();
      return new_binary(ND_CHAIN, lhs, rhs, tok);
    }
    Node *node = new_node(ND_COND, tok);
    node->cond = new_boolean(!!val, cond->tok);
    node->then = cond;
    node->els = conditional(rest, tok->next->next);
    return node;
  }

  Node *node = new_node(ND_COND, tok);
  node->cond = cond_cast(cond);
  node->then = expr(&tok, tok->next);
  tok = skip(tok, ":");
  node->els = conditional(rest, tok);
  return node;
}

// logor = logand ("||" logand)*
static Node *log_or(Token **rest, Token *tok) {
  Node *node = log_and(&tok, tok);
  while (equal(tok, "||")) {
    Token *start = tok;
    node = new_binary(ND_LOGOR, cond_cast(node), cond_cast(log_and(&tok, tok->next)), start);
  }
  *rest = tok;
  return node;
}

// logand = bitor ("&&" bitor)*
static Node *log_and(Token **rest, Token *tok) {
  Node *node = bit_or(&tok, tok);
  while (equal(tok, "&&")) {
    Token *start = tok;
    node = new_binary(ND_LOGAND, cond_cast(node), cond_cast(bit_or(&tok, tok->next)), start);
  }
  *rest = tok;
  return node;
}

// bitor = bitxor ("|" bitxor)*
static Node *bit_or(Token **rest, Token *tok) {
  Node *node = bit_xor(&tok, tok);
  while (equal(tok, "|")) {
    Token *start = tok;
    node = new_binary(ND_BITOR, node, bit_xor(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// bitxor = bitand ("^" bitand)*
static Node *bit_xor(Token **rest, Token *tok) {
  Node *node = bit_and(&tok, tok);
  while (equal(tok, "^")) {
    Token *start = tok;
    node = new_binary(ND_BITXOR, node, bit_and(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// bitand = equality ("&" equality)*
static Node *bit_and(Token **rest, Token *tok) {
  Node *node = equality(&tok, tok);
  while (equal(tok, "&")) {
    Token *start = tok;
    node = new_binary(ND_BITAND, node, equality(&tok, tok->next), start);
  }
  *rest = tok;
  return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **rest, Token *tok) {
  Node *node = relational(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (equal(tok, "==")) {
      node = new_binary(ND_EQ, node, relational(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "!=")) {
      node = new_binary(ND_NE, node, relational(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// relational = shift ("<" shift | "<=" shift | ">" shift | ">=" shift)*
static Node *relational(Token **rest, Token *tok) {
  Node *node = shift(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (equal(tok, "<")) {
      node = new_binary(ND_LT, node, shift(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "<=")) {
      node = new_binary(ND_LE, node, shift(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, ">")) {
      node = new_binary(ND_GT, node, shift(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, ">=")) {
      node = new_binary(ND_GE, node, shift(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// shift = add ("<<" add | ">>" add)*
static Node *shift(Token **rest, Token *tok) {
  Node *node = add(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (equal(tok, "<<")) {
      node = new_binary(ND_SHL, node, add(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, ">>")) {
      add_type(node);
      if (node->ty->is_unsigned)
        node = new_binary(ND_SHR, node, add(&tok, tok->next), start);
      else
        node = new_binary(ND_SAR, node, add(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// In C, `+` operator is overloaded to perform the pointer arithmetic.
// If p is a pointer, p+n adds not n but sizeof(*p)*n to the value of p,
// so that p+n points to the location n elements (not bytes) ahead of p.
// In other words, we need to scale an integer value before adding to a
// pointer value. This function takes care of the scaling.
static Node *new_add(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num + num
  if (is_numeric(lhs->ty) && is_numeric(rhs->ty))
    return new_binary(ND_ADD, lhs, rhs, tok);

  ptr_convert(&lhs);
  ptr_convert(&rhs);

  Node **ofs = is_integer(lhs->ty) ? &lhs : is_integer(rhs->ty) ? &rhs : NULL;
  Node *ptr = lhs->ty->base ? lhs : rhs->ty->base ? rhs : NULL;

  if (ptr && ofs) {
    if (ptr->ty->base->kind == TY_VLA)
      *ofs = new_binary(ND_MUL, *ofs, vla_size(ptr->ty->base, tok), tok);
    else
      *ofs = new_binary(ND_MUL, *ofs, new_intptr_t(ptr->ty->base->size, tok), tok);

    return new_binary(ND_ADD, lhs, rhs, tok);
  }

  error_tok(tok, "invalid operands");
}

// Like `+`, `-` is overloaded for the pointer type.
static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num - num
  if (is_numeric(lhs->ty) && is_numeric(rhs->ty))
    return new_binary(ND_SUB, lhs, rhs, tok);

  ptr_convert(&lhs);
  ptr_convert(&rhs);

  // ptr - num
  if (lhs->ty->base && is_integer(rhs->ty)) {
    if (lhs->ty->base->kind == TY_VLA)
      rhs = new_binary(ND_MUL, rhs, vla_size(lhs->ty->base, tok), tok);
    else
      rhs = new_binary(ND_MUL, rhs, new_intptr_t(lhs->ty->base->size, tok), tok);

    return new_binary(ND_SUB, lhs, rhs, tok);
  }

  // ptr - ptr, which returns how many elements are between the two.
  if (lhs->ty->base && rhs->ty->base) {
    int sz = lhs->ty->base->size;
    Node *node = new_binary(ND_SUB, new_cast(lhs, ty_intptr_t), new_cast(rhs, ty_intptr_t), tok);
    return new_cast(new_binary(ND_DIV, node, new_num(sz, tok), tok), ty_ptrdiff_t);
  }

  error_tok(tok, "invalid operands");
}

// add = mul ("+" mul | "-" mul)*
static Node *add(Token **rest, Token *tok) {
  Node *node = mul(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (equal(tok, "+")) {
      node = new_add(node, mul(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "-")) {
      node = new_sub(node, mul(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// mul = cast ("*" cast | "/" cast | "%" cast)*
static Node *mul(Token **rest, Token *tok) {
  Node *node = cast(&tok, tok);

  for (;;) {
    Token *start = tok;

    if (equal(tok, "*")) {
      node = new_binary(ND_MUL, node, cast(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "/")) {
      node = new_binary(ND_DIV, node, cast(&tok, tok->next), start);
      continue;
    }

    if (equal(tok, "%")) {
      node = new_binary(ND_MOD, node, cast(&tok, tok->next), start);
      continue;
    }

    *rest = tok;
    return node;
  }
}

// cast = "(" type-name ")" cast | unary
static Node *cast(Token **rest, Token *tok) {
  Token *start = tok;
  Type *ty;
  if (is_typename_paren(&tok, tok, &ty)) {
    Node *node = new_cast(cast(rest, tok), ty);
    node->tok = start;
    return node;
  }
  return unary(rest, tok);
}

// unary = ("+" | "-" | "*" | "&" | "!" | "~") cast
//       | ("++" | "--") unary
//       | "&&" ident
//       | postfix
static Node *unary(Token **rest, Token *tok) {
  if (equal(tok, "+"))
    return new_unary(ND_POS, cast(rest, tok->next), tok);

  if (equal(tok, "-"))
    return new_unary(ND_NEG, cast(rest, tok->next), tok);

  if (equal(tok, "&")) {
    Node *lhs = cast(rest, tok->next);
    add_type(lhs);
    if (is_bitfield(lhs))
      error_tok(tok, "cannot take address of bitfield");
    return new_unary(ND_ADDR, lhs, tok);
  }

  if (equal(tok, "*")) {
    // [https://www.sigbus.info/n1570#6.5.3.2p4] This is an oddity
    // in the C spec, but dereferencing a function shouldn't do
    // anything. If foo is a function, `*foo`, `**foo` or `*****foo`
    // are all equivalent to just `foo`.
    Node *node = cast(rest, tok->next);
    add_type(node);
    if (node->ty->kind == TY_FUNC)
      return node;

    Type *ty = node->ty;
    node = new_unary(ND_DEREF, node, tok);

    if (is_array(ty))
      apply_cv_qualifier(node, ty);
    return node;
  }

  if (equal(tok, "!"))
    return new_unary(ND_NOT, cond_cast(cast(rest, tok->next)), tok);

  if (equal(tok, "~"))
    return new_unary(ND_BITNOT, cast(rest, tok->next), tok);

  // Read ++i as i+=1
  if (equal(tok, "++"))
    return to_assign(new_add(unary(rest, tok->next), new_num(1, tok), tok));

  // Read --i as i-=1
  if (equal(tok, "--"))
    return to_assign(new_sub(unary(rest, tok->next), new_num(1, tok), tok));

  // [GNU] labels-as-values
  if (equal(tok, "&&")) {
    Node *node = new_node(ND_LABEL_VAL, tok);
    node->labels = ident_tok(rest, tok->next);
    node->goto_next = gotos;
    gotos = node;
    dont_dealloc_vla = true;
    return node;
  }

  Node *node = primary(&tok, tok);
  return postfix(node, rest, tok);
}

// struct-members = (declspec declarator (","  declarator)* ";")*
static void struct_members(Token **rest, Token *tok, Type *ty) {
  Member head = {0};
  Member *cur = &head;

  while (!equal(tok, "}")) {
    if (equal(tok, "_Static_assert") || equal_kw(tok, "static_assert")) {
      static_assertion(&tok, tok->next);
      continue;
    }

    VarAttr attr = {0};
    Type *basety = declspec(&tok, tok, &attr);

    // Anonymous struct member
    if (equal(tok, ";") &&
      (basety->kind == TY_STRUCT || basety->kind == TY_UNION)) {
      Member *mem = ast_arena_calloc(sizeof(Member));
      mem->ty = basety;

      tok = tok->next;
      cur = cur->next = mem;
      continue;
    }

    // Regular struct members
    bool first = true;
    for (; comma_list(&tok, &tok, ";", !first); first = false) {
      Member *mem = ast_arena_calloc(sizeof(Member));
      mem->alt_align = attr.align;
      mem->ty = declarator2(&tok, tok, basety, &mem->name, &mem->alt_align);
      if (mem->name && !current_fn)
        mem->name->is_live = true;

      for (Type *t = mem->ty; t; t = t->base)
        if (t->kind == TY_VLA)
          error_tok(tok, "members cannot be of variably-modified type");

      if (consume(&tok, tok, ":")) {
        mem->is_bitfield = true;
        mem->bit_width = const_expr(&tok, tok);
        if (mem->bit_width < 0)
          error_tok(tok, "bit-field with negative width");
        attr_aligned(tok, &mem->alt_align, TK_ATTR);
      }
      cur = cur->next = mem;
    }
  }

  // If the last element is an array of incomplete type, it's
  // called a "flexible array member". It should behave as if
  // if were a zero-sized array.
  if (cur != &head && cur->ty->kind == TY_ARRAY && cur->ty->array_len < 0) {
    if (ty->kind != TY_STRUCT)
      error_tok(tok, "flexible array member not allowed in union");
    cur->ty = array_of(cur->ty->base, 0);
    ty->is_flexible = true;
  }

  *rest = tok->next;
  ty->members = head.next;
}

// struct-union-decl = attribute? ident? ("{" struct-members)?
static Type *struct_union_decl(Token **rest, Token *tok, TypeKind kind) {
  Type *ty = new_type(kind, -1, 1);

  bool_attr(tok, TK_ATTR, "packed", &ty->is_packed);
  bool_attr(tok, TK_BATTR, "packed", &ty->is_packed);

  int alt_align = 0;
  attr_aligned(tok, &alt_align, TK_ATTR);
  attr_aligned(tok, &alt_align, TK_BATTR);

  // Read a tag.
  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !equal(tok, "{")) {
    *rest = tok;

    Type *ty2 = find_tag(tag);
    if (ty2)
      return ty2;

    push_tag_scope(tag, ty);
    return ty;
  }

  tok = skip(tok, "{");

  // Construct a struct object.
  struct_members(&tok, tok, ty);

  attr_aligned(tok, &alt_align, TK_ATTR);
  bool_attr(tok, TK_ATTR, "packed", &ty->is_packed);
  *rest = tok;

  if (kind == TY_STRUCT)
    ty = struct_decl(ty, alt_align);
  else
    ty = union_decl(ty, alt_align);

  if (!tag)
    return ty;

  ty->tag = tag;
  tag->is_live = true;

  Type *ty2 = hashmap_get2(&scope->tags, tag->loc, tag->len);
  if (ty2) {
    if (ty2->size < 0) {
      for (Type *t = ty2; t; t = t->decl_next) {
        t->size = ty->size;
        t->align = MAX(t->align, ty->align);
        t->members = ty->members;
        t->is_flexible = ty->is_flexible;
        t->is_packed = ty->is_packed;
        t->origin = ty;
        t->tag = tag;
      }
      return ty2;
    }
    if (!(opt_std >= STD_C23 && is_compatible(ty, ty2)))
      error_tok(tag, "tag redeclaration");
    return ty2;
  }
  push_tag_scope(tag, ty);
  return ty;
}

static Type *struct_decl(Type *ty, int alt_align) {
  int bits = 0;
  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (!mem->is_bitfield || mem->name) {
      if (!ty->is_packed)
        alt_align = MAX(alt_align, mem->ty->align);
      alt_align = MAX(alt_align, mem->alt_align);
    }
    if (mem->alt_align)
      if (mem->alt_align > mem->ty->align ||
        (mem->ty->kind != TY_STRUCT && mem->ty->kind != TY_UNION))
        bits = align_to(bits, mem->alt_align * 8);

    if (mem->is_bitfield) {
      if (mem->bit_width == 0) {
        bits = align_to(bits, mem->ty->size * 8);
        continue;
      }
      int sz = mem->ty->size;
      if (!ty->is_packed)
        if (bits / (sz * 8) != (bits + mem->bit_width - 1) / (sz * 8))
          bits = align_to(bits, sz * 8);

      mem->offset = align_down(bits / 8, sz);
      mem->bit_offset = bits % (sz * 8);
      bits += mem->bit_width;
      continue;
    }
    if (ty->is_packed)
      bits = align_to(bits, 8);
    else
      bits = align_to(bits, mem->ty->align * 8);

    mem->offset = bits / 8;
    bits += mem->ty->size * 8;
  }
  ty->size = MAX(ty->size, 0);

  if (alt_align)
    ty->align = alt_align;
  if (!alt_align && ty->is_packed)
    ty->size = align_to(bits, 8) / 8;
  else
    ty->size = align_to(bits, ty->align * 8) / 8;
  return ty;
}

static Type *union_decl(Type *ty, int alt_align) {
  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (!mem->is_bitfield || mem->name) {
      if (!ty->is_packed)
        alt_align = MAX(alt_align, mem->ty->align);
      alt_align = MAX(alt_align, mem->alt_align);
    }
    int sz;
    if (mem->is_bitfield)
      sz = align_to(mem->bit_width, 8) / 8;
    else
      sz = mem->ty->size;
    ty->size = MAX(ty->size, sz);
  }
  ty->size = MAX(ty->size, 0);

  if (alt_align)
    ty->align = alt_align;
  ty->size = align_to(ty->size, ty->align);
  return ty;
}

// Find a struct member by name.
static Member *get_struct_member(Type *ty, Token *tok) {
  for (Member *mem = ty->members; mem; mem = mem->next) {
    // Anonymous struct member
    if ((mem->ty->kind == TY_STRUCT || mem->ty->kind == TY_UNION) &&
        !mem->name && get_struct_member(mem->ty, tok))
      return mem;

    // Regular struct member
    if (mem->name && (mem->name->len == tok->len) &&
        !strncmp(mem->name->loc, tok->loc, tok->len))
      return mem;
  }
  return NULL;
}

// Create a node representing a struct member access, such as foo.bar
// where foo is a struct and bar is a member name.
//
// C has a feature called "anonymous struct" which allows a struct to
// have another unnamed struct as a member like this:
//
//   struct { struct { int a; }; int b; } x;
//
// The members of an anonymous struct belong to the outer struct's
// member namespace. Therefore, in the above example, you can access
// member "a" of the anonymous struct as "x.a".
//
// This function takes care of anonymous structs.
static Node *struct_ref(Node *node, Token *tok) {
  add_type(node);
  if (node->ty->kind != TY_STRUCT && node->ty->kind != TY_UNION)
    error_tok(node->tok, "not a struct nor a union");

  Type *ty = node->ty;

  for (;;) {
    Member *mem = get_struct_member(ty, tok);
    if (!mem)
      error_tok(tok, "no such member");
    node = new_unary(ND_MEMBER, node, tok);
    node->member = mem;
    apply_cv_qualifier(node, ty);

    if (mem->name)
      break;
    ty = mem->ty;
  }
  return node;
}

static Node *new_inc_dec(Node *node, Token *tok, int addend) {
  add_type(node);

  if (node->ty->is_atomic)
    return atomic_op(new_add(node, new_num(addend, tok), tok), true);

  if (opt_optimize && node->ty->kind != TY_BOOL && !is_bitfield(node) && !is_flonum(node->ty)) {
    Type *ty = node->ty;
    node = new_add(node, new_num(addend, tok), tok);
    node = to_assign(node);
    node = new_add(node, new_num(-addend, tok), tok);
    return new_cast(node, ty);
  }

  node = new_add(node, new_num(addend, tok), tok);
  node->kind = ND_POST_INCDEC;
  node->ty = node->lhs->ty;
  return node;
}

// postfix = ident "(" func-args ")" postfix-tail*
//         | primary postfix-tail*
//
// postfix-tail = "[" expr "]"
//              | "(" func-args ")"
//              | "." ident
//              | "->" ident
//              | "++"
//              | "--"
static Node *postfix(Node *node, Token **rest, Token *tok) {
  for (;;) {
    if (equal(tok, "(")) {
      node = funcall(&tok, tok->next, node);
      continue;
    }

    if (equal(tok, "[")) {
      // x[y] is short for *(x+y)
      Token *start = tok;
      Node *idx = expr(&tok, tok->next);
      tok = skip(tok, "]");

      add_type(node);
      Type *ty = node->ty;
      node = new_unary(ND_DEREF, new_add(node, idx, start), start);

      if (is_array(ty))
        apply_cv_qualifier(node, ty);
      continue;
    }

    if (equal(tok, ".")) {
      node = struct_ref(node, tok->next);
      tok = tok->next->next;
      continue;
    }

    if (equal(tok, "->")) {
      // x->y is short for (*x).y
      add_type(node);
      Type *ty = node->ty;
      node = new_unary(ND_DEREF, node, tok);
      node = struct_ref(node, tok->next);

      if (is_array(ty))
        apply_cv_qualifier(node, ty);
      tok = tok->next->next;
      continue;
    }

    if (equal(tok, "++")) {
      node = new_inc_dec(node, tok, 1);
      tok = tok->next;
      continue;
    }

    if (equal(tok, "--")) {
      node = new_inc_dec(node, tok, -1);
      tok = tok->next;
      continue;
    }

    *rest = tok;
    return node;
  }
}

// funcall = (assign ("," assign)*)? ")"
static Node *funcall(Token **rest, Token *tok, Node *fn) {
  add_type(fn);

  if (fn->ty->kind != TY_FUNC &&
      (fn->ty->kind != TY_PTR || fn->ty->base->kind != TY_FUNC))
    error_tok(fn->tok, "not a function");

  if (fn->kind == ND_VAR && fn->var->returns_twice)
    current_fn->dont_reuse_stk = true;

  Type *ty = (fn->ty->kind == TY_FUNC) ? fn->ty : fn->ty->base;
  Obj *param = ty->is_oldstyle ? NULL : ty->param_list;

  Obj head = {0};
  Obj *cur = &head;

  enter_tmp_scope();

  while (comma_list(rest, &tok, ")", cur != &head)) {
    Node *arg = assign(&tok, tok);
    if (param) {
      arg = assign_cast(param->ty, arg);

      param = param->param_next;
    } else {
      if (!ty->is_variadic && !ty->is_oldstyle)
        error_tok(tok, "too many arguments");

      add_type(arg);
      if (is_integer(arg->ty) && arg->ty->size < 4)
        arg = new_cast(arg, ty_int);
      else if (arg->ty->kind == TY_FLOAT)
        arg = new_cast(arg, ty_double);
      else
        ptr_convert(&arg);
    }
    cur = cur->param_next = new_var(NULL, arg->ty, ast_arena_calloc(sizeof(Obj)));
    cur->arg_expr = arg;
  }
  if (param)
    error_tok(tok, "too few arguments");

  Node *node = new_unary(ND_FUNCALL, fn, tok);
  node->ty = ty->return_ty;
  node->args = head.param_next;

  prepare_funcall(node, scope);
  leave_scope();

  // If a function returns a struct, it is caller's responsibility
  // to allocate a space for the return value.
  if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION ||
    (node->ty->kind == TY_BITINT && bitint_rtn_need_copy(node->ty->bit_cnt)))
    node->ret_buffer = new_lvar(NULL, node->ty);
  return node;
}

// generic-selection = "(" assign "," generic-assoc ("," generic-assoc)* ")"
//
// generic-assoc = type-name ":" assign
//               | "default" ":" assign
static Node *generic_selection(Token **rest, Token *tok) {
  Token *start = tok;
  tok = skip(tok, "(");

  Type *t1;
  if (is_typename(tok)) {
    t1 = typename(&tok, tok);
  } else {
    Node *ctrl = assign(&tok, tok);
    add_type(ctrl);
    t1 = unqual(ptr_decay(ctrl->ty));
  }
  Node *ret = NULL;
  Node *def = NULL;

  while (comma_list(rest, &tok, ")", true)) {
    if (equal(tok, "default")) {
      tok = skip(tok->next, ":");
      def = assign(&tok, tok);
      continue;
    }

    Type *t2 = typename(&tok, tok);
    if (t2->kind == TY_FUNC)
      error_tok(tok, "association has function type");

    Node *node = assign(&tok, skip(tok, ":"));
    if (is_compatible2(t1, t2)) {
      if (ret) {
        warn_tok(ret->tok, "ambiguous _Generic selection");
        error_tok(node->tok, "with this option");
      }
      ret = node;
    }
  }
  if (!ret)
    ret = def;
  if (!ret)
    error_tok(start, "controlling expression type not compatible with"
              " any generic association type");
  return ret;
}

static Node *checked_arith(Token **rest, Token *tok, NodeKind kind) {
  Node *node = new_node(ND_CKD_ARITH, tok);
  node->arith_kind = kind;
  tok = skip(tok->next, "(");
  node->lhs = assign(&tok, tok);
  tok = skip(tok, ",");
  node->rhs = assign(&tok, tok);
  tok = skip(tok, ",");
  node->inc = assign(&tok, tok);
  *rest = skip(tok, ")");
  add_type(node);

  Token *bad_tok = NULL;
  if (node->inc->ty->kind != TY_PTR || node->inc->ty->base->kind == TY_BOOL ||
    !is_int_class(node->inc->ty->base))
    bad_tok = node->inc->tok;
  else if (!is_int_class(node->lhs->ty))
    bad_tok = node->lhs->tok;
  else if (!is_int_class(node->rhs->ty))
    bad_tok = node->rhs->tok;
  if (bad_tok)
    error_tok(bad_tok, "operand invalid for integer overflow arithmetic");

  return node;
}

static Node *compound_literal(Token **rest, Token *tok) {
  Token *start = tok;
  VarAttr attr = {0};
  Type *ty = declspec(&tok, tok->next, &attr);
  ty = declarator(&tok, tok, ty, NULL);
  tok = skip(tok, ")");

  if (ty->kind == TY_VLA)
    error_tok(tok, "compound literals cannot be VLA");

  if (is_constant_context() || attr.is_static) {
    bool set_ctx = !is_constant_context();
    if (set_ctx)
      is_global_init_context = true;

    Obj *var = new_anon_gvar(ty);
    var->is_tls = attr.is_tls;
    var->is_compound_lit = true;

    if (attr.is_constexpr)
      constexpr_initializer(rest, tok, var, var);
    else
      gvar_initializer(rest, tok, var);

    if (set_ctx)
      is_global_init_context = false;
    return new_var_node(var, start);
  }

  Scope *sc = scope;
  while (sc->is_temporary)
    sc = sc->parent;

  Obj *var = new_var(NULL, ty, ast_arena_calloc(sizeof(Obj)));
  var->is_compound_lit = true;
  var->is_local = true;
  var->next = sc->locals;
  sc->locals = var;
  Node *init;
  if (attr.is_constexpr) {
    Obj *init_var = new_anon_gvar(ty);
    is_global_init_context = true;

    constexpr_initializer(&tok, tok, init_var, var);

    is_global_init_context = false;
    init = new_binary(ND_ASSIGN, new_var_node(var, tok), new_var_node(init_var, tok), tok);
  } else {
    init = lvar_initializer(&tok, tok, var);
  }
  *rest = tok;
  return new_binary(ND_CHAIN, init, new_var_node(var, tok), start);
}

// primary = "(" "{" stmt+ "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" type-name ")"
//         | "sizeof" unary
//         | "_Alignof" "(" type-name ")"
//         | "_Alignof" unary
//         | "_Generic" generic-selection
//         | "__builtin_types_compatible_p" "(" type-name, type-name, ")"
//         | ident
//         | str
//         | num
static Node *primary(Token **rest, Token *tok) {
  Token *start = tok;

  if (equal(tok, "(") && is_typename(tok->next))
    return compound_literal(rest, tok);

  if (equal(tok, "(") && equal(tok->next, "{")) {
    if (is_constant_context())
      error_tok(tok, "statement expresssion in constant context");

    Node *node = compound_stmt(&tok, tok->next->next, ND_STMT_EXPR);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "(")) {
    Node *node = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "sizeof")) {
    Type *ty;
    if (!is_typename_paren(rest, tok->next, &ty)) {
      Node *node = unary(rest, tok->next);
      add_type(node);
      ty = node->ty;
    }
    if (ty->kind == TY_VLA)
      return vla_size(ty, tok);

    if (ty->size < 0)
      error_tok(tok, "sizeof applied to incomplete type");

    if (ty->kind == TY_STRUCT && ty->is_flexible) {
      Member *mem = ty->members;
      while (mem->next)
        mem = mem->next;
      if (mem->ty->kind == TY_ARRAY)
        return new_size_t((ty->size - mem->ty->size), start);
    }
    return new_size_t(ty->size, start);
  }

  if (equal(tok, "_Alignof") || equal_kw(tok, "alignof")) {
    Type *ty;
    if (!is_typename_paren(rest, tok->next, &ty)) {
      Node *node = unary(rest, tok->next);
      switch (node->kind) {
      case ND_MEMBER:
        return new_size_t(MAX(node->member->ty->align, node->member->alt_align), tok);
      case ND_VAR:
        return new_size_t(node->var->align, tok);
      }
      add_type(node);
      ty = node->ty;
    }
    while (is_array(ty))
      ty = ty->base;
    return new_size_t(ty->align, tok);
  }

  if (equal(tok, "_Countof")) {
    Type *ty;
    if (!is_typename_paren(rest, tok->next, &ty)) {
      Node *node = unary(rest, tok->next);
      add_type(node);
      ty = node->ty;
    }
    if (ty->kind == TY_VLA)
      return vla_count(ty, start, false);
    if (ty->kind == TY_ARRAY) {
      if (ty->size < 0)
        error_tok(tok, "countof applied to incomplete array");
      return new_size_t(ty->array_len, start);
    }
    error_tok(tok, "countof applied to non-array type");
  }

  if (equal(tok, "_Generic"))
    return generic_selection(rest, tok->next);

  if (equal(tok, "__builtin_alloca")) {
    Node *node = new_node(ND_ALLOCA, tok);
    tok = skip(tok->next, "(");
    node->lhs = assign(&tok, tok);
    *rest = skip(tok, ")");
    node->ty = pointer_to(ty_void);
    return node;
  }

  if (equal(tok, "__builtin_return_address")) {
    Node *node = new_node(ND_RTN_ADDR, tok);
    node->ty = pointer_to(ty_void);
    tok = skip(tok->next, "(");
    if (!is_const_expr(expr(&tok, tok), &node->val))
      error_tok(tok, "expected integer constant expression");
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_extract_return_addr")) {
    tok = skip(tok->next, "(");
    Node *node = assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (!strncmp(tok->loc, "__builtin_atomic_fetch_", 23)) {
    Token *start = tok;
    tok = skip(tok->next, "(");
    Node *obj = new_unary(ND_DEREF, assign(&tok, tok), start);
    tok = skip(tok, ",");
    Node *val = assign(&tok, tok);
    *rest = skip(tok, ")");

    Node *binary;
    char *loc = start->loc + 23;
    int len = start->len - 23;

    if (!strncmp("add", loc, len))
      binary = new_add(obj, val, start);
    else if (!strncmp("sub", loc, len))
      binary = new_sub(obj, val, start);
    else if (!strncmp("and", loc, len))
      binary = new_binary(ND_BITAND, obj, val, start);
    else if (!strncmp("or", loc, len))
      binary = new_binary(ND_BITOR, obj, val, start);
    else if (!strncmp("xor", loc, len))
      binary = new_binary(ND_BITXOR, obj, val, start);
    else
      error_tok(start, "unsupported atomic fetch op");

    add_type(binary->lhs);
    add_type(binary->rhs);
    return atomic_op(binary, true);
  }

  if (equal(tok, "__builtin_constant_p")) {
    Node *node = new_node(ND_NUM, tok);
    tok = skip(tok->next, "(");
    node->val = is_const_expr(expr(&tok, tok), NULL);
    node->ty = ty_int;
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_expect")) {
    tok = skip(tok->next, "(");
    Node *node = new_cast(assign(&tok, tok), ty_long);
    tok = skip(tok, ",");
    assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_offsetof")) {
    tok = skip(tok->next, "(");
    Type *ty = typename(&tok, tok);
    Node *node = new_unary(ND_DEREF, new_cast(new_num(0, tok), pointer_to(ty)), tok);
    tok = skip(tok, ",");
    Token dot = {.kind = TK_PUNCT, .loc = ".", .len = 1, .next = tok, .file = tok->file};
    node = postfix(node, &tok, &dot);
    add_type(node);
    *rest = skip(tok, ")");

    int64_t ofs;
    if (eval_non_var_ofs(node, &ofs))
      return new_size_t(ofs, tok);
    return new_cast(new_unary(ND_ADDR, node, tok), ty_size_t);
  }

  if (equal(tok, "__builtin_add_overflow"))
    return checked_arith(rest, tok, ND_ADD);

  if (equal(tok, "__builtin_sub_overflow"))
    return checked_arith(rest, tok, ND_SUB);

  if (equal(tok, "__builtin_mul_overflow"))
    return checked_arith(rest, tok, ND_MUL);

  if (equal(tok, "__builtin_types_compatible_p")) {
    tok = skip(tok->next, "(");
    Type *t1 = typename(&tok, tok);
    tok = skip(tok, ",");
    Type *t2 = typename(&tok, tok);
    *rest = skip(tok, ")");
    return new_num(is_compatible(t1, t2), start);
  }

  if (equal(tok, "__builtin_va_start")) {
    Node *node = new_node(ND_VA_START, tok);
    tok = skip(tok->next, "(");
    node->lhs = conditional(&tok, tok);
    if (equal(tok, ","))
      assign(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_va_copy")) {
    Node *node = new_node(ND_VA_COPY, tok);
    tok = skip(tok->next, "(");
    node->lhs = conditional(&tok, tok);
    tok = skip(tok, ",");
    node->rhs = conditional(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_va_end")) {
    tok = skip(tok->next, "(");
    Node *node = conditional(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_va_arg")) {
    Node *node = new_node(ND_VA_ARG, tok);
    tok = skip(tok->next, "(");

    Node *ap_arg = conditional(&tok, tok);
    add_type(ap_arg);
    node->lhs = ap_arg;
    tok = skip(tok, ",");

    Type *ty = typename(&tok, tok);
    if (va_arg_need_copy(ty))
      node->var = new_lvar(NULL, ty);

    node->ty = pointer_to(ty);
    *rest = skip(tok, ")");
    return new_unary(ND_DEREF, node, tok);
  }

  if (equal(tok, "__builtin_compare_and_swap")) {
    Node *node = new_node(ND_CAS, tok);
    tok = skip(tok->next, "(");
    node->cas_addr = assign(&tok, tok);
    tok = skip(tok, ",");
    node->cas_old = assign(&tok, tok);
    tok = skip(tok, ",");
    node->cas_new = assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_atomic_exchange")) {
    Node *node = new_node(ND_EXCH, tok);
    tok = skip(tok->next, "(");
    node->lhs = assign(&tok, tok);
    tok = skip(tok, ",");
    node->rhs = assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (tok->kind == TK_IDENT) {
    // Variable or enum constant
    VarScope *sc = find_var(tok);
    *rest = tok->next;

    // For "static inline" function
    if (sc && sc->var && sc->var->ty->kind == TY_FUNC) {
      if (current_fn)
        strarray_push(&current_fn->refs, sc->var->name);
      else
        sc->var->is_referenced = true;

      char *name = sc->var->name;
      if (!strcmp(name, "alloca"))
        dont_dealloc_vla = true;
    }

    if (sc) {
      if (sc->var)
        return new_var_node(sc->var, tok);
      if (sc->enum_ty) {
        Node *n = new_num(sc->enum_val, tok);
        n->ty = (sc->enum_ty->is_unspec_enum) ? ty_int : sc->enum_ty;
        return n;
      }
    }

    // [https://www.sigbus.info/n1570#6.4.2.2p1] "__func__" is
    // automatically defined as a local variable containing the
    // current function name.
    // [GNU] __FUNCTION__ is yet another name of __func__.
    if (current_fn && (equal(tok, "__func__") || equal(tok, "__FUNCTION__"))) {
      char *name = current_fn->name;
      VarScope *vsc = ast_arena_calloc(sizeof(VarScope));
      vsc->var = new_static_lvar(array_of(ty_pchar, strlen(name) + 1));
      vsc->var->init_data = name;
      hashmap_put(&current_fn->ty->scopes->vars, "__func__", vsc);
      hashmap_put(&current_fn->ty->scopes->vars, "__FUNCTION__", vsc);
      return new_var_node(vsc->var, tok);
    }

    if (equal(tok->next, "("))
      error_tok(tok, "implicit declaration of a function");
    error_tok(tok, "undefined variable");
  }

  if (tok->kind == TK_STR) {
    Obj *var = new_anon_gvar(tok->ty);
    var->init_data = tok->str;
    *rest = tok->next;
    Node *n = new_var_node(var, tok);
    add_type(n);
    return n;
  }

  if (equal_kw(tok, "false")) {
    *rest = tok->next;
    return new_boolean(0, tok);
  }

  if (equal_kw(tok, "true")) {
    *rest = tok->next;
    return new_boolean(1, tok);
  }

  if (equal_kw(tok, "nullptr")) {
    *rest = tok->next;
    Node *node = new_node(ND_NUM, tok);
    node->ty = ty_nullptr;
    node->tok = tok;
    return node;
  }

  if (tok->kind == TK_INT_NUM || tok->kind == TK_PP_NUM) {
    Node *node = new_node(ND_NUM, tok);
    convert_pp_number(tok, node);
    *rest = tok->next;
    return node;
  }

  error_tok(tok, "expected an expression");
}

static Node *parse_typedef(Token **rest, Token *tok, Type *basety, VarAttr *attr) {
  Node *node = NULL;
  bool first = true;
  for (; comma_list(rest, &tok, ";", !first); first = false) {
    Token *name = NULL;
    int alt_align = attr->align;
    Type *ty = declarator2(&tok, tok, basety, &name, &alt_align);
    if (!name)
      error_tok(tok, "typedef name omitted");

    if (alt_align) {
      ty = new_qualified_type(ty);
      ty->align = alt_align;
    }
    push_scope(get_ident(name))->type_def = ty;
    chain_expr(&node, calc_vla(ty, tok));
  }
  return node;
}

// This function matches gotos or labels-as-values with labels.
//
// We cannot resolve gotos as we parse a function because gotos
// can refer a label that appears later in the function.
// So, we need to do this after we parse the entire function.
static void resolve_goto_labels(void) {
  for (Node *x = gotos; x; x = x->goto_next) {
    Node *dest = NULL;
    for (Node *lbls = labels; lbls && !dest; lbls = lbls->goto_next)
      for (Token *t = lbls->labels; t && !dest; t = t->label_next)
        if (equal_tok(t, x->labels))
          dest = lbls;

    if (!dest)
      error_tok(x->tok->next, "use of undeclared label");

    x->unique_label = dest->unique_label;
    if (!dest->defr_start)
      continue;

    DeferStmt *defr= x->defr_start;
    for (; defr; defr = defr->next)
      if (defr == dest->defr_start)
        break;
    if (!defr)
      error_tok(x->tok->next, "illegal jump");

    x->defr_end = defr;
  }
  gotos = labels = NULL;
}

static Obj *find_func(char *name) {
  Scope *sc = scope;
  while (sc->parent)
    sc = sc->parent;

  VarScope *sc2 = hashmap_get(&sc->vars, name);
  if (sc2 && sc2->var && sc2->var->ty->kind == TY_FUNC)
    return sc2->var;
  return NULL;
}

static void mark_fn_live(Obj *var) {
  if (var->is_live)
    return;
  var->is_live = true;

  for (int i = 0; i < var->refs.len; i++) {
    Obj *fn = find_func(var->refs.data[i]);
    if (fn)
      mark_fn_live(fn);
  }
}

static Obj *func_prototype(Type *ty, VarAttr *attr, Token *name) {
  char *name_str = get_ident(name);

  Obj *fn = find_func(name_str);
  if (!fn) {
    fn = new_gvar(name_str, ty);

    if (attr->is_static || (attr->is_inline && !attr->is_extern))
      fn->is_static = true;

    if (strstr(name_str, "setjmp") || strstr(name_str, "savectx") ||
      strstr(name_str, "vfork") || strstr(name_str, "getcontext"))
      fn->returns_twice = true;
  } else {
    if (!fn->ty->is_oldstyle && !fn->ty->param_list && ty->param_list)
      error_tok(name, "function prototype mismatch");

    if (!fn->is_static && attr->is_static)
      error_tok(name, "static declaration follows a non-static declaration");
  }
  fn->is_inline |= attr->is_inline;
  return fn;
}

static void func_definition(Token **rest, Token *tok, Obj *fn, Type *ty) {
  if (fn->is_definition)
    error_tok(tok, "redefinition of %s", fn->name);
  fn->is_definition = true;
  fn->ty = ty;

  arena_on(&ast_arena);

  current_fn = fn;
  current_defr = NULL;
  fn_use_vla = dont_dealloc_vla = false;

  if (ty->scopes) {
    scope = ty->scopes;
  } else {
    enter_scope();
    ty->scopes = scope;
  }

  fn->body = compound_stmt(rest, tok->next, ND_BLOCK);

  if (ty->pre_calc) {
    Node *calc = new_unary(ND_EXPR_STMT, ty->pre_calc, tok);
    calc->next = fn->body->body;
    fn->body->body = calc;
  }

  if (fn_use_vla && !dont_dealloc_vla &&
    (opt_reuse_stack && !current_fn->dont_reuse_stk))
    fn->dealloc_vla = true;

  leave_scope();
  resolve_goto_labels();
  current_fn = NULL;

  emit_text(fn);
  arena_off(&ast_arena);

  if (fn->only_inline)
    fn->is_definition = false;
}

static void global_declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr) {
  bool first = true;
  for (; comma_list(&tok, &tok, ";", !first); first = false) {
    Token *name = NULL;
    int alt_align = attr->align;
    Type *ty = declarator2(&tok, tok, basety, &name, &alt_align);

    if (ty->kind == TY_FUNC) {
      if (!name)
        error_tok(tok, "function name omitted");

      Obj *fn = func_prototype(ty, attr, name);
      func_attr(fn, attr, name, tok);
      symbol_attr(&tok, tok, fn, attr, name);

      if (first && !scope->parent && equal(tok, "{")) {
        func_definition(rest, tok, fn, ty);
        return;
      }
      continue;
    }

    if (!name)
      error_tok(tok, "variable name omitted");

    bool is_definition = !attr->is_extern;
    if (!is_definition && equal(tok, "="))
      is_definition = true;

    VarScope *sc = find_var(name);
    Obj *var;
    if (sc && sc->var) {
      symbol_attr(&tok, tok, sc->var, attr, name);

      if (!is_definition)
        continue;
      if (sc->var->is_definition && !sc->var->is_tentative)
        continue;
      var = sc->var;
      var->is_tentative = false;
      var->ty = ty;
    } else {
      var = new_gvar(get_ident(name), ty);
      symbol_attr(&tok, tok, var, attr, name);
    }
    var->is_definition = is_definition;
    var->is_static = attr->is_static;
    var->is_tls = attr->is_tls;
    if (alt_align)
      var->align = alt_align;

    if (attr->is_constexpr) {
      constexpr_initializer(&tok, skip(tok, "="), var, var);
      var->is_static = true;
      continue;
    }
    if (equal(tok, "="))
      gvar_initializer(&tok, tok->next, var);
    else if (is_definition)
      var->is_tentative = true;
  }
  *rest = tok;
}

static Token *free_parsed_tok(Token *tok, Token *end) {
  while (tok != end) {
    for (Token *t = tok->attr_next; t;) {
      Token *nxt_attr = t->attr_next;
      while (t) {
        Token *nxt_t = t->next;
        free(t);
        t = nxt_t;
      }
      t = nxt_attr;
    }
    Token *nxt = tok->next;
    if (!tok->is_live)
      free(tok);
    tok = nxt;
  }
  return end;
}

// program = (typedef | function-definition | global-variable)*
Obj *parse(Token *tok) {
  globals = NULL;

  Token *free_head = tok;
  Obj head = {0};
  Obj *cur = &head;
  while (tok->kind != TK_EOF) {
    if (free_alloc)
      free_head = free_parsed_tok(free_head, tok);

    if (equal_kw(tok, "asm") || equal(tok, "__asm") || equal(tok, "__asm__")) {
      cur = cur->next = calloc(1, sizeof(Obj));
      cur->asm_str = str_tok(&tok, skip(tok->next, "("));
      cur->asm_str->is_live = true;
      tok = skip(tok, ")");
      continue;
    }

    arena_on(&node_arena);

    if (equal(tok, "_Static_assert") || equal_kw(tok, "static_assert")) {
      arena_on(&ast_arena);
      Obj *last = globals;

      static_assertion(&tok, tok->next);

      while (globals != last) {
        Obj *tmp = globals;
        globals = globals->next;
        free(tmp);
      }
      arena_off(&ast_arena);
      arena_off(&node_arena);
      continue;
    }

    VarAttr attr = {0};
    Type *basety = declspec(&tok, tok, &attr);

    // Typedef
    if (attr.is_typedef) {
      parse_typedef(&tok, tok, basety, &attr);
      arena_off(&node_arena);
      continue;
    }

    // Global declarations
    global_declaration(&tok, tok, basety, &attr);
    arena_off(&node_arena);
  }

  for (Obj *var = globals; var; var = var->next)
    if (var->is_definition && var->ty->kind == TY_FUNC &&
      (var->is_referenced || !(var->is_static && var->is_inline)))
      mark_fn_live(var);

  cur->next = globals;
  return head.next;
}

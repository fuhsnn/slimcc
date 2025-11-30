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
  int32_t type_def_align;
} VarScope;

typedef enum {
  PCD_MUL,
  PCD_ADD,
  PCD_SHFT,
  PCD_CMP,
  PCD_EQ,
  PCD_BITAND,
  PCD_XOR,
  PCD_BITOR,
  PCD_LOGAND,
  PCD_LOGOR,
} Preced;

enum {
  SC_AUTO      = 1,
  SC_CONSTEXPR = 1 << 1,
  SC_EXTERN    = 1 << 2,
  SC_REGISTER  = 1 << 3,
  SC_STATIC    = 1 << 4,
  SC_THREAD    = 1 << 5,
  SC_TYPEDEF   = 1 << 6,
  SC_ALL       = (1 << 7) - 1,
  SC_NONE      = 0,
};

typedef uint8_t StorageClass;

// Variable attributes such as typedef or extern.
typedef struct {
  StorageClass strg;
  bool is_inline;
  bool is_weak;
  bool is_packed;
  bool is_common;
  bool is_nocommon;
  bool is_gnu_inline;
  bool is_naked;
  bool is_noreturn;
  bool is_returns_twice;
  bool is_used;
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
    INIT_FLEX,
    INIT_FLEX_NESTED,
    INIT_COPY,
    INIT_LIST,
    INIT_STR_ARRAY,
    INIT_EXPR,
    INIT_TOK
  } kind;

  bool is_root;
  bool has_copy;

ANON_UNION_START
    Token *tok;
    Node *expr;

    struct {
      Initializer *data;
      int32_t cnt;
      int32_t union_idx;
    } list;

    struct {
      Obj *var;
      Initializer *init;
    } cpy;
ANON_UNION_END
};

// For local variable initializer.
typedef struct InitDesg InitDesg;
struct InitDesg {
  InitDesg *parent;
  int idx;
  Member *member;
  Obj *var;
};

typedef struct {
  Initializer *init;
  Obj *var;
  Token *end;
  int lvl;
  int init_lvl;
} DesgContext;

struct LocalLabel {
  LocalLabel *next;
  Token *name;
  Node *label;
};

typedef struct {
  Token *end;
  bool is_param;
  bool is_glob;
  bool let_star;
} DeclContext;

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
  DeferStmt *dfr_lvl;
  Token *dfr_ctx;
  Token *labels;
  Node *node;
} *jump_ctx;

struct {
  int *data;
  int capacity;
  int cnt;
} pack_stk;

// Likewise, global variables are accumulated to this list.
static Obj *globals = &(Obj){0};

static Scope *scope = &(Scope){0};

static HashMap symbols;

// Points to the function object the parser is currently parsing.
static Obj *current_fn;

static Obj *current_fnname;

// Lists of all goto statements and labels in the curent function.
static Node *gotos;
static Node *labels;

static DeferStmt *current_defr;

static bool fn_use_vla;
static bool dont_dealloc_vla;
static bool is_global_init_context;
static bool *eval_recover;
static Token *defer_context;

static bool is_typename(Token *tok);
static Type *typename(Token **rest, Token *tok);
static Type *typename2(Token **rest, Token *tok, VarAttr *attr);
static Type *enum_specifier(Token **rest, Token *tok);
static Type *typeof_specifier(Token **rest, Token *tok);
static Type *declarator(Token **rest, Token *tok, Type *ty, Token **name_tok);
static Type *declarator2(Token **rest, Token *tok, Type *ty, Token **name_tok, DeclContext *ctx);
static void list_initializer(Token **rest, Token *tok, Initializer *init, int i);
static void initializer2(Token **rest, Token *tok, Initializer *init);
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var);
static void gvar_initializer(Token **rest, Token *tok, Obj *var);
static void constexpr_initializer(Token **rest, Token *tok, Obj *init_var, Obj *var);
static Node *compound_stmt(Token **rest, Token *tok, NodeKind kind);
static Node *stmt(Token **rest, Token *tok, Token *label_list);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static int64_t align_expr(Token **rest, Token *tok);
static int64_t eval(Node *node);
static int64_t eval2(Node *node, EvalContext *ctx);
static Obj *eval_var(Node *node, int *ofs, bool let_volatile);
static Node *assign(Token **rest, Token *tok);
static long_double_t eval_double(Node *node);
static void eval_fp(Node *node, FPVal *fval);
static uint64_t *eval_bitint(Node *node);
static Node *conditional(Token **rest, Token *tok);
static Node *new_add(Node *lhs, Node *rhs, Token *tok);
static Node *new_sub(Node *lhs, Node *rhs, Token *tok);
static Node *binary(Token **rest, Token *tok, Preced stop);
static Member *get_struct_member(Type *ty, Token *tok);
static Type *struct_union_decl(Token **rest, Token *tok, TypeKind kind);
static Type *struct_decl(Type *ty, int alt_align, int pack_align);
static Type *union_decl(Type *ty, int alt_align, int pack_align);
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
static Node *resolve_local_gotos(void);
static void push_goto(Node *node);

static void enter_scope(void) {
  Scope *sc = ast_arena_calloc(sizeof(Scope));
  sc->parent = scope;
  sc->sibling_next = scope->children;
  scope = scope->children = sc;
}

static void enter_param_scope(void) {
  Scope *sc = ast_arena_calloc(sizeof(Scope));
  sc->parent = scope;
  scope = sc;
}

static void enter_tmp_scope(void) {
  enter_scope();
  scope->is_temporary = true;
}

static bool leave_scope(void) {
  if (current_fn) {
    free(scope->vars.buckets);
    free(scope->tags.buckets);
  }
  bool has_label = scope->has_label;
  scope = scope->parent;
  scope->has_label |= has_label;
  return !has_label;
}

static DeferStmt *new_stmt_scope(void) {
  enter_scope();
  scope->is_stmt = true;
  return current_defr;
}

static Node *leave_stmt_scope(DeferStmt *defr, Node *stmt_node) {
  bool no_label = leave_scope();

  if (stmt_node->kind == ND_RETURN || stmt_node->kind == ND_GOTO)
    current_defr = defr;

  if (defr == current_defr && !stmt_node->next) {
    stmt_node->no_label = no_label;
    return stmt_node;
  }
  Node *blk = new_node(ND_BLOCK, stmt_node->tok);
  blk->dfr_from = current_defr;
  blk->dfr_dest = current_defr = defr;
  blk->blk.body = stmt_node;
  blk->no_label = no_label;
  return blk;
}

static Scope *base_scope(void) {
  Scope *sc = scope;
  while (sc->parent && sc->parent->parent)
    sc = sc->parent;
  return sc;
}

static Scope *decl_scope(void) {
  Scope *sc = scope;
  while (sc->is_temporary || (sc->is_stmt && opt_std == STD_C89))
    sc = sc->parent;
  return sc;
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

static Type *find_tag_in_scope(Token *tok) {
  return hashmap_get2(&decl_scope()->tags, tok->loc, tok->len);
}

static void push_tag_scope(Token *tag, Type *ty) {
  tag->is_live = true;
  ty->tag = tag;
  hashmap_put2(&decl_scope()->tags, tag->loc, tag->len, ty);
}

Obj *get_symbol_var(char *name) {
  return hashmap_get(&symbols, name);
}

static bool less_eq(Type *ty, int64_t lhs, int64_t rhs) {
  if (ty->is_unsigned && ty->size == 8)
    return (uint64_t)lhs <= rhs;
  return lhs <= rhs;
}

bool is_const_var(Obj *var) {
  Type *ty = var->ty;
  for (; ty && ty->kind == TY_ARRAY; ty = ty->base)
    if (ty->qual & Q_CONST)
      return true;
  return ty->qual & Q_CONST;
}

static bool is_int_class(Type *ty) {
  return is_integer(ty) || ty->kind == TY_BITINT;
}

static bool is_vm_ty(Type *ty) {
  Type *t = ty;
  while (t->kind == TY_PTR)
    t = t->base;
  return t->kind == TY_VLA;
}

static bool is_func_def(Token *end_tok) {
  return equal(end_tok, "{") || is_typename(end_tok);
}

bool equal_tok(Token *a, Token *b) {
  return a->len == b->len && !memcmp(a->loc, b->loc, b->len);
}

static bool equal_substr(char *loc, size_t len, char *op) {
  return strlen(op) == len && !memcmp(loc, op, len);
}

static void chk_vla_expr_side_effect(Node *expr) {
  while (expr->kind == ND_DEREF || expr->kind == ND_ADDR)
    expr = expr->m.lhs;
  if (expr->kind != ND_VAR)
    error_tok(expr->tok, "execution of VLA expression not supported");
}

static Node *new_node(NodeKind kind, Token *tok) {
  Node *node = arena_calloc(&node_arena, sizeof(Node));
  node->kind = kind;
  node->tok = tok;
  return node;
}

static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {
  Node *node = new_node(kind, tok);
  node->m.lhs = lhs;
  node->m.rhs = rhs;
  return node;
}

static Node *new_unary(NodeKind kind, Node *expr, Token *tok) {
  Node *node = new_node(kind, tok);
  node->m.lhs = expr;
  return node;
}

static Node *new_num(int64_t val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->num.val = val;
  return node;
}

static Node *new_size_t(int64_t val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->num.val = val;
  node->ty = ty_size_t;
  return node;
}

static Node *base_size(Type *base, Token *tok) {
  if (base->size < 0)
    error_tok(tok, "pointer has incomplete type");
  return new_size_t(base->size, tok);
}

static Node *new_boolean(bool val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->num.val = val;
  node->ty = ty_bool;
  return node;
}

static Node *new_var_node(Obj *var, Token *tok) {
  Node *node = new_node(ND_VAR, tok);
  node->m.var = var;
  return node;
}

static Node *new_label(Token *tok) {
  Node *node = new_node(ND_LABEL, tok);
  node->dfr_from = current_defr;
  scope->has_label = true;
  return node;
}

static Node *new_unknown(Type *ty, Token *tok) {
  Node *node = new_node(ND_UNKNOWN, tok);
  node->ty = ty;
  return node;
}

static bool invalid_cast(Node *node, Type *to) {
  if (is_array(to) || to->kind == TY_FUNC)
    return true;
  if (node->ty->size < 0 && node->ty->kind != TY_ARRAY)
    return true;

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
    if (node->ty->kind == TY_VOID)
      return true;
    if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION)
      return !is_compatible(node->ty, to);
    if (to->kind == TY_STRUCT || to->kind == TY_UNION)
      return true;
  }
  if ((node->ty->base && is_flonum(to)) ||
    (is_flonum(node->ty) && to->base))
    return true;

  return false;
}

Node *new_cast(Node *expr, Type *ty) {
  add_type(expr);
  ty = unqual(ty);

  if (invalid_cast(expr, ty))
    error_tok(expr->tok, "invalid cast");

  if (ty->kind == TY_BOOL) {
    Node *n = expr;
    while (n->kind == ND_CAST && n->ty->size == 8 &&
      (n->ty->kind == TY_PTR || is_integer(n->ty)))
      n = n->m.lhs;

    Obj *var = NULL;
    if (n->kind == ND_ADDR && n->m.lhs->kind == ND_VAR)
      var = n->m.lhs->m.var;
    else if (n->kind == ND_VAR && (is_array(n->ty) || n->ty->kind == TY_FUNC))
      var = n->m.var;

    if (var && !var->is_weak) {
      expr->kind = ND_NUM;
      expr->num.val = 1;
      expr->ty = ty_bool;
      return expr;
    }
  }

  Node tmp_node = {.kind = ND_CAST, .tok = expr->tok, .m.lhs = expr, .ty = ty};
  if (opt_optimize) {
    int64_t val = 0;
    if (is_integer(ty) && is_const_expr(&tmp_node, &val)) {
      expr->kind = ND_NUM;
      expr->num.val = val;
      expr->ty = ty;
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
  ptr_convert(&expr);
  return new_cast(expr, ty_bool);
}

static void apply_cv_qualifier(Node *node, Type *ty2) {
  add_type(node);
  cvqual_type(&node->ty, ty2);
}

static void free_initializers(Initializer *init) {
  if (init->kind == INIT_LIST && init->list.cnt) {
    for (int i = 0; i < init->list.cnt; i++)
      free_initializers(&init->list.data[i]);
    free(init->list.data);
  }
}

static void set_init(Initializer *init, int kind) {
  if (init->kind == kind)
    return;
  free_initializers(init);
  init->kind = kind;
}

static void prepare_array_init(Initializer *init, Type *ty) {
  if (init->kind == INIT_LIST)
    return;

  init->kind = INIT_LIST;
  if (!(init->list.cnt = ty->array_len))
    return;
  init->list.data = calloc(init->list.cnt, sizeof(Initializer));

  for (int i = 0; i < init->list.cnt; i++)
    init->list.data[i].ty = ty->base;
}

static void prepare_struct_init(Initializer *init, Type *ty) {
  if (init->kind == INIT_LIST)
    return;

  int len = 0;
  for (Member *mem = ty->members; mem_iter(&mem); mem = mem->next)
    mem->idx = len++;

  init->kind = INIT_LIST;
  if (!(init->list.cnt = len))
    return;
  init->list.data = calloc(init->list.cnt, sizeof(Initializer));

  for (Member *mem = ty->members; mem_iter(&mem); mem = mem->next) {
    Initializer *child = &init->list.data[mem->idx];

    if (!mem->next && ty->is_flexible) {
      child->ty = mem->ty;
      child->kind = init->is_root ? INIT_FLEX : INIT_FLEX_NESTED;
      break;
    }
    child->ty = mem->ty;
  }
}

static VarScope *push_var_scope(char *key, int keylen, Obj *var) {
  HashEntry *ent = hashmap_get_or_insert(&scope->vars, key, keylen);
  VarScope *vsc = ent->val;
  if (vsc)
    return vsc;
  vsc = ent->val = ast_arena_calloc(sizeof(VarScope));
  vsc->var = var;
  return NULL;
}

static void push_var_name2(char *key, int keylen, Token *tok, Obj *var) {
  VarScope *vsc = push_var_scope(key, keylen, var);
  if (vsc)
    error_tok(tok, "redeclaration of '%.*s'", keylen, key);
}

static void push_var_name(Token *name, Obj *var) {
  push_var_name2(name->loc, name->len, name, var);
}

static void push_gvar_name(Token *name, Obj *var) {
  VarScope *vsc = push_var_scope(name->loc, name->len, var);
  if (vsc && var != vsc->var)
    error_tok(name, "invalid redefinition of '%.*s'", name->len, name->loc);
}

static Obj *alloc_lvar(Type *ty) {
  Obj *var = ast_arena_calloc(sizeof(Obj));
  var->ty = ty;
  return var;
}

static Obj *new_lvar2(Type *ty, Scope *sc) {
  Obj *var = alloc_lvar(ty);
  var->is_local = true;
  var->next = sc->locals;
  sc->locals = var;
  return var;
}

Obj *new_lvar(Type *ty) {
  return new_lvar2(ty, scope);
}

static Obj *alloc_var(char *name, Type *ty) {
  Obj *var = calloc(1, sizeof(Obj));
  var->ty = ty;
  var->name = name;
  return var;
}

static Obj *new_param(char *name, Type *ty) {
  Obj *var = alloc_var(name, ty);
  var->is_local = true;
  var->next = scope->locals;
  scope->locals = var;
  return var;
}

static Obj *new_gvar(char *name, Type *ty) {
  return globals = globals->next = alloc_var(name, ty);
}

char *new_unique_name(void) {
  static int64_t id = 0;
  return format(".L..%"PRIi64, id++);
}

static Obj *new_static_lvar(Type *ty) {
  Obj *var = alloc_lvar(ty);
  var->name = new_unique_name();
  var->is_definition = true;
  var->is_static = true;
  var->is_static_lvar = true;
  var->next = current_fn->static_lvars;
  current_fn->static_lvars = var;
  return var;
}

static Obj *new_anon_gvar(Type *ty) {
  if (current_fn)
    return new_static_lvar(ty);
  Obj *var = new_gvar(new_unique_name(), ty);
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

static VarScope *find_typedef(Token *tok) {
  if (tok->kind == TK_IDENT) {
    VarScope *sc = find_var(tok);
    if (sc && sc->type_def)
      return sc;
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

static void assembler_name(Token **rest, Token *tok, Obj *var) {
  if (tok->kind == TK_asm) {
    char *str = str_tok(&tok, skip(tok->next, "("))->str;
    *rest = skip(tok, ")");
    if (var->asm_name && strcmp(var->asm_name, str))
      error_tok(tok, "conflict of asm name");
    var->asm_name = str;
  }
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

static void pragma_pack_push(void) {
  bool init = !pack_stk.cnt;

  int idx = pack_stk.cnt++;
  if (idx >= pack_stk.capacity) {
    pack_stk.capacity = idx + 2;
    pack_stk.data = realloc(pack_stk.data, sizeof(*pack_stk.data) * pack_stk.capacity);
  }
  pack_stk.data[idx] = idx ? pack_stk.data[idx - 1] : 0;

  if (init)
    pragma_pack_push();
}

static void pragma_pack_pop(Token *tok) {
  if (pack_stk.cnt <= 1) {
    warn_tok(tok, "#pragma pack() stack empty");
    return;
  }
  pack_stk.cnt--;
}

static void pragma_pack_set(int val) {
  if (!pack_stk.cnt) {
    pragma_pack_push();
    pack_stk.cnt = 1;
  }
  pack_stk.data[pack_stk.cnt - 1] = val;
}

static bool pragma_pack(Token **rest, Token *tok) {
  if (is_pragma(&tok, tok) && consume(&tok, tok, "pack")) {
    tok = skip(tok, "(");
    if (equal(tok, "pop")) {
      pragma_pack_pop(tok);
      *rest = skip_line(skip(tok->next, ")"));
      return true;
    }
    if (equal(tok, "push")) {
      pragma_pack_push();
      if (consume(&tok, tok->next, ")")) {
        *rest = skip_line(tok);
        return true;
      }
      tok = skip(tok->next, ",");
    }
    pragma_pack_set(equal(tok, ")") ? 0 : align_expr(&tok, tok));
    *rest = skip_line(skip(tok, ")"));
    return true;
  }
  return false;
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
        int align2 = align_expr(&tok2, tok2);
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
  bool_attr(tok, kind, "packed", &attr->is_packed);
  bool_attr(tok, kind, "common", &attr->is_common);
  bool_attr(tok, kind, "nocommon", &attr->is_nocommon);
  bool_attr(tok, kind, "gnu_inline", &attr->is_gnu_inline);
  bool_attr(tok, kind, "naked", &attr->is_naked);
  bool_attr(tok, kind, "noreturn", &attr->is_noreturn);
  bool_attr(tok, kind, "returns_twice", &attr->is_returns_twice);
  bool_attr(tok, kind, "used", &attr->is_used);
  cdtor_attr(tok, kind, "constructor", &attr->is_ctor, &attr->ctor_prior);
  cdtor_attr(tok, kind, "destructor", &attr->is_dtor, &attr->dtor_prior);
  str_attr(tok, kind, "alias", &attr->alias);
  str_attr(tok, kind, "section", &attr->section);
  str_attr(tok, kind, "visibility", &attr->visibility);
}

#define DeclAttr(Fn, ...) do {             \
    Fn(name, TK_ATTR, __VA_ARGS__);        \
    Fn(name->next, TK_BATTR, __VA_ARGS__); \
    Fn(tok, TK_ATTR, __VA_ARGS__);         \
  } while(0)

static void symbol_attr(Token *name, Token *tok, VarAttr *attr, Obj *var) {
  if (var->asm_name && (attr->strg & SC_REGISTER))
    error_tok(tok, "global register variables not supported");

  apply_str_attr("alias", name, &var->alias_name, attr->alias);
  DeclAttr(str_attr, "alias", &var->alias_name);

  apply_str_attr("section", name, &var->section_name, attr->section);
  DeclAttr(str_attr, "section", &var->section_name);

  apply_str_attr("visibility", name, &var->visibility, attr->visibility);
  DeclAttr(str_attr, "visibility", &var->visibility);

  var->is_used |= attr->is_used;
  DeclAttr(bool_attr, "used", &var->is_used);

  var->is_weak |= attr->is_weak;
  DeclAttr(bool_attr, "weak", &var->is_weak);

  if (var->is_weak && var->is_static)
    error_tok(name, "weak declaration cannot be `static`");

  var->is_common |= attr->is_common;
  DeclAttr(bool_attr, "common", &var->is_common);

  var->is_nocommon |= attr->is_nocommon;
  DeclAttr(bool_attr, "nocommon", &var->is_nocommon);

  if (var->is_common && var->is_nocommon)
    error_tok(name, "conflict of attribute common/nocommon");
}

static void func_attr(Token *name, Token *tok, VarAttr *attr, Obj *fn, bool is_def) {
  apply_cdtor_attr("constructor", name, &fn->is_ctor, &fn->ctor_prior, attr->ctor_prior, attr->is_ctor);
  DeclAttr(cdtor_attr, "constructor", &fn->is_ctor, &fn->ctor_prior);

  apply_cdtor_attr("destructor", name, &fn->is_dtor, &fn->dtor_prior, attr->dtor_prior, attr->is_dtor);
  DeclAttr(cdtor_attr, "destructor", &fn->is_dtor, &fn->dtor_prior);

  if (fn->is_ctor || fn->is_dtor)
    fn->is_used = true;

  fn->is_naked |= attr->is_naked;
  DeclAttr(bool_attr, "naked", &fn->is_naked);

  fn->is_noreturn |= attr->is_noreturn;
  DeclAttr(bool_attr, "noreturn", &fn->is_noreturn);

  fn->returns_twice |= attr->is_returns_twice;
  DeclAttr(bool_attr, "returns_twice", &fn->returns_twice);

  fn->is_gnu_inline |= attr->is_gnu_inline;
  DeclAttr(bool_attr, "gnu_inline", &fn->is_gnu_inline);

  if (!fn->is_static) {
    fn->export_fn |= !(attr->is_inline && !(attr->strg & SC_EXTERN));
    if (is_def)
      fn->export_fn_gnu |= !(attr->is_inline && (attr->strg & SC_EXTERN));
    else
      fn->export_fn_gnu |= (attr->is_inline && !(attr->strg & SC_EXTERN));
  }
}

static void aligned_attr(Token *name, Token *tok, VarAttr *attr, int *align) {
  if (name) {
    attr_aligned(name, align, TK_ATTR);
    attr_aligned(name->next, align, TK_BATTR);
  }
  attr_aligned(tok, align, TK_ATTR);
  *align = MAX(*align, attr->align);
}

static void cleanup_attr(Token *name, Token *tok, VarAttr *attr, Obj *var) {
  Obj *fn = attr->cleanup_fn;
  DeclAttr(attr_cleanup, &fn);

  if (fn) {
    Node *n = new_node(ND_FUNCALL, tok);
    n->call.expr = new_var_node(fn, tok);
    n->call.expr->ty = fn->ty;
    n->ty = ty_void;

    Node *arg = new_unary(ND_ADDR, new_var_node(var, tok), tok);
    add_type(arg);

    n->call.args = alloc_lvar(arg->ty);
    n->call.args->arg_expr = arg;
    prepare_funcall(n, scope);

    DeferStmt *defr2 = new_defr(DF_CLEANUP_FN);
    defr2->cleanup_fn = n;
  }
}

static bool chk_storage_class(uint8_t msk, StorageClass allow) {
  if (msk == SC_NONE)
    return false;
  if (msk & ~allow)
    return true;
  if (msk & SC_AUTO)
    return msk & SC_TYPEDEF;
  if (msk & SC_CONSTEXPR)
    return msk & ~(SC_CONSTEXPR | SC_AUTO | SC_REGISTER | SC_STATIC);
  if (msk & SC_THREAD)
    return msk & ~(SC_THREAD | SC_STATIC | SC_EXTERN);
  return 1 < ((bool)(msk & SC_EXTERN) + (bool)(msk & SC_REGISTER) +
    (bool)(msk & SC_STATIC) + (bool)(msk & SC_TYPEDEF));
}

static Type *declspec(Token **rest, Token *tok, VarAttr *attr, StorageClass ctx) {
  enum {
    VOID     = 1 << 0,
    BOOL     = 1 << 2,
    CHAR     = 1 << 4,
    SHORT    = 1 << 6,
    INT      = 1 << 8,
    LONG     = 1 << 10,
    FLOAT    = 1 << 12,
    DOUBLE   = 1 << 14,
    SIGNED   = 1 << 16,
    UNSIGNED = 1 << 18,
    OTHER    = 1 << 20,
    BITINT   = 1 << 21,
  };

  Type *ty = NULL;
  int counter = 0;
  QualMask qual = 0;

  if (tok->attr_next)
    tyspec_attr(tok, attr, TK_BATTR);

  for (;;) {
    if (tok->attr_next)
      tyspec_attr(tok, attr, TK_ATTR);

    TokenKind tk_kind = tok->kind;
    if (!(tk_kind >= TK_TYPEKW && tk_kind < TK_TYPEKW_END)) {
      VarScope *vsc;
      if (!ty && (vsc = find_typedef(tok))) {
        ty = vsc->type_def;
        attr->align = MAX(attr->align, vsc->type_def_align);
        tok = tok->next;
        counter |= OTHER;
        continue;
      }
      break;
    }
    tok = tok->next;

    switch (tk_kind) {
    case TK_auto: attr->strg |= SC_AUTO; continue;
    case TK_constexpr: attr->strg |= SC_CONSTEXPR; qual |= Q_CONST; continue;
    case TK_extern: attr->strg |= SC_EXTERN; continue;
    case TK_register: attr->strg |= SC_REGISTER; continue;
    case TK_static: attr->strg |= SC_STATIC; continue;
    case TK_typedef: attr->strg |= SC_TYPEDEF; continue;
    case TK_thread_local: attr->strg |= SC_THREAD; continue;
    case TK_inline: attr->is_inline = true; continue;
    case TK_Noreturn: attr->is_noreturn = true; continue;
    case TK_alignas: {
      tok = skip(tok, "(");
      int align;
      if (is_typename(tok)) {
        VarAttr attr2 = {0};
        Type *ty2 = typename2(&tok, tok, &attr2);
        align = attr2.align ? attr2.align : ty2->align;
      } else {
        align = align_expr(&tok, tok);
      }
      attr->align = MAX(attr->align, align);
      tok = skip(tok, ")");
      continue;
    }
    }

    switch (tk_kind) {
    case TK_const: qual |= Q_CONST; continue;
    case TK_restrict: qual |= Q_RESTRICT; continue;
    case TK_volatile: qual |= Q_VOLATILE; continue;
    case TK_Atomic: {
      qual |= Q_ATOMIC;
      if (consume(&tok, tok , "(")) {
        if (ty)
          error_tok(tok, "invalid type");
        ty = typename(&tok, tok);
        tok = skip(tok, ")");
        counter |= OTHER;
      }
      continue;
    }
    }

    if (!ty) {
      switch (tk_kind) {
      case TK_struct: ty = struct_union_decl(&tok, tok, TY_STRUCT); break;
      case TK_union: ty = struct_union_decl(&tok, tok, TY_UNION); break;
      case TK_enum: ty = enum_specifier(&tok, tok); break;
      case TK_typeof: ty = typeof_specifier(&tok, tok); break;
      case TK_typeof_unqual: ty = unqual(typeof_specifier(&tok, tok)); break;
      case TK_auto_type: ty = new_type(TY_AUTO, -1, 0); break;
      }
      if (ty) {
        counter |= OTHER;
        continue;
      }
    }

    switch (tk_kind) {
    default: error_tok(tok, "invalid type");
    case TK_void: counter += VOID; break;
    case TK_bool: counter += BOOL; break;
    case TK_char: counter += CHAR; break;
    case TK_short: counter += SHORT; break;
    case TK_int: counter += INT; break;
    case TK_long: counter += LONG; break;
    case TK_float: counter += FLOAT; break;
    case TK_double: counter += DOUBLE; break;
    case TK_signed: counter += SIGNED; break;
    case TK_unsigned: counter += UNSIGNED; break;
    case TK_BitInt: {
      counter += BITINT;
      ty = new_bitint(const_expr(&tok, skip(tok, "(")), tok);
      tok = skip(tok, ")");
      break;
    }
    }

    switch (counter) {
    default: error_tok(tok, "invalid type");
    case VOID: ty = ty_void; break;
    case BOOL: ty = ty_bool; break;
    case CHAR: ty = ty_pchar; break;
    case SIGNED + CHAR: ty = ty_char; break;
    case UNSIGNED + CHAR: ty = ty_uchar; break;
    case SHORT:
    case SHORT + INT:
    case SIGNED + SHORT:
    case SIGNED + SHORT + INT: ty = ty_short; break;
    case UNSIGNED + SHORT:
    case UNSIGNED + SHORT + INT: ty = ty_ushort; break;
    case INT:
    case SIGNED:
    case SIGNED + INT: ty = ty_int; break;
    case UNSIGNED:
    case UNSIGNED + INT: ty = ty_uint; break;
    case LONG:
    case LONG + INT:
    case SIGNED + LONG:
    case SIGNED + LONG + INT: ty = ty_long; break;
    case LONG + LONG:
    case LONG + LONG + INT:
    case SIGNED + LONG + LONG:
    case SIGNED + LONG + LONG + INT: ty = ty_llong; break;
    case UNSIGNED + LONG:
    case UNSIGNED + LONG + INT: ty = ty_ulong; break;
    case UNSIGNED + LONG + LONG:
    case UNSIGNED + LONG + LONG + INT: ty = ty_ullong; break;
    case FLOAT: ty = ty_float; break;
    case DOUBLE: ty = ty_double; break;
    case LONG + DOUBLE: ty = ty_ldouble; break;
    case BITINT:
    case BITINT + SIGNED: break;
    case BITINT + UNSIGNED: ty->is_unsigned = true; break;
    }
  }
  *rest = tok;

  if (chk_storage_class(attr->strg, ctx))
    error_tok(tok, "invalid storage class");

  if (!ty) {
    if (opt_std == STD_C89) {
      ty = ty_int;
    } else if (opt_std >= STD_C23 && (attr->strg & SC_AUTO)) {
      ty = new_type(TY_AUTO, -1, 0);
    } else {
      if (tok->kind == TK_IDENT)
        error_tok(tok, "unknown type name");
      error_tok(tok, "implicit int only supported under -std=c89");
    }
  }

  if (ty->kind == TY_AUTO)
    if (tok->kind != TK_IDENT || !equal(tok->next, "="))
      error_tok(tok, "unsupported form for type inference");

  if (ty->kind == TY_BITINT)
    if (ty->bit_cnt < (1 + !ty->is_unsigned))
      error_tok(tok, "invalid bit width for _BitInt");

  return qual_type(qual, ty);
}

static Type *func_params(Token **rest, Token *tok, Type *rtn_ty, Token **end) {
  Type *fn_ty = func_type(rtn_ty, tok);

  if (equal(tok, "...") && consume(rest, tok->next, ")")) {
    fn_ty->is_variadic = true;
    return fn_ty;
  }
  if (tok->kind == TK_void && consume(rest, tok->next, ")"))
    return fn_ty;

  if (consume(rest, tok, ")")) {
    if (opt_std < STD_C23)
      fn_ty->is_oldstyle = true;
    return fn_ty;
  }
  bool is_def = end && is_func_def(*end ? *end : skip_paren(tok));

  if (!is_typename(tok)) {
    fn_ty->is_oldstyle = true;

    Token *start = tok;
    if (!is_def) {
      while (comma_list(rest, &tok, ")", tok != start))
        ident_tok(&tok, tok);
    } else {
      enter_param_scope();
      fn_ty->scopes = scope;

      Obj head = {0};
      Obj *cur = &head;
      while (comma_list(rest, &tok, ")", tok != start)) {
        Token *name = ident_tok(&tok, tok);
        cur = cur->param_next = new_param(get_ident(name), NULL);
      }
      fn_ty->param_list = head.param_next;
      leave_scope();
    }
    return fn_ty;
  }

  enter_param_scope();
  fn_ty->scopes = scope;

  Obj head = {0};
  Obj *cur = &head;
  Node *expr = NULL;

  while (comma_list(rest, &tok, ")", cur != &head)) {
    if (equal(tok, "...")) {
      fn_ty->is_variadic = true;
      *rest = skip(tok->next, ")");
      break;
    }
    Token *name = NULL;
    Type *ty = declspec(&tok, tok, &(VarAttr){0}, SC_REGISTER);
    ty = declarator2(&tok, tok, ty, &name,
      &(DeclContext){.let_star = !is_def, .is_param = true});

    if (is_def)
      chain_expr(&expr, calc_vla(ty, tok));

    Type *param_ty = ptr_decay(ty);
    if (ty->param_qual)
      param_ty = qual_type(ty->param_qual, param_ty);

    if (param_ty->kind == TY_VOID)
      error_tok(tok, "parameter declared void");
    cur = cur->param_next = new_param(NULL, param_ty);
    if (name)
      push_var_name(name, cur);
  }
  fn_ty->pre_calc = expr;
  fn_ty->param_list = head.param_next;
  leave_scope();
  return fn_ty;
}

static Type *array_dimensions(Token **rest, Token *tok, Type *ty, DeclContext *ctx) {
  Node *expr;
  if (consume(&tok, tok, "]")) {
    expr = NULL;
  } else if (equal(tok, "*") && equal(tok->next, "]")) {
    if (!ctx->let_star)
      error_tok(tok, "`[*]` not allowed here");
    expr = new_unknown(ty_size_t, tok);
    tok = tok->next->next;
  } else {
    expr = assign(&tok, tok);
    add_type(expr);
    if (!is_integer(expr->ty))
      error_tok(tok, "size of array not integer");
    tok = skip(tok, "]");
  }

  if (equal(tok, "["))
    ty = array_dimensions(&tok, tok->next, ty, ctx);

  if (ty->size < 0 || ty->kind == TY_VOID || ty->kind == TY_FUNC)
    error_tok(tok, "invalid array element type");

  *rest = tok;
  if (!expr)
    return array_of(ty, -1);

  int64_t array_len;
  if (is_const_expr(expr, &array_len)) {
    if (array_len < 0)
      error_tok(expr->tok, "size of array is negative");
    if (ty->kind == TY_VLA)
      return vla_of(ty, NULL, array_len);
    return array_of(ty, array_len);
  }
  if (is_global_init_context)
    error_tok(tok, "variably-modified type in constant context");
  return vla_of(ty, expr, 0);
}

static QualMask pointer_qualifiers(Token **rest, Token *tok) {
  QualMask qual = 0;
  for (;; tok = tok->next) {
    switch (tok->kind) {
    case TK_Atomic: qual |= Q_ATOMIC; continue;
    case TK_const: qual |= Q_CONST; continue;
    case TK_restrict: qual |= Q_RESTRICT; continue;
    case TK_volatile: qual |= Q_VOLATILE; continue;
    }
    break;
  }
  *rest = tok;
  return qual;
}

static Type *type_suffix(Token **rest, Token *tok, Type *ty, DeclContext *ctx) {
  if (equal(tok, "("))
    return func_params(rest, tok->next, ty, NULL);

  if (consume(&tok, tok, "[")) {
    QualMask qual = 0;
    bool has_static = false;

    if (ctx->is_param) {
      has_static = consume(&tok, tok, "static");
      if (tok->kind >= TK_TYPEKW && tok->kind < TK_TYPEKW_END) {
        qual = pointer_qualifiers(&tok, tok);
        if (!has_static)
          has_static = consume(&tok, tok, "static");
      }
    }
    Type *ty2 = array_dimensions(rest, tok, ty, ctx);

    if (ctx->is_param) {
      if (has_static && ty2->array_len < 0)
        error_tok(tok, "'static' requires an array size");
      ty2->param_qual = qual;
    }
    return ty2;
  }
  *rest = tok;
  return ty;
}

// pointers = ("*" ("const" | "volatile" | "restrict")*)*
static Type *pointers(Token **rest, Token *tok, Type *ty) {
  while (consume(&tok, tok, "*")) {
    ty = qual_type(pointer_qualifiers(&tok, tok), pointer_to(ty));
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

static Type *declarator2(Token **rest, Token *tok, Type *ty, Token **name_tok, DeclContext *ctx) {
  ty = pointers(&tok, tok, ty);

  if (consume(&tok, tok, "(")) {
    if (is_typename(tok) || equal(tok, "...") || equal(tok, ")"))
      return func_params(rest, tok, ty, NULL);

    ty = type_suffix(rest, skip_paren(tok), ty, ctx);
    if (ctx->is_glob) {
      ctx->is_glob = false;
      ctx->end = *rest;
    }
    return declarator2(&(Token *){0}, tok, ty, name_tok, ctx);
  }

  if (name_tok && tok->kind == TK_IDENT) {
    *name_tok = tok;
    tok = tok->next;
  }
  if (consume(&tok, tok, "("))
    return func_params(rest, tok, ty, &ctx->end);

  return type_suffix(rest, tok, ty, ctx);
}

static Type *declarator(Token **rest, Token *tok, Type *ty, Token **name_tok) {
  return declarator2(rest, tok, ty, name_tok, &(DeclContext){0});
}

static Type *typename2(Token **rest, Token *tok, VarAttr *attr) {
  Type *ty = declspec(&tok, tok, attr, SC_NONE);
  return declarator(rest, tok, ty, NULL);
}

static Type *typename(Token **rest, Token *tok) {
  return typename2(rest, tok, &(VarAttr){0});
}

static void new_enum(Type **ty) {
  if (*ty) {
    (*ty) = copy_type(unqual(*ty));
  } else {
    (*ty) = new_type(TY_ENUM, -1, 1);
    (*ty)->is_int_enum = true;
  }
  (*ty)->is_enum = true;
}

static bool chk_enum_tag(Type *tag_ty, Type *fixed_ty, Token *tag) {
  if (!tag_ty)
    return false;
  if (tag_ty->kind == TY_STRUCT || tag_ty->kind == TY_UNION)
    error_tok(tag, "not an enum tag");
  if (fixed_ty && (fixed_ty->kind != tag_ty->kind ||
    fixed_ty->is_unsigned != tag_ty->is_unsigned))
    error_tok(tag, "enum redeclared with incompatible type");
  return true;
}

static Type *enum_specifier(Token **rest, Token *tok) {
  bool is_packed = false;
  bool_attr(tok, TK_ATTR, "packed", &is_packed);
  bool_attr(tok, TK_BATTR, "packed", &is_packed);

  // Read a tag.
  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  Type *ty = NULL;
  if (consume(&tok, tok, ":")) {
    ty = typename(&tok, tok);
    if (!equal(tok, "{"))
      skip(tok, ";");
  }
  if (tag && !equal(tok, "{")) {
    *rest = tok;

    Type *tag_ty = ty ? find_tag_in_scope(tag) : find_tag(tag);
    if (chk_enum_tag(tag_ty, ty, tag))
      return tag_ty;

    new_enum(&ty);
    push_tag_scope(tag, ty);
    return ty;
  }
  tok = skip(tok, "{");

  EnumVal *tag_enums = NULL;
  int64_t tag_enum_cnt = 0;
  int64_t decl_enum_cnt = 0;

  Type *tag_ty = NULL;
  if (tag)
    tag_ty = find_tag_in_scope(tag);

  if (chk_enum_tag(tag_ty, ty, tag)) {
    if (opt_std < STD_C23) {
      if (!ty && tag_ty->kind != TY_ENUM)
        error_tok(tag, "enum redeclaration");
    } else {
      tag_enums = tag_ty->enums;
      for (EnumVal *ev = tag_enums; ev; ev = ev->next)
        tag_enum_cnt++;
    }
    ty = tag_ty;
  } else {
    new_enum(&ty);
    if (tag)
      push_tag_scope(tag, ty);
  }

  EnumType ety = ETY_I8;
  bool been_neg = false;

  EnumVal head = {0};
  EnumVal *cur = &head;

  uint64_t val = 0;
  bool is_neg = false;
  bool is_ovf = false;
  bool first = true;
  for (; comma_list(&tok, &tok, "}", !first); first = false) {
    Token *name = ident_tok(&tok, tok);

    if (!consume(&tok, tok, "=")) {
      if (is_ovf)
        error_tok(tok, "enum value overflowed");
    } else {
      Type *val_ty = NULL;
      val = const_expr2(&tok, tok, &val_ty);

      if ((is_neg = (!val_ty->is_unsigned && (int64_t)val < 0))) {
        been_neg = true;

        if ((int64_t)val < INT8_MIN) {
          if ((int64_t)val >= INT16_MIN)
            ety = MAX(ety, ETY_I16);
          else if ((int64_t)val >= INT32_MIN)
            ety = MAX(ety, ETY_I32);
          else
            ety = MAX(ety, ETY_I64);
        }
      }
    }

    if (!is_neg && val > INT8_MAX) {
      if (val <= UINT8_MAX)
        ety = MAX(ety, ETY_U8);
      else if (val <= INT16_MAX)
        ety = MAX(ety, ETY_I16);
      else if (val <= UINT16_MAX)
        ety = MAX(ety, ETY_U16);
      else if (val <= INT32_MAX)
        ety = MAX(ety, ETY_I32);
      else if (val <= UINT32_MAX)
        ety = MAX(ety, ETY_U32);
      else if (val <= INT64_MAX)
        ety = MAX(ety, ETY_I64);
      else
        ety = ETY_U64;
    }

    int64_t v = val++;
    is_ovf = !is_neg && val == 0;
    is_neg = (int64_t)val < 0;

    HashEntry *ent = hashmap_get_or_insert(&decl_scope()->vars, name->loc, name->len);
    VarScope *vsc = ent->val;
    if (vsc) {
      if (opt_std >= STD_C23 && tag_enum_cnt && vsc->enum_ty) {
        if (vsc->enum_val != v)
          error_tok(tok, "enum redeclared with conflicting value");
        decl_enum_cnt++;
        continue;
      }
      error_tok(name, "enum redeclaration");
    } else {
      if (opt_std >= STD_C23) {
        cur = cur->next = malloc(sizeof(EnumVal));
        cur->val = v;
        cur->name = name;
        name->is_live = true;
      }
      vsc = ent->val = ast_arena_calloc(sizeof(VarScope));
      vsc->enum_ty = ty;
      vsc->enum_val = v;
    }
  }
  if (first)
    error_tok(tok, "empty enum specifier");

  bool_attr(tok, TK_ATTR, "packed", &is_packed);
  *rest = tok;

  if (opt_std >= STD_C23) {
    if (tag_enum_cnt) {
      if (tag_enum_cnt != decl_enum_cnt)
        error_tok(tag, "enum redeclared with conflicting value");
      return ty;
    }
    cur->next = NULL;
    ty->enums = head.next;
  }

  if (ty->kind != TY_ENUM) {
    if ((ty->is_unsigned && been_neg) ||
      (ty->size < enum_ty[ety]->size) ||
      ((ty->size == enum_ty[ety]->size) && (ty->is_unsigned < enum_ty[ety]->is_unsigned)))
      error_tok(tok, "enum value out of type range");
    return ty;
  }

  if (!(is_packed || opt_short_enums))
    ety = MAX(ety, ety_of_int);

  bool is_int = ety <= ety_of_int;
  if ((been_neg && (ety & 1)) || (!been_neg && !(ety & 1))) {
    ety++;
    if (ety > ETY_U64)
      error_tok(tok, "unsupported enum value range");
  }

  Type *base_ty = enum_ty[ety];
  for (Type *t = ty; t; t = t->decl_next) {
    t->kind = base_ty->kind;
    t->is_unsigned = base_ty->is_unsigned;
    t->size = base_ty->size;
    t->align = base_ty->align;
    t->is_int_enum = is_int;
  }
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

    if (is_vm_ty(ty))
      chk_vla_expr_side_effect(node);
  }
  *rest = skip(tok, ")");
  return ty;
}

static Node *vla_count(Type *ty, Token *tok, bool is_void) {
  if (!ty->vla_len)
    return is_void ? NULL : new_size_t(ty->array_len, tok);

  if (ty->vla_cnt)
    return is_void ? NULL : new_var_node(ty->vla_cnt, tok);

  ty->vla_cnt = new_lvar2(ty_size_t, base_scope());
  return new_binary(ND_ASSIGN, new_var_node(ty->vla_cnt, tok), ty->vla_len, tok);
}

static Node *vla_size(Type *ty, Token *tok) {
  Node *base_sz;
  if (ty->base->kind == TY_VLA)
    base_sz = vla_size(ty->base, tok);
  else
    base_sz = new_size_t(ty->base->size, tok);

  return new_binary(ND_MUL, vla_count(ty, tok, false), base_sz, tok);
}

static Node *calc_vla(Type *ty, Token *tok) {
  Node *n = NULL;
  if (ty->kind == TY_VLA)
    n = vla_count(ty, tok, true);

  if (ty->base)
    chain_expr(&n, calc_vla(ty->base, tok));
  else if (ty->kind == TY_FUNC)
    chain_expr(&n, calc_vla(ty->return_ty, tok));
  return n;
}

static Node *declaration2(Token **rest, Token *tok, Type *basety, VarAttr *attr, Obj **cond_var) {
  Token *name = NULL;
  Type *ty = declarator(&tok, tok, basety, &name);

  if (ty->kind == TY_FUNC) {
    if (!name)
      error_tok(tok, "function name omitted");

    Obj *fn = func_prototype(ty, attr, name);
    assembler_name(&tok, tok, fn);
    aligned_attr(name, tok, attr, &fn->alt_align);
    symbol_attr(name, tok, attr, fn);
    func_attr(name, tok, attr, fn, false);

    *rest = tok;
    return NULL;
  }
  if (ty->kind == TY_VOID)
    error_tok(tok, "variable declared void");
  if (!name)
    error_tok(tok, "variable name omitted");

  if (attr->strg & SC_STATIC) {
    if (ty->kind == TY_VLA)
      error_tok(tok, "variable length arrays cannot be 'static'");

    Obj *var = new_static_lvar(ty);
    push_var_name(name, var);

    var->is_tls = attr->strg & SC_THREAD;
    assembler_name(&tok, tok, var);
    aligned_attr(name, tok, attr, &var->alt_align);
    symbol_attr(name, tok, attr, var);

    if (attr->strg & SC_CONSTEXPR)
      constexpr_initializer(&tok, skip(tok, "="), var, var);
    else if (equal(tok, "="))
      gvar_initializer(&tok, tok->next, var);
    *rest = tok;
    return NULL;
  }

  Node *expr = NULL;
  chain_expr(&expr, calc_vla(ty, tok));

  Obj *var = new_lvar(ty);
  push_var_name(name, var);

  if (ty->kind == TY_VLA) {
    fn_use_vla = true;

    Node *node = vla_size(ty, name);
    add_type(node);
    node = new_unary(ND_ALLOCA, node, name);
    node->ty = pointer_to(ty_void);
    node->m.var = var;

    new_defr(DF_VLA_DEALLOC)->vla = var;

    aligned_attr(name, tok, attr, &var->alt_align);
    cleanup_attr(name, tok, attr, var);

    if (equal(tok, "=")) {
      tok = skip(skip(tok->next, "{"), "}");
      node->kind = ND_ALLOCA_ZINIT;
    }
    *rest = tok;
    chain_expr(&expr, node);
    return expr;
  }

  assembler_name(&tok, tok, var);
  aligned_attr(name, tok, attr, &var->alt_align);
  cleanup_attr(name, tok, attr, var);

  if (attr->strg & SC_CONSTEXPR) {
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
  *rest = tok;
  return expr;
}

static Node *cond_declaration(Token **rest, Token *tok, char *stopper, int clause) {
  Node *n = NULL;
  Obj *var = NULL;

  for (; is_typename(tok); var = NULL) {
    clause++;

    VarAttr attr = {0};
    Type *basety = declspec(&tok, tok, &attr, SC_AUTO | SC_CONSTEXPR | SC_REGISTER);

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

static Node *declaration(Token **rest, Token *tok) {
  VarAttr attr = {0};
  Type *basety = declspec(&tok, tok, &attr, SC_ALL);

  if (attr.strg & SC_EXTERN) {
    global_declaration(rest, tok, basety, &attr);
    return NULL;
  }
  if (attr.strg & SC_TYPEDEF)
    return parse_typedef(rest, tok, basety, &attr);

  Node *expr = NULL;
  for (bool first = true; comma_list(rest, &tok, ";", !first); first = false)
    chain_expr(&expr, declaration2(&tok, tok, basety, &attr, &(Obj *){0}));
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

static bool string_initializer(Token **rest, Token *tok, Initializer *init) {
  int paren = 0;
  while (consume(&tok, tok, "("))
    paren++;

  if (tok->kind != TK_STR)
    return false;
  if (tok->ty->base->size != init->ty->base->size)
    error_tok(tok, "array initialization with string of incompatible size");

  if (init->kind == INIT_FLEX)
    init->ty = array_of(init->ty->base, tok->ty->array_len);

  set_init(init, INIT_STR_ARRAY);
  init->tok = tok;

  tok = tok->next;
  while (paren--)
    tok = skip(tok, ")");
  *rest = tok;
  return true;
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

static void designation(Token **rest, Token *tok, Initializer *init,
  bool post_bracket, DesgContext *ctx) {
  if (equal(tok, "[")) {
    if (init->ty->kind != TY_ARRAY)
      error_tok(tok, "array index in non-array initializer");

    int begin, end;
    array_designator(&tok, tok, init->ty, &begin, &end);
    prepare_array_init(init, init->ty);

    Token *start = tok;
    if (begin == end || is_global_init_context) {
      for (int i = begin; i <= end; i++)
        designation(&tok, start, &init->list.data[i], true, ctx);
    } else {
      ctx = ctx ? ctx : &(DesgContext){0};
      ctx->lvl++;

      for (int i = begin; i <= end; i++) {
        if (ctx->init_lvl != ctx->lvl) {
          designation(&tok, start, &init->list.data[i], true, ctx);

          if (ctx->init)
            continue;
          ctx->init = arena_malloc(&ast_arena, sizeof(Initializer));
          *ctx->init = init->list.data[i];

          ctx->var = new_lvar(init->list.data[i].ty);
          ctx->end = tok;
          ctx->init_lvl = ctx->lvl;
        }
        init->list.data[i].kind = INIT_COPY;
        init->list.data[i].cpy.init = ctx->init;
        init->list.data[i].cpy.var = ctx->var;
      }
      ctx->lvl--;
      tok = ctx->end;
    }

    list_initializer(rest, tok, init, end + 1);
    return;
  }

  if (equal(tok, ".")) {
    Member *mem = struct_designator(&tok, tok->next, init->ty);
    prepare_struct_init(init, init->ty);

    if (init->ty->kind == TY_UNION) {
      init->list.union_idx = mem->idx;
      designation(rest, tok, &init->list.data[mem->idx], false, ctx);
    } else {
      designation(&tok, tok, &init->list.data[mem->idx], false, ctx);
      list_initializer(rest, tok, init, mem->idx + 1);
    }
    return;
  }

  if (post_bracket)
    consume(&tok, tok, "=");
  else
    tok = skip(tok, "=");

  initializer2(rest, tok, init);
}

// An array length can be omitted if an array has an initializer
// (e.g. `int x[] = {1,2,3}`). If it's omitted, count the number
// of initializer elements.
static int count_array_init_elements(Token *tok, Type *ty) {
  Initializer dummy = {.ty = ty->base};

  int i = 0, max = 0;
  while (comma_list(&tok, &tok, "}", i)) {
    if (equal(tok, "[")) {
      i = const_expr(&tok, tok->next);
      if (equal(tok, "..."))
        i = const_expr(&tok, tok->next);
      tok = skip(tok, "]");
      designation(&tok, tok, &dummy, true, NULL);
    } else {
      initializer2(&tok, tok, &dummy);
    }

    i++;
    max = MAX(max, i);
  }
  free_initializers(&dummy);
  return max;
}

static void aggregate_initializer(Token **rest, Token *tok, Initializer *init, bool has_brace) {
  Token *start = tok;
  list_initializer(&tok, tok, init, 0);

  if (has_brace) {
    for (; comma_list(&tok, &tok, "}", tok != start);) {
      if (equal(tok, ".") || equal(tok, "[")) {
        designation(&tok, tok, init, false, NULL);
        continue;
      }
      tok = skip_excess_element(tok);
    }
  }
  *rest = tok;
}

static void list_initializer(Token **rest, Token *tok, Initializer *init, int idx) {
  for (; idx < init->list.cnt && !equal(tok, "}"); idx++) {
    Token *tok2 = (idx == 0) ? tok : skip(tok, ",");

    if (equal(tok2, "}") || equal(tok2, "[") || equal(tok2, "."))
      break;

    initializer2(&tok, tok2, &init->list.data[idx]);

    if (init->ty->kind == TY_UNION) {
      init->list.union_idx = idx;
      break;
    }
  }
  *rest = tok;
}

static void initializer2(Token **rest, Token *tok, Initializer *init) {
  bool has_brace = consume(&tok, tok, "{");
  if (has_brace && consume(rest, tok, "}")) {
    if (init->kind == INIT_FLEX)
      init->ty = array_of(init->ty->base, 0);
    set_init(init, INIT_NONE);
    return;
  }

  if (init->ty->kind == TY_ARRAY) {
    if (init->kind == INIT_FLEX_NESTED)
      error_tok(tok, "nested initialization of flexible array member");

    if (init->ty->base->kind != TY_BOOL && is_integer(init->ty->base)) {
      if (string_initializer(&tok, tok, init)) {
        *rest = has_brace ? (consume(&tok, tok, ","), skip(tok, "}")) : tok;
        return;
      }
    }
    if (init->is_root && !has_brace)
      error_tok(tok, "expected initializer list");

    if (init->kind == INIT_FLEX) {
      int len = count_array_init_elements(tok, init->ty);
      init->ty = array_of(init->ty->base, len);
    }
    prepare_array_init(init, init->ty);
    aggregate_initializer(rest, tok, init, has_brace);
    return;
  }

  if (init->ty->kind == TY_STRUCT || init->ty->kind == TY_UNION) {
    if (!has_brace) {
      Node *expr = assign(rest, tok);
      add_type(expr);
      if (is_compatible(expr->ty, init->ty)) {
        set_init(init, INIT_EXPR);
        init->expr = expr;
        return;
      }
      if (init->is_root)
        error_tok(tok, "expected initializer list");
    }
    prepare_struct_init(init, init->ty);
    aggregate_initializer(rest, tok, init, has_brace);
    return;
  }

  if ((tok->kind == TK_INT_NUM || tok->kind == TK_PP_NUM) &&
    (equal(tok->next, ",") || equal(tok->next, "}") || equal(tok->next, ";"))) {
    init->kind = INIT_TOK;
    init->tok = tok;
    tok = tok->next;
  } else {
    init->kind = INIT_EXPR;
    init->expr = assign(&tok, tok);
  }
  *rest = has_brace ? (consume(&tok, tok, ","), skip(tok, "}")) : tok;
}

static void initializer(Token **rest, Token *tok, Initializer *init, Obj *var) {
  Type *ty = var->ty;

  if (ty->kind == TY_AUTO) {
    init->kind = INIT_EXPR;
    init->expr = assign(rest, tok);
    ptr_convert(&init->expr);
    new_derived_type(ty, ty->qual, init->expr->ty);
    init->ty = ty;
    return;
  }

  init->ty = ty;
  if (ty->kind == TY_ARRAY && ty->size < 0)
    init->kind = INIT_FLEX;
  else
    init->is_root = true;

  initializer2(rest, tok, init);
  var->ty = init->ty;

  if (ty->is_flexible && init->kind == INIT_LIST) {
    ty = new_derived_type(NULL, ty->qual, ty);

    Initializer *i = &init->list.data[init->list.cnt - 1];
    if (i->ty->size > 0)
      ty->size += i->ty->size;
    var->ty = ty;
  }
}

static Node *init_desg_expr(InitDesg *desg, Token *tok) {
  if (desg->var)
    return new_var_node(desg->var, tok);

  if (desg->member) {
    Node *node = new_unary(ND_MEMBER, init_desg_expr(desg->parent, tok), tok);
    node->m.member = desg->member;
    return node;
  }

  Node *lhs = init_desg_expr(desg->parent, tok);
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

static Node *create_lvar_init(Node *expr, Initializer *init, InitDesg *desg, Token *tok) {
  switch (init->kind) {
  case INIT_NONE:
  case INIT_FLEX:
  case INIT_FLEX_NESTED:
    return expr;
  case INIT_TOK:
  case INIT_EXPR: {
    Node *node = init_num_tok(init, new_node(ND_NUM, init->tok));

    Node *n = new_binary(ND_ASSIGN, init_desg_expr(desg, tok), node, tok);
    add_type(n);
    return expr->next = n;
  }
  case INIT_STR_ARRAY: {
    Token *str = init->tok;
    size_t base_sz = init->ty->base->size;
    Obj *var = new_anon_gvar(init->ty);
    var->is_string_lit = true;
    if (str->ty->array_len < init->ty->array_len) {
      var->init_data = calloc(1, base_sz * init->ty->array_len);
      memcpy(var->init_data, str->str, base_sz * str->ty->array_len);
    } else {
      var->init_data = malloc(base_sz * init->ty->array_len);
      memcpy(var->init_data, str->str, base_sz * init->ty->array_len);
    }
    Node *n = new_binary(ND_ASSIGN, init_desg_expr(desg, tok), new_var_node(var, tok), tok);
    add_type(n->m.lhs);
    add_type(n->m.rhs);
    n->ty = ty_void;

    // non-static struct with flexible array member, supported as extension
    if (n->m.lhs->ty->size < 0)
      n->m.lhs->ty = init->ty;
    return expr->next = n;
  }
  case INIT_LIST: {
    if (init->ty->kind == TY_ARRAY) {
      for (int i = 0; i < init->list.cnt; i++) {
        InitDesg desg2 = {.parent = desg, .idx = i};
        expr = create_lvar_init(expr, &init->list.data[i], &desg2, tok);
      }
      return expr;
    }
    if (init->ty->kind == TY_STRUCT || init->ty->kind == TY_UNION) {
      for (Member *mem = init->ty->members; mem_iter(&mem); mem = mem->next) {
        if (init->ty->kind == TY_UNION && init->list.union_idx != mem->idx)
          continue;

        InitDesg desg2 = {.parent = desg, .member = mem};
        expr = create_lvar_init(expr, &init->list.data[mem->idx], &desg2, tok);
      }
      return expr;
    }
    break;
  }
  case INIT_COPY: {
    Node *n;
    if (!init->cpy.init->has_copy) {
      init->cpy.init->has_copy = true;
      expr = create_lvar_init(expr, init->cpy.init, desg, tok);

      n = new_binary(ND_ASSIGN, new_var_node(init->cpy.var, tok), init_desg_expr(desg, tok), tok);
    } else {
      n = new_binary(ND_ASSIGN, init_desg_expr(desg, tok), new_var_node(init->cpy.var, tok), tok);
    }
    add_type(n);
    return expr->next = n;
  }
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
  initializer(rest, tok, &init, var);

  InitDesg desg = {.var = var};
  Node head = {0};
  create_lvar_init(&head, &init, &desg, tok);
  free_initializers(&init);

  if (opt_optimize && (init.kind == INIT_EXPR || init.kind == INIT_TOK))
    return head.next;

  // If a partial initializer list is given, the standard requires
  // that unspecified elements are set to 0. Here, we simply
  // zero-initialize the entire memory region of a variable before
  // initializing it with user-supplied values.
  Node *node = new_node(ND_INIT_SEQ, tok);
  node->m.var = var;
  node->m.lhs = head.next;
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

static long_double_t read_double_buf(char *buf, Type *ty) {
  if (ty->kind == TY_FLOAT)
    return BUFF_CAST(float, buf);
  if (ty->kind == TY_DOUBLE)
    return BUFF_CAST(double, buf);
  if (ty->kind == TY_LDOUBLE)
    return BUFF_CAST(long_double_t, buf);
  internal_error();
}

static Relocation *
write_gvar_data(Relocation *cur, Initializer *init, char *buf, int offset, EvalKind ev_kind) {
  switch (init->kind) {
  case INIT_NONE:
  case INIT_FLEX:
  case INIT_FLEX_NESTED:
    return cur;
  case INIT_TOK:
  case INIT_EXPR: {
    Node *node = init_num_tok(init, &(Node){.kind = ND_NUM, .tok = init->tok});
    node = assign_cast(init->ty, node);

    if (init->ty->kind == TY_ARRAY)
      error_tok(node->tok, "array initializer must be an initializer list");

    // Direct initialization with a const variable
    if (init->kind == INIT_EXPR) {
      int sofs;
      Obj *var = NULL;
      if (is_compatible(init->ty, init->expr->ty))
        var = eval_var(init->expr, &sofs, false);

      if (var && var->init_data && !var->is_weak &&
        (is_const_var(var) || var->is_compound_lit)) {
        Relocation *srel= var->rel;
        while (srel && srel->offset < sofs)
          srel = srel->next;

        for (int pos = 0; pos < init->ty->size && (pos + sofs) < var->ty->size;) {
          if (srel && srel->offset == (pos + sofs)) {
            cur = cur->next = ast_arena_calloc(sizeof(Relocation));
            cur->offset = (pos + offset);
            cur->label = srel->label;
            cur->var = srel->var;
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

    if (is_integer(init->ty) || init->ty->kind == TY_PTR || init->ty->kind == TY_NULLPTR) {
      EvalContext ctx = {.kind = (init->ty->size != ty_nullptr->size) ? EV_CONST : ev_kind};
      int64_t val = eval2(node, &ctx);
      if (ctx.label || ctx.var) {
        Relocation *rel = ast_arena_calloc(sizeof(Relocation));
        rel->offset = offset;
        rel->label = ctx.label;
        rel->var = ctx.var;
        rel->addend = val;
        return cur->next = rel;
      }
      memcpy(buf + offset, &val, init->ty->size);
      return cur;
    }

    if (is_flonum(init->ty)) {
      FPVal fval = {0};
      eval_fp(node, &fval);
      memcpy(buf + offset, &fval, init->ty->size);
      return cur;
    }

    if (init->ty->kind == TY_BITINT) {
      uint64_t *data = eval_bitint(node);
      if (init->ty->bit_cnt < init->ty->size * 8)
        eval_bitint_sign_ext(init->ty->bit_cnt, data, init->ty->size * 8, init->ty->is_unsigned);
      memcpy(buf + offset, data, init->ty->size);
      free(data);
      return cur;
    }

    error_tok(node->tok, "invalid initializer");
  }
  case INIT_STR_ARRAY: {
    size_t len = MIN(init->ty->array_len, init->tok->ty->array_len);
    memcpy(buf + offset, init->tok->str, init->ty->base->size * len);
    return cur;
  }
  case INIT_LIST:
    if (init->ty->kind == TY_ARRAY) {
      int sz = init->ty->base->size;
      for (int i = 0; i < init->list.cnt; i++)
        cur = write_gvar_data(cur, &init->list.data[i], buf, offset + sz * i, ev_kind);
      return cur;
    }

    if (init->ty->kind == TY_STRUCT || init->ty->kind == TY_UNION) {
      for (Member *mem = init->ty->members; mem_iter(&mem); mem = mem->next) {
        if (init->ty->kind == TY_UNION && init->list.union_idx != mem->idx)
          continue;

        Initializer *init2 = &init->list.data[mem->idx];
        if (mem->is_bitfield && (init2->kind == INIT_EXPR || init2->kind == INIT_TOK)) {
          Node *node = init_num_tok(init2, &(Node){.kind = ND_NUM, .tok = init2->tok});
          node = assign_cast(mem->ty, node);

          char *loc = buf + offset + mem->offset;

          if (mem->ty->kind == TY_BITINT) {
            uint64_t *val = eval_bitint(node);
            eval_bitint_bitfield_save(mem->ty->bit_cnt, val, loc, mem->bit_width, mem->bit_offset);
            free(val);
            continue;
          }
          uint64_t val = eval(node);
          if (mem->is_aligned_bitfiled) {
            int sz = next_pow_of_two(mem->bit_offset + mem->bit_width) / 8;
            uint64_t oldval = read_buf(loc, sz);
            uint64_t mask = (1L << mem->bit_width) - 1;
            uint64_t combined = oldval | ((val & mask) << mem->bit_offset);
            memcpy(loc, &combined, sz);
            continue;
          }
          int rem = mem->bit_offset + mem->bit_width;
          if (mem->bit_offset) {
            int shft = 8 - mem->bit_offset;
            *loc = ((uint8_t)(*loc << shft) >> shft) | (val << mem->bit_offset);
            val >>= shft;
            rem -= 8;
            loc++;
          }
          for (int start = rem; rem > 0; rem -= 8, loc++) {
            if (rem != start)
              val >>= 8;
            if (rem >= 8) {
              *loc = val;
              continue;
            }
            val &= (1 << rem) - 1;
            *loc = (*loc >> rem) << rem | val;
            break;
          }
          continue;
        }
        cur = write_gvar_data(cur, init2, buf, offset + mem->offset, ev_kind);
      }
      return cur;
    }
  }
  internal_error();
}

// Initializers for global variables are evaluated at compile-time and
// embedded to .data section. This function serializes Initializer
// objects to a flat byte array. It is a compile error if an
// initializer list contains a non-constant expression.
static void gvar_initializer(Token **rest, Token *tok, Obj *var) {
  bool ctx = is_global_init_context;
  is_global_init_context = true;

  Initializer init = {0};
  initializer(rest, tok, &init, var);

  if (var->ty->size < 0)
    error_tok(tok, "variable has incomplete type");

  Relocation head = {0};
  char *buf = calloc(1, var->ty->size);
  write_gvar_data(&head, &init, buf, 0, EV_LABEL);
  free_initializers(&init);

  var->init_data = buf;
  var->rel = head.next;

  is_global_init_context = ctx;
}

static void constexpr_initializer(Token **rest, Token *tok, Obj *init_var, Obj *var) {
  bool ctx = is_global_init_context;
  is_global_init_context = true;

  Initializer init = {0};
  initializer(rest, tok, &init, init_var);

  Relocation head = {0};
  char *buf = calloc(1, init_var->ty->size);
  write_gvar_data(&head, &init, buf, 0, EV_CONST);
  free_initializers(&init);

  init_var->init_data = var->constexpr_data = buf;
  init_var->rel = head.next;
  var->ty = init_var->ty;

  is_global_init_context = ctx;
}

// Returns true if a given token represents a type.
static bool is_typename(Token *tok) {
  return (tok->kind >= TK_TYPEKW && tok->kind < TK_TYPEKW_END) || find_typedef(tok);
}

static bool is_typename_paren2(Token **rest, Token *tok, Type **ty, VarAttr *attr) {
  if (equal(tok, "(") && is_typename(tok->next) &&
    !equal(skip_paren(tok->next->next), "{")) {
    *ty = typename2(&tok, tok->next, attr);
    *rest = skip(tok, ")");
    return true;
  }
  return false;
}

static bool is_typename_paren(Token **rest, Token *tok, Type **ty) {
  return is_typename_paren2(rest, tok, ty, &(VarAttr){0});
}

static void eval_static_assert(Token **rest, Token *tok) {
  int64_t result = const_expr(&tok, skip(tok, "("));
  if (!result)
    error_tok(tok, "static assertion failed");

  if (equal(tok, ",")) {
    if (tok->next->kind != TK_STR)
      error_tok(tok, "expected string literal");
    tok = tok->next->next;
  }
  *rest = skip(tok, ")");
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
    Node *node = new_node(ND_GOTO, ident_tok(&tok, tok));
    push_goto(node);
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
    if (tok->kind == TK_inline || tok->kind == TK_volatile) {
      tok = tok->next;
      continue;
    } else if (equal(tok, "goto")) {
      tok = tok->next;
      is_asm_goto = true;
      continue;
    }
    break;
  }
  node->gasm.str_tok = str_tok(&tok, skip(tok, "("));
  if (consume(rest, tok, ")"))
    return node;

  node->gasm.outputs = asm_params(&tok, skip(tok, ":"));
  if (consume(rest, tok, ")"))
    return node;

  node->gasm.inputs = asm_params(&tok, skip(tok, ":"));
  if (consume(rest, tok, ")"))
    return node;

  node->gasm.clobbers = asm_clobbers(&tok, skip(tok, ":"));
  if (!is_asm_goto) {
    *rest = skip(tok, ")");
    return node;
  }
  node->gasm.labels = asm_labels(rest, skip(tok, ":"));
  return node;
}

static LocalLabel *find_local_label(Scope *sc, Token *tok) {
  for (LocalLabel *ll = sc->labels; ll; ll = ll->next)
    if (equal_tok(ll->name, tok))
      return ll;
  return NULL;
}

static void push_goto(Node *node) {
  for (Scope *sc = scope; sc; sc = sc->parent) {
    if (find_local_label(sc, node->tok)) {
      node->lbl.next = sc->gotos;
      sc->gotos = node;
      return;
    }
  }
  node->lbl.next = gotos;
  gotos = node;
}

static Token *label_stmt(Node **cur_node, Token **rest, Token *tok) {
  Node *case_node = NULL;
  Node *active_sw = NULL;

  Token head = {0};
  Token *cur = &head;

  for (;;) {
    if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
      Token *start = tok;
      cur = cur->label_next = tok;
      tok = tok->next->next;

      LocalLabel *ll = NULL;
      for (Scope *sc = scope; sc; sc = sc->parent)
        if ((ll = find_local_label(sc, start)))
          break;

      if (ll) {
        if (ll->label)
          error_tok(tok, "duplicated label");
        (*cur_node) = (*cur_node)->next = ll->label = new_label(tok);
        continue;
      }

      if (!defer_context) {
        Node *node = new_label(start);
        node->lbl.unique_label = new_unique_name();
        node->lbl.next = labels;
        (*cur_node) = (*cur_node)->next = labels = node;
      }
      continue;
    }

    if (tok->kind == TK_case || tok->kind == TK_default) {
      if (!case_node) {
        (*cur_node) = (*cur_node)->next = case_node = new_label(tok);

        JumpContext *ctx = jump_ctx;
        for (; ctx; ctx = ctx->next)
          if (ctx->node->kind == ND_SWITCH)
            break;
        if (!ctx)
          error_tok(tok, "stray case");
        if (jump_ctx->dfr_lvl != current_defr || jump_ctx->dfr_ctx != defer_context)
          error_tok(tok, "illegal jump");
        active_sw = ctx->node;
      }

      if (tok->kind == TK_default) {
        if (active_sw->ctrl.sw_default)
          error_tok(tok, "duplicated defualt");

        active_sw->ctrl.sw_default = case_node;
        tok = skip(tok->next, ":");
        continue;
      }

      int64_t lo = const_expr(&tok, tok->next);
      int64_t hi;
      if (equal(tok, "..."))
        hi = const_expr(&tok, tok->next);
      else
        hi = lo;

      Type *ty = active_sw->ctrl.cond->ty;
      if (ty->size <= 4) {
        if (!ty->is_unsigned)
          lo = (int32_t)lo, hi = (int32_t)hi;
        else
          lo = (uint32_t)lo, hi = (uint32_t)hi;
      }
      if (hi != lo && less_eq(ty, hi, lo))
        error_tok(tok, "empty case range specified");

      for (CaseRange *cr = active_sw->ctrl.sw_cases; cr; cr = cr->next)
        if ((less_eq(ty, cr->lo, lo) && less_eq(ty, lo, cr->hi)) ||
          (less_eq(ty, lo, cr->lo) && less_eq(ty, cr->lo, hi)))
          error_tok(tok, "duplicated case");

      tok = skip(tok, ":");

      // Merge adjacent ranges
      if (opt_optimize) {
        CaseRange *lo_adj = NULL, *hi_adj = NULL;
        for (CaseRange *cr = active_sw->ctrl.sw_cases; cr; cr = cr->next) {
          if (cr->label != case_node)
            break;
          if (less_eq(ty, cr->hi, lo) && (uint64_t)cr->hi + 1 == lo)
            lo_adj = cr;
          else if (less_eq(ty, hi, cr->lo) && (uint64_t)hi + 1 == cr->lo)
            hi_adj = cr;
        }
        if (lo_adj && hi_adj) {
          if (lo_adj->next) {
            hi_adj->lo = lo_adj->lo;
            *lo_adj = *lo_adj->next;
          } else {
            lo_adj->hi = hi_adj->hi;
            *hi_adj = *hi_adj->next;
          }
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
      cr->label = case_node;
      cr->lo = lo;
      cr->hi = hi;
      cr->next = active_sw->ctrl.sw_cases;
      active_sw->ctrl.sw_cases = cr;
      continue;
    }

    cur->label_next = NULL;
    *rest = tok;
    return head.label_next;
  }
}

static Node *secondary_block(Token **rest, Token *tok) {
  if (equal(tok, "{"))
    return compound_stmt(rest, tok, ND_BLOCK);

  DeferStmt *dfr = new_stmt_scope();
  Node head = {0};
  Node *cur = &head;
  Token *label_list = label_stmt(&cur, &tok, tok);
  cur->next = stmt(rest, tok, label_list);
  return leave_stmt_scope(dfr, head.next);
}

static void loop_body(Token **rest, Token *tok, Node *node, Token *label_list) {
  JumpContext ctx = {.next = jump_ctx};
  jump_ctx = &ctx;

  ctx.labels = label_list;
  ctx.node = node;
  ctx.dfr_lvl = current_defr;
  ctx.dfr_ctx = defer_context;

  node->ctrl.then = secondary_block(rest, tok);

  jump_ctx = ctx.next;
}

static JumpContext *resolve_labeled_jump(Token **rest, Token *tok, bool is_cont) {
  Token *name = NULL;
  if (tok->next->kind == TK_IDENT)
    name = tok = tok->next;
  *rest = skip(tok->next, ";");

  for (JumpContext *ctx = jump_ctx; ctx; ctx = ctx->next) {
    if (is_cont && ctx->node->kind == ND_SWITCH)
      continue;
    if (!name)
      return ctx;
    for (Token *t = ctx->labels; t; t = t->label_next)
      if (equal_tok(t, name))
        return ctx;
  }
  error_tok(tok, "cannot resolve jump");
}

static Node *stmt(Token **rest, Token *tok, Token *label_list) {
  if (tok->kind == TK_return) {
    if (defer_context)
      error_tok(tok, "return in defer block");

    Node *node = new_node(ND_RETURN, tok);
    node->dfr_from = current_defr;
    if (consume(rest, tok->next, ";"))
      return node;

    Node *n = expr(&tok, tok->next);
    if (current_fn->ty->return_ty->kind != TY_VOID)
      n = assign_cast(current_fn->ty->return_ty, n);
    node->m.lhs = n;
    *rest = skip(tok, ";");
    return node;
  }

  if (tok->kind == TK_if) {
    DeferStmt *dfr = new_stmt_scope();

    Node *node = new_node(ND_IF, tok);
    node->ctrl.cond = cond_cast(cond_declaration(&tok, skip(tok->next, "("), ")", 0));
    node->ctrl.then = secondary_block(&tok, tok);
    if (tok->kind == TK_else)
      node->ctrl.els = secondary_block(&tok, tok->next);

    *rest = tok;
    return leave_stmt_scope(dfr, node);
  }

  if (tok->kind == TK_switch) {
    DeferStmt *dfr = new_stmt_scope();

    Node *node = new_node(ND_SWITCH, tok);
    node->ctrl.cond = cond_declaration(&tok, skip(tok->next, "("), ")", 0);
    add_type(node->ctrl.cond);
    if (!is_integer(node->ctrl.cond->ty))
      error_tok(tok, "controlling expression not integer");

    JumpContext ctx = {.next = jump_ctx, .node = node};
    jump_ctx = &ctx;

    ctx.labels = label_list;
    ctx.dfr_lvl = current_defr;
    ctx.dfr_ctx = defer_context;

    node->ctrl.then = secondary_block(rest, tok);

    jump_ctx = ctx.next;
    return leave_stmt_scope(dfr, node);
  }

  if (tok->kind == TK_for) {
    DeferStmt *dfr = new_stmt_scope();

    Node *node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");

    if (is_typename(tok)) {
      Node *expr = declaration(&tok, tok);
      if (expr)
        node->ctrl.for_init = new_unary(ND_EXPR_STMT, expr, tok);
    } else {
      node->ctrl.for_init = expr_stmt(&tok, tok);
    }

    if (!consume(&tok, tok, ";")) {
      node->dfr_dest = current_defr;
      node->ctrl.cond = cond_cast(cond_declaration(&tok, tok, ";", 1));
      node->dfr_from = current_defr;
    }

    if (!equal(tok, ")"))
      node->ctrl.for_inc = expr(&tok, tok);
    tok = skip(tok, ")");

    loop_body(rest, tok, node, label_list);

    return leave_stmt_scope(dfr, node);
  }

  if (tok->kind == TK_while) {
    DeferStmt *dfr = new_stmt_scope();

    Node *node = new_node(ND_FOR, tok);
    node->dfr_dest = current_defr;
    node->ctrl.cond = cond_cast(cond_declaration(&tok, skip(tok->next, "("), ")", 1));
    node->dfr_from = current_defr;

    loop_body(rest, tok, node, label_list);

    return leave_stmt_scope(dfr, node);
  }

  if (tok->kind == TK_do) {
    DeferStmt *dfr = new_stmt_scope();

    Node *node = new_node(ND_DO, tok);

    loop_body(&tok, tok->next, node, label_list);

    tok = skip(tok, "while");
    tok = skip(tok, "(");
    node->ctrl.cond = cond_cast(expr(&tok, tok));
    tok = skip(tok, ")");
    *rest = skip(tok, ";");
    return leave_stmt_scope(dfr, node);
  }

  if (tok->kind == TK_asm) {
    Node *node = asm_stmt(&tok, tok);

    enter_tmp_scope();
    prepare_inline_asm(node);
    leave_scope();

    *rest = skip(tok, ";");
    return node;
  }

  if (tok->kind == TK_goto) {
    if (equal(tok->next, "*")) {
      // [GNU] `goto *ptr` jumps to the address specified by `ptr`.
      Node *node = new_node(ND_GOTO_EXPR, tok);
      node->m.lhs = expr(&tok, tok->next->next);
      *rest = skip(tok, ";");
      return node;
    }
    Node *node = new_node(ND_GOTO, ident_tok(&tok, tok->next));
    node->dfr_from = current_defr;

    push_goto(node);
    *rest = skip(tok, ";");
    return node;
  }

  if (tok->kind == TK_break || tok->kind == TK_continue) {
    bool is_cont = tok->kind == TK_continue;
    Node *node = new_node(is_cont ? ND_CONT : ND_BREAK, tok);
    JumpContext *ctx = resolve_labeled_jump(rest, tok, is_cont);
    if (ctx->dfr_ctx != defer_context)
      error_tok(tok, "illegal jump");
    node->dfr_dest = ctx->dfr_lvl;
    node->dfr_from = current_defr;

    node->lbl.next = ctx->node->ctrl.breaks;
    ctx->node->ctrl.breaks = node;
    return node;
  }

  if (tok->kind == TK_defer) {
    Token *prev = defer_context;
    defer_context = tok;
    Node *node = secondary_block(rest, tok->next);
    add_type(node);
    defer_context = prev;
    new_defr(DF_DEFER_STMT)->stmt = node;
    return new_node(ND_NULL_STMT, tok);
  }

  if (equal(tok, "{"))
    return compound_stmt(rest, tok, ND_BLOCK);

  return expr_stmt(rest, tok);
}

static void local_labels(Token **rest, Token *tok) {
  while (consume(&tok, tok, "__label__")) {
    bool first = true;
    for (; comma_list(&tok, &tok, ";", !first); first = false) {
      LocalLabel *ll = ast_arena_calloc(sizeof(LocalLabel));
      ll->name = ident_tok(&tok, tok);
      ll->next = scope->labels;
      scope->labels = ll;
    }
  }
  *rest = tok;
}

// compound-stmt = (typedef | declaration | stmt)* "}"
static Node *compound_stmt2(Token **rest, Token *tok, NodeKind kind) {
  Node *node = new_node(kind, tok);
  node->dfr_dest = current_defr;

  local_labels(&tok, tok->next);

  Node head = {0};
  Node *cur = &head;

  for (;;) {
    Token *label_list = label_stmt(&cur, &tok, tok);

    if (equal(tok, "}"))
      break;
    if (consume(&tok, tok, ";"))
      continue;
    if (pragma_pack(&tok, tok))
      continue;

    Node *node;
    if (is_typename(tok)) {
      Node *expr = declaration(&tok, tok);
      if (!expr)
        continue;
      node = new_unary(ND_EXPR_STMT, expr, tok);
    } else {
      node = stmt(&tok, tok, label_list);
    }
    cur = cur->next = node;
    add_type(cur);
  }

  if (cur->kind == ND_RETURN || cur->kind == ND_GOTO)
    current_defr = node->dfr_dest;

  node->dfr_from = current_defr;
  current_defr = node->dfr_dest;
  node->blk.local_labels = resolve_local_gotos();
  node->no_label = leave_scope();

  if (kind == ND_STMT_EXPR && cur->kind == ND_EXPR_STMT) {
    add_type(cur->m.lhs);
    Type *ty = cur->m.lhs->ty;
    if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
      Obj *var = new_lvar(ty);
      Node *expr = new_binary(ND_ASSIGN, new_var_node(var, tok), cur->m.lhs, tok);
      chain_expr(&expr, new_var_node(var, tok));
      cur->m.lhs = expr;
    }
  }

  node->blk.body = head.next;
  *rest = tok->next;
  return node;
}

static Node *compound_stmt(Token **rest, Token *tok, NodeKind kind) {
  enter_scope();
  return compound_stmt2(rest, tok, kind);
}

// expr-stmt = expr? ";"
static Node *expr_stmt(Token **rest, Token *tok) {
  if (consume(rest, tok, ";"))
    return new_node(ND_NULL_STMT, tok);

  Node *n = expr(&tok, tok);
  add_type(n);
  if (n->ty->size < 0 && n->ty->kind != TY_ARRAY)
    error_tok(n->tok, "expression has incomplete type");

  Node *node = new_node(ND_EXPR_STMT, tok);
  node->m.lhs = n;
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

static int64_t eval_error2(Node *node, char *fmt, ...) {
  if (eval_recover) {
    *eval_recover = true;
    return 0;
  }
  va_list ap;
  va_start(ap, fmt);
  verror_at_tok(node->tok, fmt, ap);
  va_end(ap);
  exit(1);
}

static int64_t eval_error(Node *node) {
  return eval_error2(node, "not a compile-time constant");
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
    if ((let_volatile || !(node->ty->qual & Q_VOLATILE)) &&
      (let_atomic || !(node->ty->qual & Q_ATOMIC))) {
      *ofs = 0;
      return node->m.var;
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
  if (!var->init_data || var->is_weak || !is_const_var(var))
    return false;
  for (Relocation *rel = var->rel; rel; rel = rel->next) {
    if ((rel->offset + ty_nullptr->size) <= ofs)
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

  if (!var || !(var->constexpr_data || var->is_string_lit ||
    is_static_const_var(var, ofs, node->ty->size)))
    return (char *)eval_error(node);

  int32_t access_sz = !is_bitfield(node) ? node->ty->size : bitfield_footprint(node->m.member);

  if (ofs < 0 || (var->ty->size < (ofs + access_sz)))
    return (char *)eval_error2(node, "constexpr access out of bounds");

  if (var->constexpr_data)
    return var->constexpr_data + ofs;

  return var->init_data + ofs;
}

int64_t eval_sign_extend(Type *ty, int64_t val) {
  switch (ty->size) {
  case 1: return ty->is_unsigned ? (uint8_t)val : (int64_t)(int8_t)val;
  case 2: return ty->is_unsigned ? (uint16_t)val : (int64_t)(int16_t)val;
  case 4: return ty->is_unsigned ? (uint32_t)val : (int64_t)(int32_t)val;
  }
  return val;
}

static void eval_void(Node *node) {
  if (node->kind == ND_VAR) {
    if (!node->m.var->constexpr_data)
      eval_error(node);
    return;
  }
  if (node->ty->kind == TY_BITINT)
    free(eval_bitint(node));
  else if (is_flonum(node->ty))
    eval_double(node);
  else
    eval(node);
}

static int64_t eval_cmp(Node *node) {
  Node *lhs = node->m.lhs;
  Node *rhs = node->m.rhs;

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
  Node *lhs = node->m.lhs;
  Node *rhs = node->m.rhs;

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
      return eval_error2(rhs, "division by zero during constant evaluation");
    if (ty->is_unsigned)
      return (uint64_t)lval / rval;
    if ((lval == INT64_MIN || lval == INT32_MIN) && rval == -1)
      return lval;
    return lval / rval;
  }
  case ND_MOD: {
    int64_t lval = eval(lhs);
    int64_t rval = eval(rhs);
    if (!rval)
      return eval_error2(rhs, "remainder by zero during constant evaluation");
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
    return eval(node->ctrl.cond) ? eval2(node->ctrl.then, ctx) : eval2(node->ctrl.els, ctx);
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

    int64_t val = eval2(lhs, ctx);
    if (ty->kind == TY_BOOL)
      return !!val;
    if (is_integer(ty))
      return eval_sign_extend(ty, val);
    return val;
  }
  case ND_NUM:
    return node->num.val;
  }

  if (ctx->kind == EV_AGGREGATE) {
    if (((ty->qual & Q_ATOMIC) && !ctx->let_atomic) ||
      ((ty->qual & Q_VOLATILE) && !ctx->let_volatile))
      return eval_error(node);

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
        return eval_error(node);
      ctx->let_array = false;
      ctx->deref_cnt = 0;
    }

    bool is_agg = ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION;

    if (node->kind == ND_MEMBER && (!ctx->deref_cnt || is_agg))
      return eval2(lhs, ctx) + node->m.member->offset;

    if (node->kind == ND_VAR && (!ctx->deref_cnt && is_agg)) {
      ctx->var = node->m.var;
      return 0;
    }
  }

  if (ctx->kind == EV_LABEL) {
    if (!ctx->label) {
      if (node->kind == ND_LABEL_VAL) {
        ctx->label = &node->lbl.unique_label;
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
        return ofs;
      }
    }
    return eval(node);
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
        if (!is_bitfield(node))
          return eval_sign_extend(ty, read_buf(data, ty->size));

        Member *mem = node->m.member;
        uint64_t val = 0;
        if (mem->is_aligned_bitfiled) {
          val = read_buf(data, next_pow_of_two(mem->bit_offset + mem->bit_width) / 8);
          val <<= (64 - mem->bit_width - mem->bit_offset);
        } else {
          int pofs = bitfield_footprint(mem) - 1;
          for (int start = pofs; pofs >= !!mem->bit_offset; pofs--) {
            if (pofs != start)
              val <<= 8;
            val |= (uint8_t)data[pofs];
          }
          if (mem->bit_offset) {
            val <<= 8 - mem->bit_offset;
            val |= (uint8_t)*data >> mem->bit_offset;
          }
          val <<= (64 - mem->bit_width);
        }
        if (ty->is_unsigned)
          return (uint64_t)val >> (64 - mem->bit_width);
        return (int64_t)val >> (64 - mem->bit_width);
      }
    }
  }
  return eval_error(node);
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

bool is_const_fp(Node *node, FPVal *fval) {
  bool failed = false;
  bool *prev = eval_recover;
  eval_recover = &failed;

  eval_fp(node, fval);

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

static int64_t align_expr(Token **rest, Token *tok) {
  int64_t val = const_expr2(rest, tok, NULL);
  if (!is_pow_of_two(val))
    error_tok(tok, "alignment not power of two");
  return val;
}

static long_double_t eval_fp_cast(long_double_t fval, Type *ty) {
  switch (ty->kind) {
  case TY_FLOAT: return (float)fval;
  case TY_DOUBLE: return (double)fval;
  case TY_LDOUBLE: return fval;
  }
  internal_error();
}

static void build_math_constant(Node *node, FPVal *fval) {
  switch (node->num.constant) {
  case MATH_CONSTANT_NANF: fval->chunk[0] = 0x7FC00000; return;
  case MATH_CONSTANT_INFF: fval->chunk[0] = 0x7F800000; return;
  case MATH_CONSTANT_NANSF: fval->chunk[0] = 0x7FA00000; return;
  case MATH_CONSTANT_NANS: fval->chunk[0] = 0x7FF4000000000000; return;
  case MATH_CONSTANT_NANSL: fval->chunk[1] = 0x7FFF;
    fval->chunk[0] = 0xA000000000000000; return;
  }
  internal_error();
}

static void eval_fp(Node *node, FPVal *fval) {
  add_type(node);

  while (node->kind == ND_CAST && node->ty->kind == node->m.lhs->ty->kind)
    node = node->m.lhs;

  if (node->kind == ND_NUM && node->num.constant) {
    if (fval)
      build_math_constant(node, fval);
    return;
  }

  long_double_t v = eval_double(node);
  if (fval) {
    switch (node->ty->kind) {
    case TY_FLOAT: fval->f = (float)v; break;
    case TY_DOUBLE: fval->d = (double)v; break;
    case TY_LDOUBLE: fval->ld = v; break;
    }
  }
}

static long_double_t eval_double(Node *node) {
  if (eval_recover && *eval_recover)
    return false;

  Type *ty = node->ty;
  Node *lhs = node->m.lhs;
  Node *rhs = node->m.rhs;

  switch (node->kind) {
  case ND_ADD: return eval_fp_cast(eval_double(lhs) + eval_double(rhs), ty);
  case ND_SUB: return eval_fp_cast(eval_double(lhs) - eval_double(rhs), ty);
  case ND_MUL: return eval_fp_cast(eval_double(lhs) * eval_double(rhs), ty);
  case ND_DIV: {
    long_double_t lval = eval_double(lhs);
    long_double_t rval = eval_double(rhs);
    if (rval == 0 && !is_global_init_context)
      break;
    return eval_fp_cast(lval / rval, ty);
  }
  case ND_POS:
    return eval_double(lhs);
  case ND_NEG:
    return -eval_double(lhs);
  case ND_COND:
    return eval(node->ctrl.cond) ? eval_double(node->ctrl.then) : eval_double(node->ctrl.els);
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
    if (node->num.constant) {
      FPVal fval;
      build_math_constant(node, &fval);

      switch (node->ty->kind) {
      case TY_FLOAT: return fval.f;
      case TY_DOUBLE: return fval.d;
      case TY_LDOUBLE: return fval.ld;
      }
      internal_error();
    }
    return node->num.fval;
  }

  char *data = eval_constexpr_data(node);
  if (data)
    return read_double_buf(data, ty);

  return eval_error(node);
}

static uint64_t *eval_bitint(Node *node) {
  if (eval_recover && *eval_recover)
    return NULL;

  Type *ty = node->ty;
  Node *lhs = node->m.lhs;
  Node *rhs = node->m.rhs;

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
        return (void *)eval_error2(node, "division by zero during constant evaluation");
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

    if (amount >= ty->bit_cnt)
      return (void *)eval_error2(rhs, "invalid shift amount");

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
    return eval(node->ctrl.cond) ? eval_bitint(node->ctrl.then) : eval_bitint(node->ctrl.els);
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
    memcpy(val, node->num.bitint_data, ty->size);
    return val;
  }
  }

  char *data = eval_constexpr_data(node);
  if (data) {
    uint64_t *val = malloc(MAX(ty->size, 8));
    if (is_bitfield(node)) {
      memcpy(val, data, bitfield_footprint(node->m.member));
      eval_bitint_bitfield_load(ty->bit_cnt, val, val,
        node->m.member->bit_width, node->m.member->bit_offset, ty->is_unsigned);
      return val;
    }
    memcpy(val, data, ty->size);
    return val;
  }

  return (void *)eval_error(node);
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

  Obj *addr = new_lvar(pointer_to(binary->m.lhs->ty));
  Obj *val = new_lvar(binary->m.rhs->ty);
  Obj *old = new_lvar(binary->m.lhs->ty);
  Obj *new = new_lvar(binary->m.lhs->ty);

  cur = cur->next =
    new_unary(ND_EXPR_STMT,
              new_binary(ND_ASSIGN, new_var_node(addr, tok),
                         new_unary(ND_ADDR, binary->m.lhs, tok), tok),
              tok);

  cur = cur->next =
    new_unary(ND_EXPR_STMT,
              new_binary(ND_ASSIGN, new_var_node(val, tok), binary->m.rhs, tok),
              tok);

  cur = cur->next =
    new_unary(ND_EXPR_STMT,
              new_binary(ND_ASSIGN, new_var_node(old, tok),
                         new_unary(ND_DEREF, new_var_node(addr, tok), tok), tok),
              tok);

  Node *loop = new_node(ND_DO, tok);

  Node *body = new_binary(ND_ASSIGN,
                          new_var_node(new, tok),
                          new_binary(binary->kind, new_var_node(old, tok),
                                     new_var_node(val, tok), tok),
                          tok);

  loop->ctrl.then = new_node(ND_BLOCK, tok);
  loop->ctrl.then->blk.body = new_unary(ND_EXPR_STMT, body, tok);

  Node *cas = new_node(ND_CAS, tok);
  cas->cas.addr = new_var_node(addr, tok);
  cas->cas.old_val = new_unary(ND_ADDR, new_var_node(old, tok), tok);
  cas->cas.new_val = new_var_node(new, tok);
  loop->ctrl.cond = new_unary(ND_NOT, cas, tok);

  cur = cur->next = loop;

  if (return_old)
    cur->next = new_unary(ND_EXPR_STMT, new_var_node(old, tok), tok);
  else
    cur->next = new_unary(ND_EXPR_STMT, new_var_node(new, tok), tok);

  Node *node = new_node(ND_STMT_EXPR, tok);
  node->blk.body = head.next;
  return node;
}

static Node *atomic_builtin_op(Token **rest, Token *tok, bool return_old) {
  Token *start = tok;
  tok = skip(tok->next, "(");
  Node *obj = new_unary(ND_DEREF, assign(&tok, tok), start);
  tok = skip(tok, ",");
  Node *val = assign(&tok, tok);
  if (consume(&tok, tok, ","))
    ident_tok(&tok, tok);
  *rest = skip(tok, ")");

  Node *binary;
  char *loc = start->loc + 23;
  int len = start->len - 23;

  if (equal_substr(loc, len, "add"))
    binary = new_add(obj, val, start);
  else if (equal_substr(loc, len, "sub"))
    binary = new_sub(obj, val, start);
  else if (equal_substr(loc, len, "and"))
    binary = new_binary(ND_BITAND, obj, val, start);
  else if (equal_substr(loc, len, "or"))
    binary = new_binary(ND_BITOR, obj, val, start);
  else if (equal_substr(loc, len, "xor"))
    binary = new_binary(ND_BITXOR, obj, val, start);
  else
    error_tok(start, "unsupported atomic op");

  add_type(binary->m.lhs);
  add_type(binary->m.rhs);
  return atomic_op(binary, return_old);
}

static Node *to_assign(Node *binary) {
  add_type(binary->m.lhs);

  if (binary->m.lhs->ty->qual & Q_ATOMIC)
    return atomic_op(binary, false);

  binary->arith_kind = binary->kind;
  binary->kind = ND_ARITH_ASSIGN;
  binary->ty = binary->m.lhs->ty;
  return binary;
}

// assign    = conditional (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^="
//           | "<<=" | ">>="
static Node *assign(Token **rest, Token *tok) {
  Node *node = conditional(&tok, tok);
  add_type(node);

  if ((node->ty->qual & Q_CONST) || is_array(node->ty)) {
    *rest = tok;
    return node;
  }

  // Convert A = B to (tmp = B, atomic_exchange(&A, tmp), tmp)
  if (equal(tok, "=") && (node->ty->qual & Q_ATOMIC)) {
    Node *rhs = assign(rest, tok->next);
    add_type(rhs);
    Obj *tmp = new_lvar(rhs->ty);
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

static Node *vla_cond_result_len2(Type *ty) {
  if (!ty->vla_len)
    return NULL;
  if (!ty->vla_cnt)
    internal_error();
  return new_var_node(ty->vla_cnt, ty->vla_len->tok);
}

Type *vla_cond_result_len(Type *ty1, Type *ty2, Type *base, Node **cond, Obj **cond_var) {
  Node *len1 = vla_cond_result_len2(ty1);
  Node *len2 = vla_cond_result_len2(ty2);

  if (!len1 + !len2 == 1)
    return vla_of(base, len1 ? len1 : len2, 0);

  if (!*cond_var) {
    int64_t val;
    if (is_const_expr(*cond, &val))
      return vla_of(base, val ? len1 : len2, 0);

    *cond_var = new_lvar2(ty_bool, base_scope());
    *cond = new_binary(ND_ASSIGN, new_var_node(*cond_var, (*cond)->tok), *cond, (*cond)->tok);
    add_type(*cond);
  }
  Node *node = new_node(ND_COND, (*cond)->tok);
  node->ctrl.cond = new_var_node(*cond_var, (*cond)->tok);
  node->ctrl.then = len1;
  node->ctrl.els = len2;
  return vla_of(base, node, 0);
}

// conditional = logor ("?" expr? ":" conditional)?
static Node *conditional(Token **rest, Token *tok) {
  Node *cond = binary(&tok, tok, PCD_LOGOR);

  if (!equal(tok, "?")) {
    *rest = tok;
    return cond;
  }
  Node *node = new_node(ND_COND, tok);
  if (!consume(&tok, tok->next, ":")) {
    node->ctrl.then = expr(&tok, tok->next);
    tok = skip(tok, ":");
  }
  node->ctrl.els = conditional(rest, tok);

  if (node->ctrl.then) {
    node->ctrl.cond = cond_cast(cond);
    return node;
  }
  int64_t val;
  Node n = *cond;
  if (is_const_expr(cond_cast(&n), &val)) {
    node->ctrl.cond = new_boolean(!!val, cond->tok);
    node->ctrl.then = cond;
    return node;
  }
  add_type(cond);
  enter_tmp_scope();
  Obj *var = new_lvar(cond->ty);
  node->ctrl.cond = cond_cast(new_binary(ND_ASSIGN, new_var_node(var, cond->tok), cond, tok));
  node->ctrl.then = new_var_node(var, cond->tok);
  leave_scope();
  return node;
}

static Node *binary(Token **rest, Token *tok, Preced stop) {
  Node *node = unary(&tok, tok);

  for (;;) {
    Token *start = tok;
    if (equal(tok, "*")) {
      node = new_binary(ND_MUL, node, unary(&tok, tok->next), start);
      continue;
    }
    if (equal(tok, "/")) {
      node = new_binary(ND_DIV, node, unary(&tok, tok->next), start);
      continue;
    }
    if (equal(tok, "%")) {
      node = new_binary(ND_MOD, node, unary(&tok, tok->next), start);
      continue;
    }
    if (stop == PCD_MUL)
      return *rest = tok, node;
    break;
  }

  for (;;) {
    Token *start = tok;
    if (equal(tok, "+")) {
      node = new_add(node, binary(&tok, tok->next, PCD_MUL), start);
      continue;
    }
    if (equal(tok, "-")) {
      node = new_sub(node, binary(&tok, tok->next, PCD_MUL), start);
      continue;
    }
    if (stop == PCD_ADD)
      return *rest = tok, node;
    break;
  }

  for (;;) {
    Token *start = tok;
    if (equal(tok, "<<")) {
      node = new_binary(ND_SHL, node, binary(&tok, tok->next, PCD_ADD), start);
      continue;
    }
    if (equal(tok, ">>")) {
      add_type(node);
      if (node->ty->is_unsigned)
        node = new_binary(ND_SHR, node, binary(&tok, tok->next, PCD_ADD), start);
      else
        node = new_binary(ND_SAR, node, binary(&tok, tok->next, PCD_ADD), start);
      continue;
    }
    if (stop == PCD_SHFT)
      return *rest = tok, node;
    break;
  }

  for (;;) {
    Token *start = tok;
    if (equal(tok, "<")) {
      node = new_binary(ND_LT, node, binary(&tok, tok->next, PCD_SHFT), start);
      continue;
    }
    if (equal(tok, "<=")) {
      node = new_binary(ND_LE, node, binary(&tok, tok->next, PCD_SHFT), start);
      continue;
    }
    if (equal(tok, ">")) {
      node = new_binary(ND_GT, node, binary(&tok, tok->next, PCD_SHFT), start);
      continue;
    }
    if (equal(tok, ">=")) {
      node = new_binary(ND_GE, node, binary(&tok, tok->next, PCD_SHFT), start);
      continue;
    }
    if (stop == PCD_CMP)
      return *rest = tok, node;
    break;
  }

  for (;;) {
    Token *start = tok;
    if (equal(tok, "==")) {
      node = new_binary(ND_EQ, node, binary(&tok, tok->next, PCD_CMP), start);
      continue;
    }
    if (equal(tok, "!=")) {
      node = new_binary(ND_NE, node, binary(&tok, tok->next, PCD_CMP), start);
      continue;
    }
    if (stop == PCD_EQ)
      return *rest = tok, node;
    break;
  }

  for (;;) {
    Token *start = tok;
    if (equal(tok, "&")) {
      node = new_binary(ND_BITAND, node, binary(&tok, tok->next, PCD_EQ), start);
      continue;
    }
    if (stop == PCD_BITAND)
      return *rest = tok, node;
    break;
  }

  for (;;) {
    Token *start = tok;
    if (equal(tok, "^")) {
      node = new_binary(ND_BITXOR, node, binary(&tok, tok->next, PCD_BITAND), start);
      continue;
    }
    if (stop == PCD_XOR)
      return *rest = tok, node;
    break;
  }

  for (;;) {
    Token *start = tok;
    if (equal(tok, "|")) {
      node = new_binary(ND_BITOR, node, binary(&tok, tok->next, PCD_XOR), start);
      continue;
    }
    if (stop == PCD_BITOR)
      return *rest = tok, node;
    break;
  }

  for (;;) {
    Token *start = tok;
    if (equal(tok, "&&")) {
      node = new_binary(ND_LOGAND,
        cond_cast(node), cond_cast(binary(&tok, tok->next, PCD_BITOR)), start);
      continue;
    }
    if (stop == PCD_LOGAND)
      return *rest = tok, node;
    break;
  }

  for (;;) {
    Token *start = tok;
    if (equal(tok, "||")) {
      node = new_binary(ND_LOGOR,
        cond_cast(node), cond_cast(binary(&tok, tok->next, PCD_LOGAND)), start);
      continue;
    }
    break;
  }
  *rest = tok;
  return node;
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
      *ofs = new_binary(ND_MUL, *ofs, base_size(ptr->ty->base, tok), tok);

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
      rhs = new_binary(ND_MUL, rhs, base_size(lhs->ty->base, tok), tok);

    return new_binary(ND_SUB, lhs, rhs, tok);
  }

  // ptr - ptr, which returns how many elements are between the two.
  if (lhs->ty->base && rhs->ty->base && is_compatible(lhs->ty->base, rhs->ty->base)) {
    Node *node = new_binary(ND_SUB, lhs, rhs, tok);
    node = new_binary(ND_DIV, node, base_size(lhs->ty->base, tok), tok);
    node->ty = node->m.lhs->ty = node->m.rhs->ty = ty_ptrdiff_t;
    return node;
  }

  error_tok(tok, "invalid operands");
}

static Node *unary(Token **rest, Token *tok) {
  // Casts
  {
    Type *ty;
    if (is_typename_paren(&tok, tok, &ty)) {
      Node *calc = calc_vla(ty, tok);
      Node *node = new_cast(unary(rest, tok), ty);
      if (calc)
        return new_binary(ND_COMMA, calc, node, tok);
      return node;
    }
  }

  if (equal(tok, "+"))
    return new_unary(ND_POS, unary(rest, tok->next), tok);

  if (equal(tok, "-"))
    return new_unary(ND_NEG, unary(rest, tok->next), tok);

  if (equal(tok, "&")) {
    Node *lhs = unary(rest, tok->next);
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
    Node *node = unary(rest, tok->next);
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
    return new_unary(ND_NOT, cond_cast(unary(rest, tok->next)), tok);

  if (equal(tok, "~"))
    return new_unary(ND_BITNOT, unary(rest, tok->next), tok);

  // Read ++i as i+=1
  if (equal(tok, "++"))
    return to_assign(new_add(unary(rest, tok->next), new_num(1, tok), tok));

  // Read --i as i-=1
  if (equal(tok, "--"))
    return to_assign(new_sub(unary(rest, tok->next), new_num(1, tok), tok));

  // [GNU] labels-as-values
  if (equal(tok, "&&")) {
    Node *node = new_node(ND_LABEL_VAL, ident_tok(rest, tok->next));
    push_goto(node);
    dont_dealloc_vla = true;
    return node;
  }

  if (tok->kind == TK_alignof) {
    Type *ty;
    VarAttr attr = {0};
    int attr_align = 0;
    if (is_typename_paren2(rest, tok->next, &ty, &attr)) {
      attr_align = attr.align;
    } else {
      Node *node = unary(rest, tok->next);
      switch (node->kind) {
      case ND_MEMBER:
        return new_size_t(MAX(node->m.member->ty->align, node->m.member->alt_align), tok);
      case ND_VAR:
        return new_size_t(node->m.var->alt_align ? node->m.var->alt_align : node->m.var->ty->align, tok);
      }
      add_type(node);
      ty = node->ty;
    }
    while (is_array(ty))
      ty = ty->base;
    return new_size_t(MAX(ty->align, attr_align), tok);
  }

  if (tok->kind == TK_Countof) {
    Type *ty;
    if (!is_typename_paren(rest, tok->next, &ty)) {
      Node *node = unary(rest, tok->next);
      add_type(node);
      ty = node->ty;

      if (ty->kind == TY_VLA && ty->vla_len)
        return new_binary(ND_COMMA, node, vla_count(ty, tok, false), tok);
    }
    if (ty->kind == TY_VLA)
      return vla_count(ty, tok, false);

    if (ty->kind == TY_ARRAY) {
      if (ty->size < 0)
        error_tok(tok, "countof applied to incomplete array");
      return new_size_t(ty->array_len, tok);
    }
    error_tok(tok, "countof applied to non-array type");
  }

  if (tok->kind == TK_sizeof) {
    Type *ty;
    if (!is_typename_paren(rest, tok->next, &ty)) {
      Node *node = unary(rest, tok->next);
      add_type(node);
      ty = node->ty;

      if (ty->kind == TY_VLA)
        return new_binary(ND_COMMA, node, vla_size(ty, tok), tok);
    }
    if (ty->kind == TY_VLA)
      return vla_size(ty, tok);

    if (ty->is_flexible && ty->origin)
      ty = ty->origin;
    if (ty->size < 0)
      error_tok(tok, "sizeof applied to incomplete type");
    return new_size_t(ty->size, tok);
  }

  if (tok->kind == TK_static_assert) {
    eval_static_assert(rest, tok->next);
    return new_node(ND_NULL_EXPR, tok);
  }

  Node *node = primary(&tok, tok);
  return postfix(node, rest, tok);
}

static void chk_mem_name2(HashMap *map, Token *name) {
  HashEntry *ent = hashmap_get_or_insert(map, name->loc, name->len);
  Token *prv = ent->val;
  if (prv) {
    notice_tok(name, "duplicated member name");
    error_tok(prv, "previously declared here");
  }
  ent->val = name;
}

static void chk_mem_name(HashMap *map, Member *members) {
  for (Member *mem = members; mem; mem = mem->next) {
    if (mem->name)
      chk_mem_name2(map, mem->name);
    else if (mem->ty->kind == TY_STRUCT || mem->ty->kind == TY_UNION)
      chk_mem_name(map, mem->ty->members);
  }
}

// struct-members = (declspec declarator (","  declarator)* ";")*
static void struct_members(Token **rest, Token *tok, Type *ty) {
  Member head = {0};
  Member *cur = &head;
  Token *flex_tok = NULL;
  HashMap names = {0};

  while (!equal(tok, "}")) {
    if (consume(&tok, tok, ";"))
      continue;
    if (pragma_pack(&tok, tok))
      continue;
    if (tok->kind == TK_static_assert) {
      eval_static_assert(&tok, tok->next);
      tok = skip(tok, ";");
      continue;
    }

    VarAttr attr = {0};
    Type *basety = declspec(&tok, tok, &attr, SC_NONE);

    // Anonymous struct member
    if (equal(tok, ";") && (basety->kind == TY_STRUCT || basety->kind == TY_UNION)) {
      if (basety->size < 0)
        error_tok(tok, "member has incomplete type");
      if (basety->tag && !opt_ms_anon_struct)
        error_tok(tok, "enable MSVC anonymous struct extension with `-fms-anon-struct`");
      chk_mem_name(&names, basety->members);

      Member *mem = calloc(1, sizeof(Member));
      mem->ty = basety;

      tok = tok->next;
      cur = cur->next = mem;
      continue;
    }

    // Regular struct members
    bool first = true;
    for (; comma_list(&tok, &tok, ";", !first); first = false) {
      Member *mem = calloc(1, sizeof(Member));
      mem->is_packed = attr.is_packed;
      mem->ty = declarator(&tok, tok, basety, &mem->name);
      if (mem->name) {
        chk_mem_name2(&names, mem->name);
        mem->name->is_live = true;
      }
      aligned_attr(mem->name, tok, &attr, &mem->alt_align);

      if (is_vm_ty(mem->ty) || mem->ty->kind == TY_FUNC || mem->ty->kind == TY_VOID)
        error_tok(tok, "invalid member type");

      bool_attr(tok, TK_BATTR, "packed", &mem->is_packed);

      if (consume(&tok, tok, ":")) {
        mem->is_bitfield = true;
        mem->bit_width = const_expr(&tok, tok);
        if (mem->bit_width < 0)
          error_tok(tok, "bit-field with negative width");
        attr_aligned(tok, &mem->alt_align, TK_ATTR);
      }
      bool_attr(tok, TK_ATTR, "packed", &mem->is_packed);
      cur = cur->next = mem;

      if (mem->ty->size < 0) {
        if (mem->ty->kind == TY_ARRAY && ty->kind == TY_STRUCT && !flex_tok) {
          flex_tok = tok;
          continue;
        }
        error_tok(flex_tok ? flex_tok : tok, "member has incomplete type");
      }
    }
  }
  free(names.buckets);
  *rest = tok->next;

  if (flex_tok) {
    if (cur->ty->size >= 0)
      error_tok(flex_tok, "member has incomplete type");
    ty->is_flexible = true;
  }
  ty->members = head.next;
}

static Type *struct_tag(TypeKind kind, Token *tag, Token *tok, Type **tag_compat_ty) {
  Type *tag_ty;
  if (equal(tok, "{") || equal(tok, ";"))
    tag_ty = find_tag_in_scope(tag);
  else
    tag_ty = find_tag(tag);

  if (!tag_ty) {
    Type *ty = new_type(kind, -1, 1);
    push_tag_scope(tag, ty);
    return ty;
  }

  if (tag_ty->kind != kind)
    error_tok(tag, "conflict of tag type");

  if (equal(tok, "{")) {
    if (tag_ty->is_constructing)
      error_tok(tag, "nested redifinition");

    if (tag_ty->size >= 0) {
      if (opt_std < STD_C23)
        error_tok(tag, "tag redeclaration");
      *tag_compat_ty = tag_ty;
      return new_type(kind, -1, 1);
    }
  }
  return tag_ty;
}

static Type *struct_union_decl(Token **rest, Token *tok, TypeKind kind) {
  bool is_packed = false;
  bool_attr(tok, TK_ATTR, "packed", &is_packed);
  bool_attr(tok, TK_BATTR, "packed", &is_packed);

  int alt_align = 0;
  attr_aligned(tok, &alt_align, TK_ATTR);
  attr_aligned(tok, &alt_align, TK_BATTR);

  // Read a tag.
  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  Type *tag_compat_ty = NULL;
  Type *ty;
  if (!tag) {
    ty = new_type(kind, -1, 1);
  } else {
    ty = struct_tag(kind, tag, tok, &tag_compat_ty);

    if (!equal(tok, "{")) {
      *rest = tok;
      return ty;
    }
    ty->is_constructing = true;
  }

  struct_members(&tok, skip(tok, "{"), ty);

  attr_aligned(tok, &alt_align, TK_ATTR);
  bool_attr(tok, TK_ATTR, "packed", &is_packed);
  *rest = tok;

  int pack_align = is_packed ? 1 :
    pack_stk.cnt ? pack_stk.data[pack_stk.cnt - 1] : 0;

  if (kind == TY_STRUCT)
    ty = struct_decl(ty, alt_align, pack_align);
  else
    ty = union_decl(ty, alt_align, pack_align);

  if (tag) {
    ty->is_constructing = false;

    if (tag_compat_ty) {
      if (!is_record_compat(ty, tag_compat_ty))
        error_tok(tag, "tag redeclaration");
      return tag_compat_ty;
    }

    for (Type *t = ty->decl_next; t;) {
      Type *nxt = t->decl_next;
      new_derived_type(t, t->qual, ty);
      t = nxt;
    }
  }
  return ty;
}

static Type *struct_decl(Type *ty, int alt_align, int pack_align) {
  int bits = 0;

  for (Member *mem = ty->members; mem; mem = mem->next) {
    int mem_align = mem->is_packed ? 1 :
      (pack_align > 0 && pack_align < mem->ty->align) ? pack_align : mem->ty->align;

    if (!mem->is_bitfield || mem->name) {
      alt_align = MAX(alt_align, mem_align);
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
      if (mem->is_packed || pack_align) {
        int ceil = align_to(bits + mem->bit_width, 8);
        for (int rsz = 8; rsz <= 64; rsz *= 2) {
          if (ceil % rsz != 0)
            break;
          int mofs = (ceil - rsz) / 8;
          if (mofs * 8 > bits)
            continue;
          mem->offset = mofs;
          mem->is_aligned_bitfiled = true;
          break;
        }
        if (!mem->is_aligned_bitfiled)
          mem->offset = bits / 8;
      } else {
        int bsz = mem->ty->size * 8;
        if (bits / bsz != (bits + mem->bit_width - 1) / bsz)
          bits = align_to(bits, bsz);
        mem->offset = bits / bsz * mem->ty->size;
        mem->is_aligned_bitfiled = true;
      }

      mem->bit_offset = bits - mem->offset * 8;
      bits += mem->bit_width;
      continue;
    }
    bits = align_to(bits, mem_align * 8);
    mem->offset = bits / 8;

    if (mem->ty->size < 0)  {
      if (!mem->next && ty->is_flexible)
        break;
      internal_error();
    }
    bits += mem->ty->size * 8;
  }
  ty->size = MAX(ty->size, 0);

  if (alt_align)
    ty->align = alt_align;
  if (!alt_align && pack_align > 0)
    ty->size = align_to(bits, pack_align * 8) / 8;
  else
    ty->size = align_to(bits, ty->align * 8) / 8;
  return ty;
}

static Type *union_decl(Type *ty, int alt_align, int pack_align) {
  for (Member *mem = ty->members; mem; mem = mem->next) {
    int mem_align = mem->is_packed ? 1 :
      (pack_align > 0 && pack_align < mem->ty->align) ? pack_align : mem->ty->align;

    if (!mem->is_bitfield || mem->name) {
      alt_align = MAX(alt_align, mem_align);
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
    if (mem->name) {
      // Regular struct member
      if (equal_tok(mem->name, tok))
        return mem;
    } else {
      // Anonymous struct member
      if ((mem->ty->kind == TY_STRUCT || mem->ty->kind == TY_UNION) &&
        get_struct_member(mem->ty, tok))
        return mem;
    }
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
    node->m.member = mem;
    apply_cv_qualifier(node, ty);

    if (mem->name)
      break;
    ty = mem->ty;
  }
  return node;
}

static Node *new_inc_dec(Node *node, Token *tok, int addend) {
  add_type(node);

  if (node->ty->qual & Q_ATOMIC)
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
  node->ty = node->m.lhs->ty;
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

  Type *ty = get_func_ty(fn->ty);
  if (!ty)
    error_tok(fn->tok, "not a function");

  if (fn->kind == ND_VAR && fn->m.var->returns_twice)
    current_fn->dont_reuse_stk = true;

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
    cur = cur->param_next = alloc_lvar(arg->ty);
    cur->arg_expr = arg;
  }
  if (param)
    error_tok(tok, "too few arguments");

  Node *node = new_node(ND_FUNCALL, tok);
  node->call.expr = fn;
  node->ty = ty->return_ty;
  node->call.args = head.param_next;

  prepare_funcall(node, scope);
  leave_scope();

  // If a function returns a struct, it is caller's responsibility
  // to allocate a space for the return value.
  if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION ||
    (node->ty->kind == TY_BITINT && bitint_rtn_need_copy(node->ty->bit_cnt)))
    node->call.rtn_buf = new_lvar(node->ty);
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

    Type *t2 = declspec(&tok, tok, &(VarAttr){0}, SC_NONE);
    t2 = declarator2(&tok, tok, t2, NULL, &(DeclContext){.let_star = true});

    Node *node = assign(&tok, skip(tok, ":"));
    if (is_compatible2(t1, t2)) {
      if (ret) {
        notice_tok(ret->tok, "ambiguous _Generic selection");
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
  node->m.lhs = assign(&tok, tok);
  tok = skip(tok, ",");
  node->m.rhs = assign(&tok, tok);
  tok = skip(tok, ",");
  node->m.target = assign(&tok, tok);
  *rest = skip(tok, ")");
  add_type(node);

  Token *bad_tok = NULL;
  if (node->m.target->ty->kind != TY_PTR || node->m.target->ty->base->kind == TY_BOOL ||
    !is_int_class(node->m.target->ty->base))
    bad_tok = node->m.target->tok;
  else if (!is_int_class(node->m.lhs->ty))
    bad_tok = node->m.lhs->tok;
  else if (!is_int_class(node->m.rhs->ty))
    bad_tok = node->m.rhs->tok;
  if (bad_tok)
    error_tok(bad_tok, "operand invalid for integer overflow arithmetic");

  return node;
}

static Node *compound_literal(Token **rest, Token *tok) {
  Token *start = tok;
  VarAttr attr = {0};
  Type *ty = declspec(&tok, tok->next, &attr,
    SC_CONSTEXPR | SC_REGISTER | SC_STATIC | SC_THREAD);

  ty = declarator(&tok, tok, ty, NULL);
  tok = skip(tok, ")");

  if (ty->kind == TY_VOID || ty->kind == TY_FUNC ||
    ty->kind == TY_VLA || (ty->size < 0 && ty->kind != TY_ARRAY))
    error_tok(tok, "invalid compound literal type");

  if (is_global_init_context || (attr.strg & SC_STATIC)) {
    Obj *var = new_anon_gvar(ty);
    var->is_compound_lit = true;
    var->is_tls = attr.strg & SC_THREAD;
    var->alt_align = attr.align;

    if (attr.strg & SC_CONSTEXPR)
      constexpr_initializer(rest, tok, var, var);
    else
      gvar_initializer(rest, tok, var);

    return new_var_node(var, start);
  }

  Obj *var = new_lvar2(ty, decl_scope());
  var->is_compound_lit = true;
  var->alt_align = attr.align;

  Node *init;
  if (attr.strg & SC_CONSTEXPR) {
    Obj *init_var = new_anon_gvar(ty);
    constexpr_initializer(&tok, tok, init_var, var);
    init = new_binary(ND_ASSIGN, new_var_node(var, tok), new_var_node(init_var, tok), tok);
  } else {
    init = lvar_initializer(&tok, tok, var);
  }
  *rest = tok;
  return new_binary(ND_CHAIN, init, new_var_node(var, tok), start);
}

static Node *builtin_functions(Token **rest, Token *tok) {
  if (equal(tok, "__builtin_alloca")) {
    Node *node = new_node(ND_ALLOCA, tok);
    tok = skip(tok->next, "(");
    node->m.lhs = assign(&tok, tok);
    *rest = skip(tok, ")");
    node->ty = pointer_to(ty_void);
    dont_dealloc_vla = true;
    return node;
  }

  if (equal(tok, "__builtin_frame_address")) {
    Node *node = new_node(ND_FRAME_ADDR, tok);
    node->ty = pointer_to(ty_void);
    tok = skip(tok->next, "(");
    int64_t val;
    if (!is_const_expr(expr(&tok, tok), &val))
      error_tok(tok, "expected integer constant expression");
    node->num.val = val;
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_return_address")) {
    Node *node = new_node(ND_RTN_ADDR, tok);
    node->ty = pointer_to(ty_void);
    tok = skip(tok->next, "(");
    int64_t val;
    if (!is_const_expr(expr(&tok, tok), &val))
      error_tok(tok, "expected integer constant expression");
    node->num.val = val;
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_extract_return_addr")) {
    tok = skip(tok->next, "(");
    Node *node = assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_atomic_chk")) {
    tok = skip(tok->next, "(");
    Node *node = assign(&tok, tok);
    *rest = skip(tok, ")");
    add_type(node);
    if (node->ty->kind != TY_PTR || node->ty->base->kind == TY_VOID)
      error_tok(tok, "expected pointer to non-void type");
    Type *base = node->ty->base;
    if (!(base->qual & Q_ATOMIC))
      node->ty = pointer_to(qual_type(Q_ATOMIC, base));
    return node;
  }

  if (!strncmp(tok->loc, "__builtin_atomic_arith_", 23))
    return atomic_builtin_op(rest, tok, false);

  if (!strncmp(tok->loc, "__builtin_atomic_fetch_", 23))
    return atomic_builtin_op(rest, tok, true);

  if (equal(tok, "__builtin_atomic_thread_fence")) {
    Node *node = new_node(ND_THREAD_FENCE, tok);
    *rest = skip(skip(tok->next, "("), ")");
    return node;
  }

  if (equal(tok, "__builtin_constant_p")) {
    Node *node = new_node(ND_NUM, tok);
    tok = skip(tok->next, "(");
    Node *exp = conditional(&tok, tok);
    add_type(exp);

    if (((is_integer(exp->ty) || is_ptr(exp->ty)) && is_const_expr(exp, NULL)) ||
      (is_flonum(exp->ty) && is_const_fp(exp, NULL)) ||
      (exp->kind == ND_VAR && exp->m.var->is_string_lit))
      node->num.val = 1;

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
    Token dot = {.kind = TK_PUNCT, .loc = ".", .len = 1, .next = tok, .origin = tok};
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
    return new_num(is_compatible(t1, t2), tok);
  }

  if (equal(tok, "__builtin_unreachable")) {
    Node *node = new_node(ND_UNREACHABLE, tok);
    *rest = skip(skip(tok->next, "("), ")");
    return node;
  }

  if (equal(tok, "__builtin_c23_va_start")) {
    Node *node = new_node(ND_VA_START, tok);
    tok = skip(tok->next, "(");
    node->m.lhs = conditional(&tok, tok);
    if (equal(tok, ","))
      assign(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_va_start")) {
    Node *node = new_node(ND_VA_START, tok);
    tok = skip(tok->next, "(");
    node->m.lhs = conditional(&tok, tok);
    assign(&tok, skip(tok, ","));
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_va_copy")) {
    Node *node = new_node(ND_VA_COPY, tok);
    tok = skip(tok->next, "(");
    node->m.lhs = conditional(&tok, tok);
    tok = skip(tok, ",");
    node->m.rhs = conditional(&tok, tok);
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
    node->m.lhs = ap_arg;
    tok = skip(tok, ",");

    Type *ty = typename(&tok, tok);
    *rest = skip(tok, ")");

    if (va_arg_need_copy(ty))
      node->m.var = new_lvar(ty);

    node->ty = pointer_to(ty);
    node = new_unary(ND_DEREF, node, tok);

    Node *calc = calc_vla(ty, tok);
    if (calc)
      return new_binary(ND_COMMA, calc, node, tok);
    return node;
  }

  if (equal(tok, "__builtin_compare_and_swap")) {
    Node *node = new_node(ND_CAS, tok);
    tok = skip(tok->next, "(");
    node->cas.addr = assign(&tok, tok);
    tok = skip(tok, ",");
    node->cas.old_val = assign(&tok, tok);
    tok = skip(tok, ",");
    node->cas.new_val = assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (equal(tok, "__builtin_atomic_exchange")) {
    Node *node = new_node(ND_EXCH, tok);
    tok = skip(tok->next, "(");
    node->m.lhs = assign(&tok, tok);
    tok = skip(tok, ",");
    node->m.rhs = assign(&tok, tok);
    *rest = skip(tok, ")");
    return node;
  }

  if (!strncmp(tok->loc, "__builtin_math_constant_", 24)) {
    *rest = skip(skip(tok->next, "("), ")");

    Node *node = new_node(ND_NUM, tok);
    char *loc = tok->loc + 24;
    int len = tok->len - 24;

    if (equal_substr(loc, len, "nanf")) {
      node->num.constant = MATH_CONSTANT_NANF;
      node->ty = ty_float;
    } else if (equal_substr(loc, len, "inff")) {
      node->num.constant = MATH_CONSTANT_INFF;
      node->ty = ty_float;
    } else if (equal_substr(loc, len, "nansf")) {
      node->num.constant = MATH_CONSTANT_NANSF;
      node->ty = ty_float;
    } else if (equal_substr(loc, len, "nans")) {
      node->num.constant = MATH_CONSTANT_NANS;
      node->ty = ty_double;
    } else if (equal_substr(loc, len, "nansl")) {
      node->num.constant = MATH_CONSTANT_NANSL;
      node->ty = ty_ldouble;
    } else {
      error_tok(tok, "unknown math constant");
    }
    return node;
  }

  error_tok(tok, "implicit declaration of a function");
}

static Node *primary(Token **rest, Token *tok) {
  if (equal(tok, "(")) {
    if (is_typename(tok->next))
      return compound_literal(rest, tok);

    if (equal(tok->next, "{")) {
      if (is_global_init_context || !scope->parent)
        error_tok(tok, "statement expresssion in constant context");

      Node *node = compound_stmt(&tok, tok->next, ND_STMT_EXPR);
      *rest = skip(tok, ")");
      return node;
    }

    Node *node = expr(&tok, tok->next);
    *rest = skip(tok, ")");
    return node;
  }

  if (tok->kind == TK_Generic)
    return generic_selection(rest, tok->next);

  if (tok->kind == TK_IDENT) {
    // Variable or enum constant
    VarScope *sc = find_var(tok);
    *rest = tok->next;

    if (sc) {
      if (sc->var)
        return new_var_node(sc->var, tok);
      if (sc->enum_ty) {
        Node *n = new_num(sc->enum_val, tok);
        n->ty = (sc->enum_ty->is_int_enum) ? ty_int : sc->enum_ty;
        return n;
      }
    }

    if (equal(tok->next, "("))
      return builtin_functions(rest, tok);

    error_tok(tok, "undefined variable");
  }

  if (tok->kind == TK_STR) {
    Obj *var = new_anon_gvar(tok->ty);
    var->init_data = tok->str;
    var->is_string_lit = true;
    *rest = tok->next;
    Node *n = new_var_node(var, tok);
    add_type(n);
    return n;
  }

  if (tok->kind == TK_true || tok->kind == TK_false) {
    *rest = tok->next;
    return new_boolean(tok->kind == TK_true, tok);
  }

  if (tok->kind == TK_nullptr) {
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

  if (tok->kind == TK_FUNCTION) {
    if (!current_fn)
      error_tok(tok, "not in function");
    if (!current_fnname) {
      char *name = current_fn->name;
      current_fnname = new_static_lvar(array_of(ty_pchar, strlen(name) + 1));
      current_fnname->init_data = name;
    }
    *rest = tok->next;
    return new_var_node(current_fnname, tok);
  }

  error_tok(tok, "expected an expression");
}

static Node *parse_typedef(Token **rest, Token *tok, Type *basety, VarAttr *attr) {
  Node *node = NULL;
  bool first = true;
  for (; comma_list(rest, &tok, ";", !first); first = false) {
    Token *name = NULL;
    Type *ty = declarator(&tok, tok, basety, &name);

    if (!name)
      error_tok(tok, "typedef name omitted");

    int align = 0;
    aligned_attr(name, tok, attr, &align);

    HashEntry *ent = hashmap_get_or_insert(&scope->vars, name->loc, name->len);
    VarScope *vsc = ent->val;
    if (vsc) {
      if (!vsc->type_def)
        error_tok(name, "redeclaration of '%.*s'", name->len, name->loc);
      if (!is_compatible2(ty, vsc->type_def))
        error_tok(name, "incompatible redeclaration");
      if (is_vm_ty(ty) || is_vm_ty(vsc->type_def))
        error_tok(name, "redefining typedef of variably-modified type");
      if (vsc->type_def_align != align)
        error_tok(name, "conflict of typedef alignment");
    } else {
      vsc = ent->val = ast_arena_calloc(sizeof(VarScope));
      vsc->type_def = ty;
      vsc->type_def_align = align;
      chain_expr(&node, calc_vla(ty, tok));
    }
  }
  return node;
}

static void resolve_goto_defer(Node *node, DeferStmt *dst_dfr) {
  if (!dst_dfr)
    return;

  DeferStmt *dfr = node->dfr_from;
  for (; dfr; dfr = dfr->next)
    if (dfr == dst_dfr)
      break;
  if (!dfr)
    error_tok(node->tok->next, "illegal jump");

  node->dfr_dest = dfr;
}

static void resolve_gotos(void) {
  HashMap label_cache = {0};
  for (Node *x = gotos; x; x = x->lbl.next) {
    Node *dest = hashmap_get2(&label_cache, x->tok->loc, x->tok->len);
    if (!dest) {
      for (Node *lbl = labels; lbl; lbl = lbl->lbl.next) {
        if (!equal_tok(lbl->tok, x->tok))
          continue;
        if (dest)
          error_tok(x->tok, "duplicated label");
        dest = lbl;
      }
      if (!dest)
        error_tok(x->tok, "use of undeclared label");

      hashmap_put2(&label_cache, x->tok->loc, x->tok->len, dest);
    }
    x->lbl.unique_label = dest->lbl.unique_label;
    resolve_goto_defer(x, dest->dfr_from);
  }
  free(label_cache.buckets);
  gotos = NULL;
}

static Node *resolve_local_gotos(void) {
  for (Node *x = scope->gotos; x; x = x->lbl.next) {
    LocalLabel *ll = find_local_label(scope, x->tok);
    if (!ll || !ll->label)
      error_tok(x->tok, "use of undeclared local label");

    x->lbl.node = ll->label;
    resolve_goto_defer(x, ll->label->dfr_from);
  }

  Node head = {0};
  Node *cur = &head;
  for (LocalLabel *ll = scope->labels; ll; ll = ll->next)
    if (ll->label)
      cur = cur->lbl.next = ll->label;
  return head.lbl.next;
}

static Obj *func_prototype(Type *ty, VarAttr *attr, Token *name) {
  if (scope->parent && (attr->strg & SC_STATIC))
    error_tok(name, "static function not in file scope");

  HashEntry *ent = hashmap_get_or_insert(&symbols, name->loc, name->len);
  Obj *fn = ent->val;
  if (fn) {
    if (!is_compatible(fn->ty, ty))
      error_tok(name, "incompatible redeclaration");
    if (!fn->is_static && (attr->strg & SC_STATIC))
      error_tok(name, "static declaration follows a non-static declaration");
    if (fn->ty->is_oldstyle && !ty->is_oldstyle)
      fn->ty = ty;
  } else {
    fn = ent->val = new_gvar(get_ident(name), ty);
    fn->is_static = attr->strg & SC_STATIC;

    if (strstr(fn->name, "setjmp") || strstr(fn->name, "savectx") ||
      strstr(fn->name, "vfork") || strstr(fn->name, "getcontext"))
      fn->returns_twice = true;
  }
  push_gvar_name(name, fn);
  return fn;
}

static Obj *find_param(Token *name, Obj *list) {
  for (Obj *p = list; p; p = p->param_next)
    if (equal(name, p->name))
      return p;
  return NULL;
}

static Node *func_old_style_param(Token **rest, Token *tok, Type *prot_ty, Type *def_ty) {
  if (!prot_ty->is_oldstyle && def_ty->is_oldstyle) {
    Obj *p1 = prot_ty->param_list;
    Obj *p2 = def_ty->param_list;
    while (p1 && p2) {
      p2->ty = p1->ty;
      p1 = p1->param_next;
      p2 = p2->param_next;
    }
    if (!p1 != !p2)
      error_tok(tok, "prototype mismatch");
    def_ty->is_oldstyle = false;
  }

  Node *expr = NULL;
  while (is_typename(tok)) {
    Type *basety = declspec(&tok, tok, &(VarAttr){0}, SC_REGISTER);

    do {
      Token *name = NULL;
      Type *ty = declarator(&tok, tok, basety, &name);
      if (!name)
        error_tok(tok, "expected identifier");

      chain_expr(&expr, calc_vla(ty, tok));

      Obj *var = find_param(name, def_ty->param_list);
      if (!var)
        error_tok(name, "no such parameter");

      if (!def_ty->is_oldstyle) {
        ty = ptr_decay(ty);
        if (!is_compatible(var->ty, ty))
          error_tok(name, "incompatible type");
        if (ty->kind == TY_VOID || ty->size <= 0)
          error_tok(name, "invalid parameter type");
        var->ty = ty;
        push_var_name(name, var);
        continue;
      }

      Type *promoted = NULL;
      if (is_integer(ty) && ty->size < ty_int->size)
        promoted = ty_int;
      else if (ty->kind == TY_FLOAT)
        promoted = ty_double;

      if (!promoted) {
        ty = ptr_decay(ty);
        if (ty->kind == TY_VOID || ty->size <= 0)
          error_tok(name, "invalid parameter type");
        var->ty = ty;
        push_var_name(name, var);
      } else {
        var->ty = promoted;
        Node *lhs = new_var_node(new_lvar(ty), name);
        push_var_name(name, lhs->m.var);

        Node *rhs = new_var_node(var, name);
        if (ty->kind == TY_BOOL)
          rhs = new_binary(ND_BITAND, rhs, new_num(1, name), name);
        chain_expr(&expr, new_binary(ND_ASSIGN, lhs, rhs, name));
      }
    } while (comma_list(&tok, &tok, ";", true));
  }

  if (def_ty->is_oldstyle) {
    for (Obj *var = def_ty->param_list; var; var = var->param_next) {
      if (var->ty)
        continue;
      var->ty = ty_int;
      push_var_name2(var->name, strlen(var->name), tok, var);
    }
  }
  *rest = tok;
  return expr;
}

static void func_definition(Token **rest, Token *tok, Obj *fn, Type *ty) {
  if (ty->return_ty->size < 0)
    error_tok(tok, "incomplate return type");
  if (fn->is_definition)
    error_tok(tok, "redefinition of %s", fn->name);
  fn->is_definition = true;

  Type *prot_ty = fn->ty;
  fn->ty = ty;

  arena_on(&ast_arena);

  current_fn = fn;
  current_defr = NULL;
  fn_use_vla = dont_dealloc_vla = false;

  Node *precalc = NULL;
  if (ty->scopes) {
    scope = ty->scopes;

    if (ty->is_oldstyle) {
      precalc = func_old_style_param(&tok, tok, prot_ty, ty);
    } else {
      precalc = ty->pre_calc;

      for (Obj *var = ty->param_list; var; var = var->param_next)
        if (var->ty->size <= 0)
          error_tok(tok, "incomplete parameter type");
    }
  } else {
    enter_scope();
    ty->scopes = scope;
  }

  fn->body = compound_stmt2(rest, tok, ND_BLOCK);

  if (precalc) {
    precalc = new_unary(ND_EXPR_STMT, precalc, tok);
    add_type(precalc);
    precalc->next = fn->body->blk.body;
    fn->body->blk.body = precalc;
  }

  if (fn_use_vla && !dont_dealloc_vla &&
    (opt_reuse_stack && !current_fn->dont_reuse_stk))
    fn->dealloc_vla = true;

  if (gotos)
    resolve_gotos();
  labels = NULL;
  current_fn = NULL;
  current_fnname = NULL;

  emit_text(fn);
  arena_off(&ast_arena);
}

static void global_declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr) {
  bool first = true;
  for (; comma_list(&tok, &tok, ";", !first); first = false) {
    Token *name = NULL;
    Type *ty = declarator2(&tok, tok, basety, &name,
      &(DeclContext){.is_glob = !scope->parent});

    if (ty->kind == TY_FUNC) {
      if (!name)
        error_tok(tok, "function name omitted");

      Obj *fn = func_prototype(ty, attr, name);
      assembler_name(&tok, tok, fn);
      aligned_attr(name, tok, attr, &fn->alt_align);
      symbol_attr(name, tok, attr, fn);

      if (first && !scope->parent && is_func_def(tok)) {
        func_attr(name, tok, attr, fn, true);
        func_definition(rest, tok, fn, ty);
        return;
      }
      func_attr(name, tok, attr, fn, false);
      continue;
    }

    if (!name)
      error_tok(tok, "variable name omitted");

    bool is_definition = !(attr->strg & SC_EXTERN);
    if (!is_definition && equal(tok, "=")) {
      if (scope->parent)
        error_tok(tok, "extern variable cannot be initialized");
      is_definition = true;
    }

    HashEntry *ent = hashmap_get_or_insert(&symbols, name->loc, name->len);
    Obj *var = ent->val;
    if (var) {
      if (!is_compatible2(var->ty, ty))
        error_tok(tok, "incompatible type");
      if ((!var->is_static && !!(attr->strg & SC_STATIC)) ||
        (var->is_static && !(attr->strg & (SC_STATIC | SC_EXTERN))))
        error_tok(name, "inconsistent static");
      if (var->ty->kind == TY_ARRAY && var->ty->size < 0)
        var->ty = ty;
    } else {
      var = ent->val = new_gvar(get_ident(name), ty);
      var->is_static = (attr->strg & SC_STATIC) || (attr->strg & SC_CONSTEXPR);
      var->is_tls = attr->strg & SC_THREAD;
    }
    push_gvar_name(name, var);

    assembler_name(&tok, tok, var);
    aligned_attr(name, tok, attr, &var->alt_align);
    symbol_attr(name, tok, attr, var);

    if (!is_definition || var->init_data)
      continue;
    var->is_definition = true;

    if (attr->strg & SC_CONSTEXPR)
      constexpr_initializer(&tok, skip(tok, "="), var, var);
    else if (equal(tok, "="))
      gvar_initializer(&tok, tok->next, var);
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
  Obj *glb_head = globals;

  Token *free_head = tok;
  while (tok->kind != TK_EOF) {
    if (free_alloc)
      free_head = free_parsed_tok(free_head, tok);

    if (consume(&tok, tok, ";"))
      continue;

    if (tok->kind == TK_asm) {
      static Type ty = {.kind = TY_ASM};
      Obj *obj = new_gvar(NULL, &ty);
      obj->asm_name = str_tok(&tok, skip(tok->next, "("))->str;
      obj->is_definition = true;
      tok = skip(tok, ")");
      continue;
    }

    arena_on(&node_arena);

    if (pragma_pack(&tok, tok)) {
      arena_off(&node_arena);
      continue;
    }

    if (tok->kind == TK_static_assert) {
      arena_on(&ast_arena);
      Obj *last = globals;

      eval_static_assert(&tok, tok->next);
      tok = skip(tok, ";");

      for (Obj *obj = last->next; obj;) {
        Obj *tmp = obj;
        obj = obj->next;
        free(tmp);
      }
      globals = last;
      last->next = NULL;

      arena_off(&ast_arena);
      arena_off(&node_arena);
      continue;
    }

    VarAttr attr = {0};
    Type *basety = declspec(&tok, tok, &attr, SC_ALL);

    // Typedef
    if (attr.strg & SC_TYPEDEF) {
      parse_typedef(&tok, tok, basety, &attr);
      arena_off(&node_arena);
      continue;
    }

    // Global declarations
    global_declaration(&tok, tok, basety, &attr);
    arena_off(&node_arena);
  }

  return glb_head->next;
}

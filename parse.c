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
  int enum_val;
} VarScope;

// Variable attributes such as typedef or extern.
typedef struct {
  bool is_typedef;
  bool is_static;
  bool is_extern;
  bool is_inline;
  bool is_tls;
  bool is_constexpr;
  bool local_only;
  Obj *cleanup_fn;
  int align;
} VarAttr;

// This struct represents a variable initializer. Since initializers
// can be nested (e.g. `int x[2][2] = {{1, 2}, {3, 4}}`), this struct
// is a tree data structure.
typedef struct Initializer Initializer;
struct Initializer {
  Type *ty;
  bool is_flexible;

  // If it's not an aggregate type and has an initializer,
  // `expr` has an initialization expression.
  Node *expr;

  // If it's an initializer for an aggregate type (e.g. array or struct),
  // `children` has initializers for its children.
  Initializer **children;

  // Only one member can be initialized for a union.
  // `mem` is used to clarify which member is initialized.
  Member *mem;
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
  bool is_volatile;
} EvalContext;

// Likewise, global variables are accumulated to this list.
static Obj *globals;

static Scope *scope = &(Scope){0};

// Points to the function object the parser is currently parsing.
static Obj *current_fn;

// Lists of all goto statements and labels in the curent function.
static Node *gotos;
static Node *labels;

// Current "goto" and "continue" jump targets.
static char *brk_label;
static char *cont_label;

// Points to a node representing a switch if we are parsing
// a switch statement. Otherwise, NULL.
static Node *current_switch;

static DeferStmt *current_defr;
static DeferStmt *brk_defr;
static DeferStmt *cont_defr;
static bool fn_use_vla;
static bool dont_dealloc_vla;

static bool *eval_recover;

static bool is_typename(Token *tok);
static Type *declspec(Token **rest, Token *tok, VarAttr *attr);
static Type *typename(Token **rest, Token *tok);
static Type *enum_specifier(Token **rest, Token *tok);
static Type *typeof_specifier(Token **rest, Token *tok);
static Type *type_suffix(Token **rest, Token *tok, Type *ty);
static Type *declarator(Token **rest, Token *tok, Type *ty, Token **name_tok);
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr);
static void array_initializer2(Token **rest, Token *tok, Initializer *init, int i);
static void struct_initializer2(Token **rest, Token *tok, Initializer *init, Member *mem, bool post_desig);
static void initializer2(Token **rest, Token *tok, Initializer *init);
static Initializer *initializer(Token **rest, Token *tok, Type *ty, Type **new_ty);
static Node *lvar_initializer(Token **rest, Token *tok, Obj *var);
static void gvar_initializer(Token **rest, Token *tok, Obj *var);
static void constexpr_initializer(Token **rest, Token *tok, Obj *init_var, Obj *var);
static Node *compound_stmt(Token **rest, Token *tok, NodeKind kind);
static Node *stmt(Token **rest, Token *tok, bool is_labeled);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static int64_t eval(Node *node);
static int64_t eval2(Node *node, EvalContext *ctx);
static Obj *eval_var(Node *node, int *ofs, bool let_volatile);
static Node *assign(Token **rest, Token *tok);
static Node *log_or(Token **rest, Token *tok);
static long double eval_double(Node *node);
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
static Node *postfix(Token **rest, Token *tok);
static Node *funcall(Token **rest, Token *tok, Node *node);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);
static Node *parse_typedef(Token **rest, Token *tok, Type *basety, VarAttr *attr);
static Obj *func_prototype(Type *ty, VarAttr *attr, Token *name);
static Token *global_declaration(Token *tok, Type *basety, VarAttr *attr);
static Node *compute_vla_size(Type *ty, Token *tok);
static Obj *build_fn_param(Node *arg, Token *tok);

static int align_down(int n, int align) {
  return align_to(n - align + 1, align);
}

static void enter_scope(void) {
  Scope *sc = calloc(1, sizeof(Scope));
  sc->parent = scope;
  sc->sibling_next = scope->children;
  scope = scope->children = sc;
}

static void enter_tmp_scope(void) {
  enter_scope();
  scope->is_temporary = true;
}

static void leave_scope(void) {
  scope = scope->parent;
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

static bool is_const_var(Obj *var) {
  Type *ty = var->ty;
  for (; ty && ty->kind == TY_ARRAY; ty = ty->base)
    if (ty->is_const)
      return true;
  return ty->is_const;
}

static Node *new_node(NodeKind kind, Token *tok) {
  Node *node = calloc(1, sizeof(Node));
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

static Node *new_long(int64_t val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  node->ty = ty_long;
  return node;
}

static Node *new_ulong(long val, Token *tok) {
  Node *node = new_node(ND_NUM, tok);
  node->val = val;
  node->ty = ty_ulong;
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

Node *new_cast(Node *expr, Type *ty) {
  add_type(expr);

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
    if (expr->ty == ty && !is_bitfield(expr))
      return expr;

    if (is_redundant_cast(expr, ty)) {
      expr->ty = unqual(ty);
      return expr;
    }
  }
  Node *node = malloc(sizeof(Node));
  *node = tmp_node;
  node->ty = unqual(ty);
  return node;
}

Node *to_bool(Node *expr) {
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
  VarScope *sc = calloc(1, sizeof(VarScope));
  hashmap_put(&scope->vars, name, sc);
  return sc;
}

static Initializer *new_initializer(Type *ty, bool is_flexible) {
  Initializer *init = calloc(1, sizeof(Initializer));
  init->ty = ty;

  if (ty->kind == TY_ARRAY) {
    if (is_flexible && ty->size < 0) {
      init->is_flexible = true;
      return init;
    }

    init->children = calloc(ty->array_len, sizeof(Initializer *));
    for (int i = 0; i < ty->array_len; i++)
      init->children[i] = new_initializer(ty->base, false);
    return init;
  }

  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    // Count the number of struct members.
    int len = 0;
    for (Member *mem = ty->members; mem; mem = mem->next)
      mem->idx = len++;

    init->children = calloc(len, sizeof(Initializer *));

    for (Member *mem = ty->members; mem; mem = mem->next) {
      if (is_flexible && ty->is_flexible && !mem->next) {
        Initializer *child = calloc(1, sizeof(Initializer));
        child->ty = mem->ty;
        child->is_flexible = true;
        init->children[mem->idx] = child;
      } else {
        init->children[mem->idx] = new_initializer(mem->ty, false);
      }
    }
    return init;
  }

  return init;
}

static Obj *new_var(char *name, Type *ty) {
  Obj *var = calloc(1, sizeof(Obj));
  var->name = name;
  var->ty = ty;
  var->align = ty->align;
  if (name)
    push_scope(name)->var = var;
  return var;
}

static Obj *new_lvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->is_local = true;
  var->next = scope->locals;
  scope->locals = var;
  return var;
}

static Obj *new_gvar(char *name, Type *ty) {
  Obj *var = new_var(name, ty);
  var->next = globals;
  globals = var;
  return var;
}

static char *new_unique_name(void) {
  static int id = 0;
  return format(".L..%d", id++);
}

static Obj *new_anon_gvar(Type *ty) {
  Obj *var = new_gvar(NULL, ty);
  var->name = new_unique_name();
  var->is_definition = true;
  var->is_static = true;
  return var;
}

static Obj *new_static_lvar(Type *ty) {
  Obj *var = new_var(NULL, ty);
  var->name = new_unique_name();
  var->is_definition = true;
  var->is_static = true;

  var->next = current_fn->static_lvars;
  current_fn->static_lvars = var;
  return var;
}

static DeferStmt *new_defr(DeferKind kind) {
  DeferStmt *defr = calloc(1, sizeof(DeferStmt));
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

static void attr_aligned(Token *tok, int *align, TokenKind kind) {
  for (Token *lst = tok->attr_next; lst; lst = lst->attr_next) {
    if (lst->kind != kind)
      continue;
    if (equal(lst, "aligned") || equal(lst, "__aligned__")) {
      Token *tok2;
      if (consume(&tok2, lst->next, "(")) {
        int align2 = const_expr(&tok2, tok2);
        *align = MAX(*align, align2);
        continue;
      }
      *align = MAX(*align, 16);
    }
  }
}

static void attr_cleanup(Token *tok, Obj **fn, TokenKind kind) {
  if (*fn)
    return;
  for (Token *lst = tok->attr_next; lst; lst = lst->attr_next) {
    if (equal(lst, "cleanup") || equal(lst, "__cleanup__")) {
      if (lst->kind != kind)
        continue;
      Token *tok2 = skip(lst->next, "(");
      VarScope *sc = find_var(tok2);
      if (!(sc && sc->var && sc->var->ty->kind == TY_FUNC))
        error_tok(tok2, "cleanup function not found");
      strarray_push(&current_fn->refs, sc->var->name);
      *fn = sc->var;
      return;
    }
  }
}

static void attr_packed(Token *tok, Type *ty, TokenKind kind) {
  for (Token *lst = tok->attr_next; lst; lst = lst->attr_next) {
    if (lst->kind != kind)
      continue;
    if (equal(lst, "packed") || equal(lst, "__packed__")) {
      ty->is_packed = true;
      return;
    }
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
  };

  if (attr) {
    attr_aligned(tok, &attr->align, TK_BATTR);
    attr_cleanup(tok, &attr->cleanup_fn, TK_BATTR);
  }

  Type *ty = NULL;
  int counter = 0;
  bool is_atomic = false;
  bool is_const = false;
  bool is_restrict = false;
  bool is_volatile = false;
  bool is_auto = false;
  for (;;) {
    if (attr) {
      attr_aligned(tok, &attr->align, TK_ATTR);
      attr_cleanup(tok, &attr->cleanup_fn, TK_ATTR);
    }
    if (!is_typename(tok))
      break;

    // Handle storage class specifiers.
    if (equal(tok, "typedef") || equal(tok, "static") || equal(tok, "extern") ||
        equal(tok, "inline") || equal(tok, "_Thread_local") || equal(tok, "__thread") ||
        equal_tykw(tok, "thread_local")) {
      if (!attr || attr->local_only)
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

    // These keywords are recognized but ignored.
    if (consume(&tok, tok, "_Noreturn") || consume(&tok, tok, "register"))
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

    if (consume(&tok, tok, "volatile")) {
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
    else
      internal_error();

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
    ty = unqual(array_to_pointer(node->ty));
  }

  if (!ty)
    ty = ty_int;

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
      else if (ty->kind == TY_ARRAY || ty->kind == TY_VLA)
        ty = pointer_to(ty->base);
      else if (ty->kind == TY_FUNC)
        ty = pointer_to(ty);

      Obj *var = new_lvar(get_ident(name), ty);
      if (promoted) {
        var->param_promoted = promoted;
        chain_expr(&expr, new_binary(ND_ASSIGN, new_var_node(var, tok),
                                     new_var_node(promoted, tok), tok));
      }
      chain_expr(&expr, compute_vla_size(ty, tok));
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

    chain_expr(&expr, compute_vla_size(ty2, tok));

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

  if (scope->parent == NULL)
    error_tok(tok, "variably-modified type at file scope");
  return vla_of(ty, expr);
}

static void pointer_qualifiers(Token **rest, Token *tok, Type *ty) {
  for (;; tok = tok->next) {
    if (equal(tok, "_Atomic"))
      ty->is_atomic = true;
    else if (equal(tok, "const"))
      ty->is_const = true;
    else if (equal(tok, "volatile"))
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

static bool is_end(Token *tok) {
  return equal(tok, "}") || (equal(tok, ",") && equal(tok->next, "}"));
}

// enum-specifier = ident? "{" enum-list? "}"
//                | ident ("{" enum-list? "}")?
//
// enum-list      = ident ("=" num)? ("," ident ("=" num)?)* ","?
static Type *enum_specifier(Token **rest, Token *tok) {
  Type *ty = enum_type();

  // Read a struct tag.
  Token *tag = NULL;
  if (tok->kind == TK_IDENT) {
    tag = tok;
    tok = tok->next;
  }

  if (tag && !equal(tok, "{")) {
    Type *ty = find_tag(tag);
    if (!ty)
      error_tok(tag, "unknown enum type");
    if (ty->kind != TY_ENUM)
      error_tok(tag, "not an enum tag");
    *rest = tok;
    return ty;
  }

  tok = skip(tok, "{");

  // Read an enum-list.
  int val = 0;
  bool first = true;
  for (; comma_list(rest, &tok, "}", !first); first = false) {
    char *name = get_ident(tok);
    tok = tok->next;

    if (equal(tok, "="))
      val = const_expr(&tok, tok->next);

    VarScope *sc = push_scope(name);
    sc->enum_ty = ty;
    sc->enum_val = val++;
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

// Generate code for computing a VLA size.
static Node *compute_vla_size(Type *ty, Token *tok) {
  if (ty->vla_size)
    return NULL;

  Node *node = NULL;
  if (ty->base)
    node = compute_vla_size(ty->base, tok);

  if (ty->kind != TY_VLA)
    return node;

  Node *base_sz;
  if (ty->base->kind == TY_VLA)
    base_sz = new_var_node(ty->base->vla_size, tok);
  else
    base_sz = new_num(ty->base->size, tok);

  ty->vla_size = new_lvar(NULL, ty_ulong);
  chain_expr(&node, new_binary(ND_ASSIGN, new_var_node(ty->vla_size, tok),
                               new_binary(ND_MUL, ty->vla_len, base_sz, tok),
                               tok));
  add_type(node);
  return node;
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
  n->args = build_fn_param(new_unary(ND_ADDR, new_var_node(var, tok), tok), tok);

  DeferStmt *defr2 = new_defr(DF_CLEANUP_FN);
  defr2->cleanup_fn = n;
}

// declaration = declspec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **rest, Token *tok, Type *basety, VarAttr *attr) {
  Node *expr = NULL;

  bool first = true;
  for (; comma_list(rest, &tok, ";", !first); first = false) {
    Token *name = NULL;
    int alt_align = attr ? attr->align : 0;

    Type *ty = declarator2(&tok, tok, basety, &name, &alt_align);

    if (ty->kind == TY_FUNC) {
      if (!name)
        error_tok(tok, "function name omitted");
      func_prototype(ty, attr, name);
      continue;
    }
    if (ty->kind == TY_VOID)
      error_tok(tok, "variable declared void");
    if (!name)
      error_tok(tok, "variable name omitted");

    Obj *cleanup_fn = attr ? attr->cleanup_fn : NULL;
    attr_cleanup(name, &cleanup_fn, TK_ATTR);
    attr_cleanup(name->next, &cleanup_fn, TK_BATTR);
    attr_cleanup(tok, &cleanup_fn, TK_ATTR);

    // Generate code for computing a VLA size. We need to do this
    // even if ty is not VLA because ty may be a pointer to VLA
    // (e.g. int (*foo)[n][m] where n and m are variables.)
    chain_expr(&expr, compute_vla_size(ty, tok));

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
        if (!equal(tok, "="))
          error_tok(tok, "constexpr variable not initialized");
        constexpr_initializer(&tok, tok->next, var, var);
        continue;
      }
      if (equal(tok, "="))
        gvar_initializer(&tok, tok->next, var);
      continue;
    }

    if (ty->kind == TY_VLA) {
      if (equal(tok, "="))
        error_tok(tok, "variable-sized object may not be initialized");

      fn_use_vla = true;
      // Variable length arrays (VLAs) are translated to alloca() calls.
      // For example, `int x[n+2]` is translated to `tmp = n + 2,
      // x = alloca(tmp)`.
      Obj *var = new_lvar(get_ident(name), ty);
      if (alt_align)
        var->align = alt_align;
      chain_expr(&expr, new_vla(new_var_node(ty->vla_size, name), var));

      DeferStmt *defr = new_defr(DF_VLA_DEALLOC);
      defr->vla = var;

      if (cleanup_fn)
        defr_cleanup(var, cleanup_fn, tok);
      continue;
    }

    Obj *var = new_lvar(get_ident(name), ty);
    if (alt_align)
      var->align = alt_align;

    if (cleanup_fn)
      defr_cleanup(var, cleanup_fn, tok);

    if (attr && attr->is_constexpr) {
      if (!equal(tok, "="))
        error_tok(tok, "constexpr variable not initialized");
      Obj *init_var = new_static_lvar(ty);
      constexpr_initializer(&tok, tok->next, init_var, var);
      chain_expr(&expr, new_binary(ND_ASSIGN, new_var_node(var, tok),
                                   new_var_node(init_var, tok), tok));
      continue;
    }
    if (equal(tok, "="))
      chain_expr(&expr, lvar_initializer(&tok, tok->next, var));

    if (var->ty->size < 0)
      error_tok(name, "variable has incomplete type");
    if (var->ty->kind == TY_VOID)
      error_tok(name, "variable declared void");
  }
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
  if (init->is_flexible)
    *init = *new_initializer(array_of(init->ty->base, tok->ty->array_len), false);

  int len = MIN(init->ty->array_len, tok->ty->array_len);

  switch (init->ty->base->size) {
  case 1: {
    char *str = tok->str;
    for (int i = 0; i < len; i++)
      init->children[i]->expr = new_num(str[i], tok);
    break;
  }
  case 2: {
    uint16_t *str = (uint16_t *)tok->str;
    for (int i = 0; i < len; i++)
      init->children[i]->expr = new_num(str[i], tok);
    break;
  }
  case 4: {
    uint32_t *str = (uint32_t *)tok->str;
    for (int i = 0; i < len; i++)
      init->children[i]->expr = new_num(str[i], tok);
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

// designation = ("[" const-expr "]" | "." ident)* "="? initializer
static void designation(Token **rest, Token *tok, Initializer *init) {
  if (equal(tok, "[")) {
    if (init->ty->kind != TY_ARRAY)
      error_tok(tok, "array index in non-array initializer");

    int begin, end;
    array_designator(&tok, tok, init->ty, &begin, &end);

    Token *start = tok;
    for (int i = begin; i <= end; i++)
      designation(&tok, start, init->children[i]);

    array_initializer2(rest, tok, init, begin + 1);
    return;
  }

  if (equal(tok, ".") && init->ty->kind == TY_STRUCT) {
    Member *mem = struct_designator(&tok, tok->next, init->ty);
    designation(&tok, tok, init->children[mem->idx]);
    init->expr = NULL;
    struct_initializer2(rest, tok, init, mem->next, true);
    return;
  }

  if (equal(tok, ".") && init->ty->kind == TY_UNION) {
    Member *mem = struct_designator(&tok, tok->next, init->ty);
    init->mem = mem;
    designation(rest, tok, init->children[mem->idx]);
    return;
  }

  if (equal(tok, "."))
    error_tok(tok, "field name not in struct or union initializer");

  if (equal(tok, "="))
    tok = tok->next;
  initializer2(rest, tok, init);
}

// An array length can be omitted if an array has an initializer
// (e.g. `int x[] = {1,2,3}`). If it's omitted, count the number
// of initializer elements.
static int count_array_init_elements(Token *tok, Type *ty) {
  Initializer *dummy = new_initializer(ty->base, true);

  int i = 0, max = 0;

  while (comma_list(&tok, &tok, "}", i)) {
    if (equal(tok, "[")) {
      i = const_expr(&tok, tok->next);
      if (equal(tok, "..."))
        i = const_expr(&tok, tok->next);
      tok = skip(tok, "]");
      designation(&tok, tok, dummy);
    } else {
      initializer2(&tok, tok, dummy);
    }

    i++;
    max = MAX(max, i);
  }
  return max;
}

// array-initializer1 = "{" initializer ("," initializer)* ","? "}"
static void array_initializer1(Token **rest, Token *tok, Initializer *init) {
  tok = skip(tok, "{");

  if (init->is_flexible) {
    int len = count_array_init_elements(tok, init->ty);
    *init = *new_initializer(array_of(init->ty->base, len), false);
  }

  int i = 0;
  bool first = true;
  for (; comma_list(rest, &tok, "}", !first); first = false, i++) {
    if (equal(tok, "[")) {
      int begin, end;
      array_designator(&tok, tok, init->ty, &begin, &end);

      Token *start = tok;
      for (int j = begin; j <= end; j++)
        designation(&tok, start, init->children[j]);

      i = end;
      continue;
    }

    if (i < init->ty->array_len)
      initializer2(&tok, tok, init->children[i]);
    else
      tok = skip_excess_element(tok);
  }
}

// array-initializer2 = initializer ("," initializer)*
static void array_initializer2(Token **rest, Token *tok, Initializer *init, int i) {
  if (init->is_flexible) {
    int len = count_array_init_elements(tok, init->ty);
    *init = *new_initializer(array_of(init->ty->base, len), false);
  }

  for (; i < init->ty->array_len && !is_end(tok); i++) {
    Token *start = tok;
    if (i > 0)
      tok = skip(tok, ",");

    if (equal(tok, "[") || equal(tok, ".")) {
      *rest = start;
      return;
    }

    initializer2(&tok, tok, init->children[i]);
  }
  *rest = tok;
}

// struct-initializer1 = "{" initializer ("," initializer)* ","? "}"
static void struct_initializer1(Token **rest, Token *tok, Initializer *init) {
  tok = skip(tok, "{");

  Member *mem = init->ty->members;

  bool first = true;
  for (; comma_list(rest, &tok, "}", !first); first = false) {
    if (equal(tok, ".")) {
      mem = struct_designator(&tok, tok->next, init->ty);
      designation(&tok, tok, init->children[mem->idx]);
      mem = mem->next;
      continue;
    }

    if (mem) {
      initializer2(&tok, tok, init->children[mem->idx]);
      mem = mem->next;
    } else {
      tok = skip_excess_element(tok);
    }
  }
}

// struct-initializer2 = initializer ("," initializer)*
static void struct_initializer2(Token **rest, Token *tok, Initializer *init, Member *mem, bool post_desig) {
  bool first = true;

  for (; mem && !is_end(tok); mem = mem->next) {
    Token *start = tok;

    if (!first || post_desig)
      tok = skip(tok, ",");
    first = false;

    if (equal(tok, "[") || equal(tok, ".")) {
      *rest = start;
      return;
    }

    initializer2(&tok, tok, init->children[mem->idx]);
  }
  *rest = tok;
}

static void union_initializer(Token **rest, Token *tok, Initializer *init) {
  tok = skip(tok, "{");

  bool first = true;
  for (; comma_list(rest, &tok, "}", !first); first = false) {
    if (equal(tok, ".")) {
      init->mem = struct_designator(&tok, tok->next, init->ty);
      designation(&tok, tok, init->children[init->mem->idx]);
      continue;
    }

    if (first && init->ty->members) {
      init->mem = init->ty->members;
      initializer2(&tok, tok, init->children[0]);
    } else {
      tok = skip_excess_element(tok);
    }
  }
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
    if (equal(tok, "{"))
      array_initializer1(rest, tok, init);
    else
      array_initializer2(rest, tok, init, 0);
    return;
  }

  if (init->ty->kind == TY_STRUCT) {
    if (equal(tok, "{")) {
      struct_initializer1(rest, tok, init);
      return;
    }

    // A struct can be initialized with another struct. E.g.
    // `struct T x = y;` where y is a variable of type `struct T`.
    // Handle that case first.
    Node *expr = assign(rest, tok);
    add_type(expr);
    if (expr->ty->kind == TY_STRUCT) {
      init->expr = expr;
      return;
    }

    if (!init->ty->members)
      error_tok(tok, "initializer for empty aggregate requires explicit braces");

    struct_initializer2(rest, tok, init, init->ty->members, false);
    return;
  }

  if (init->ty->kind == TY_UNION) {
    if (equal(tok, "{")) {
      union_initializer(rest, tok, init);
      return;
    }

    Node *expr = assign(rest, tok);
    add_type(expr);
    if (expr->ty->kind == TY_UNION) {
      init->expr = expr;
      return;
    }
    if (!init->ty->members)
      error_tok(tok, "initializer for empty aggregate requires explicit braces");

    init->mem = init->ty->members;
    initializer2(rest, tok, init->children[0]);
    return;
  }

  if (equal(tok, "{")) {
    // An initializer for a scalar variable can be surrounded by
    // braces. E.g. `int x = {3};`. Handle that case.
    initializer2(&tok, tok->next, init);
    *rest = skip(tok, "}");
    return;
  }

  init->expr = assign(rest, tok);
}

static Initializer *initializer(Token **rest, Token *tok, Type *ty, Type **new_ty) {
  Initializer *init = new_initializer(ty, true);
  initializer2(rest, tok, init);

  if (ty->kind == TY_STRUCT && ty->is_flexible) {
    ty = copy_type(ty);

    Member head = {0};
    Member *cur = &head;
    for (Member *mem = ty->members; mem; mem = mem->next) {
      Member *m = calloc(1, sizeof(Member));
      *m = *mem;
      cur = cur->next = m;
    }

    cur->ty = init->children[cur->idx]->ty;
    ty->size += cur->ty->size;
    ty->members = head.next;

    *new_ty = ty;
    return init;
  }

  *new_ty = init->ty;
  return init;
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

static Node *create_lvar_init(Initializer *init, Type *ty, InitDesg *desg, Token *tok) {
  if (ty->kind == TY_ARRAY) {
    if (init->expr)
      error_tok(init->expr->tok, "array initializer must be an initializer list");
    Node *node = NULL;
    for (int i = 0; i < ty->array_len; i++) {
      InitDesg desg2 = {desg, i};
      chain_expr(&node, create_lvar_init(init->children[i], ty->base, &desg2, tok));
    }
    return node;
  }

  if (init->expr) {
    Node *lhs = init_desg_expr(desg, tok);
    return new_binary(ND_ASSIGN, lhs, init->expr, tok);
  }

  if (ty->kind == TY_STRUCT) {
    Node *node = NULL;
    for (Member *mem = ty->members; mem; mem = mem->next) {
      InitDesg desg2 = {desg, 0, mem};
      chain_expr(&node, create_lvar_init(init->children[mem->idx], mem->ty, &desg2, tok));
    }
    return node;
  }

  if (ty->kind == TY_UNION) {
    if (!init->mem)
      return NULL;
    InitDesg desg2 = {desg, 0, init->mem};
    return create_lvar_init(init->children[init->mem->idx], init->mem->ty, &desg2, tok);
  }

  return NULL;
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
  Initializer *init = initializer(rest, tok, var->ty, &var->ty);
  InitDesg desg = {NULL, 0, NULL, var};

  Node *expr = create_lvar_init(init, var->ty, &desg, tok);
  if (opt_optimize && init->expr)
    return expr;

  // If a partial initializer list is given, the standard requires
  // that unspecified elements are set to 0. Here, we simply
  // zero-initialize the entire memory region of a variable before
  // initializing it with user-supplied values.
  Node *node = new_node(ND_MEMZERO, tok);
  node->var = var;
  chain_expr(&node, expr);
  return node;
}

static uint64_t read_buf(char *buf, int sz) {
  if (sz == 1)
    return *buf;
  if (sz == 2)
    return *(uint16_t *)buf;
  if (sz == 4)
    return *(uint32_t *)buf;
  if (sz == 8)
    return *(uint64_t *)buf;
  internal_error();
}

static void write_buf(char *buf, uint64_t val, int sz) {
  if (sz == 1)
    *buf = val;
  else if (sz == 2)
    *(uint16_t *)buf = val;
  else if (sz == 4)
    *(uint32_t *)buf = val;
  else if (sz == 8)
    *(uint64_t *)buf = val;
  else
    internal_error();
}

static long double read_double_buf(char *buf, Type *ty){
  if (ty->kind == TY_FLOAT)
    return *(float *)buf;
  if (ty->kind == TY_DOUBLE)
    return *(double *)buf;
  if (ty->kind == TY_LDOUBLE)
    return *(long double *)buf;
  internal_error();
}

static Relocation *
write_gvar_data(Relocation *cur, Initializer *init, Type *ty, char *buf, int offset, EvalKind kind) {
  if (ty->kind == TY_ARRAY) {
    if (init->expr)
      error_tok(init->expr->tok, "array initializer must be an initializer list");
    int sz = ty->base->size;
    for (int i = 0; i < ty->array_len; i++)
      cur = write_gvar_data(cur, init->children[i], ty->base, buf, offset + sz * i, kind);
    return cur;
  }

  if (!init->expr) {
    if (ty->kind == TY_STRUCT) {
      for (Member *mem = ty->members; mem; mem = mem->next) {
        if (mem->is_bitfield) {
          Node *expr = init->children[mem->idx]->expr;
          if (!expr)
            continue;
          add_type(expr);

          char *loc = buf + offset + mem->offset;
          uint64_t oldval = read_buf(loc, mem->ty->size);
          uint64_t newval = eval(expr);
          uint64_t mask = (1L << mem->bit_width) - 1;
          uint64_t combined = oldval | ((newval & mask) << mem->bit_offset);
          write_buf(loc, combined, mem->ty->size);
        } else {
          cur = write_gvar_data(cur, init->children[mem->idx], mem->ty, buf, offset + mem->offset, kind);
        }
      }
      return cur;
    }
    if (ty->kind == TY_UNION) {
      if (init->mem)
        cur = write_gvar_data(cur, init->children[init->mem->idx], init->mem->ty, buf, offset, kind);
      return cur;
    }
    return cur;
  }
  add_type(init->expr);

  if (is_compatible(ty, init->expr->ty)) {
    int sofs;
    Obj *var = eval_var(init->expr, &sofs, false);
    if (var && var->init_data && (is_const_var(var) || var->is_compound_lit)) {
      Relocation *srel= var->rel;
      while (srel && srel->offset < sofs)
        srel = srel->next;
      for (int pos = 0; pos < ty->size && (pos + sofs) < var->ty->size;) {
        if (srel && srel->offset == (pos + sofs)) {
          cur = cur->next = calloc(1, sizeof(Relocation));
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

  Node *init_expr = new_cast(init->expr, ty);
  switch (ty->kind) {
  case TY_FLOAT:
    *(float *)(buf + offset) = eval_double(init_expr);
    return cur;
  case TY_DOUBLE:
    *(double *)(buf + offset) = eval_double(init_expr);
    return cur;
  case TY_LDOUBLE:
    *(long double *)(buf + offset) = eval_double(init_expr);
    return cur;
  }

  if (kind == EV_CONST) {
    write_buf(buf + offset, eval(init_expr), ty->size);
    return cur;
  }

  int64_t val;
  if (is_const_expr(init_expr, &val)) {
    write_buf(buf + offset, val, ty->size);
    return cur;
  }

  EvalContext ctx = {.kind = EV_LABEL};
  int64_t addend = eval2(init_expr, &ctx);

  if (ctx.label) {
    Relocation *rel = calloc(1, sizeof(Relocation));
    rel->offset = offset;
    rel->label = ctx.label;
    rel->addend = addend;
    cur->next = rel;
    return cur->next;
  }
  error_tok(init->expr->tok, "invalid initializer");
}

// Initializers for global variables are evaluated at compile-time and
// embedded to .data section. This function serializes Initializer
// objects to a flat byte array. It is a compile error if an
// initializer list contains a non-constant expression.
static void gvar_initializer(Token **rest, Token *tok, Obj *var) {
  Initializer *init = initializer(rest, tok, var->ty, &var->ty);

  Relocation head = {0};
  char *buf = calloc(1, var->ty->size);
  write_gvar_data(&head, init, var->ty, buf, 0, EV_LABEL);
  var->init_data = buf;
  var->rel = head.next;
}

static void constexpr_initializer(Token **rest, Token *tok, Obj *init_var, Obj *var) {
  Initializer *init = initializer(rest, tok, init_var->ty, &init_var->ty);

  Relocation head = {0};
  char *buf = calloc(1, init_var->ty->size);
  write_gvar_data(&head, init, init_var->ty, buf, 0, EV_CONST);
  init_var->init_data = var->constexpr_data = buf;
  init_var->rel = head.next;
  var->ty = init_var->ty;
}

// Returns true if a given token represents a type.
static bool is_typename(Token *tok) {
  return tok->kind == TK_TYPEKW || find_typedef(tok);
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

// asm-stmt = "__asm__" ("volatile" | "inline")* "(" string-literal ")"
static Node *asm_stmt(Token **rest, Token *tok) {
  Node *node = new_node(ND_ASM, tok);
  tok = tok->next;

  while (equal(tok, "volatile") || equal(tok, "inline"))
    tok = tok->next;

  tok = skip(tok, "(");
  if (tok->kind != TK_STR || tok->ty->base->kind != TY_PCHAR)
    error_tok(tok, "expected string literal");
  node->asm_str = tok->str;
  *rest = skip(tok->next, ")");
  return node;
}

static Node *secondary_block(Token **rest, Token *tok) {
  Node *node = new_node(ND_BLOCK, tok);

  node->defr_end = current_defr;
  enter_scope();

  node->body = stmt(rest, tok, true);

  node->defr_start = current_defr;
  current_defr = node->defr_end;
  leave_scope();

  if (node->body->kind == ND_RETURN || node->body->kind == ND_GOTO)
    node->defr_start = node->defr_end;
  return node;
}

static void loop_body(Token **rest, Token *tok, Node *node) {
  char *brk = brk_label;
  char *cont = cont_label;
  brk_label = node->brk_label = new_unique_name();
  cont_label = node->cont_label = new_unique_name();

  DeferStmt *brkdefr = brk_defr;
  DeferStmt *contdefr = cont_defr;
  brk_defr = cont_defr = current_defr;

  node->then = secondary_block(rest, tok);

  brk_label = brk;
  cont_label = cont;
  brk_defr = brkdefr;
  cont_defr = contdefr;
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
static Node *stmt(Token **rest, Token *tok, bool is_labeled) {
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
    Node *node = new_node(ND_IF, tok);
    tok = skip(tok->next, "(");
    node->cond = to_bool(expr(&tok, tok));
    tok = skip(tok, ")");
    node->then = secondary_block(&tok, tok);
    if (equal(tok, "else"))
      node->els = secondary_block(&tok, tok->next);
    *rest = tok;
    return node;
  }

  if (equal(tok, "switch")) {
    Node *node = new_node(ND_SWITCH, tok);
    tok = skip(tok->next, "(");
    node->cond = expr(&tok, tok);
    add_type(node->cond);
    if (!is_integer(node->cond->ty))
      error_tok(tok, "controlling expression not integer");
    tok = skip(tok, ")");

    Node *sw = current_switch;
    current_switch = node;

    char *brk = brk_label;
    brk_label = node->brk_label = new_unique_name();

    DeferStmt *defr = brk_defr;
    brk_defr = current_defr;

    node->then = secondary_block(rest, tok);

    current_switch = sw;
    brk_label = brk;
    brk_defr = defr;
    return node;
  }

  if (equal(tok, "case")) {
    if (!current_switch)
      error_tok(tok, "stray case");
    if (current_defr != brk_defr)
      error_tok(tok, "jump crosses VLA initialization");

    Node *node = new_node(ND_CASE, tok);
    node->label = new_unique_name();

    int64_t begin = const_expr(&tok, tok->next);
    int64_t end;

    // [GNU] Case ranges, e.g. "case 1 ... 5:"
    if (equal(tok, "..."))
      end = const_expr(&tok, tok->next);
    else
      end = begin;

    Type *cond_ty = current_switch->cond->ty;
    if (cond_ty->size <= 4) {
      if (!cond_ty->is_unsigned)
        begin = (int32_t)begin, end = (int32_t)end;
      else
        begin = (uint32_t)begin, end = (uint32_t)end;
    }
    if ((!cond_ty->is_unsigned && (end < begin)) ||
      ((cond_ty->is_unsigned && ((uint64_t)end < begin))))
      error_tok(tok, "empty case range specified");

    tok = skip(tok, ":");
    if (is_labeled)
      node->lhs = stmt(rest, tok, true);
    else
      *rest = tok;
    node->begin = begin;
    node->end = end;
    node->case_next = current_switch->case_next;
    current_switch->case_next = node;
    return node;
  }

  if (equal(tok, "default")) {
    if (!current_switch)
      error_tok(tok, "stray default");
    if (current_defr != brk_defr)
      error_tok(tok, "jump crosses VLA initialization");

    Node *node = new_node(ND_CASE, tok);
    node->label = new_unique_name();

    tok = skip(tok->next, ":");
    if (is_labeled)
      node->lhs = stmt(rest, tok, true);
    else
      *rest = tok;
    current_switch->default_case = node;
    return node;
  }

  if (equal(tok, "for")) {
    Node *node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");

    node->defr_end = current_defr;
    enter_tmp_scope();

    if (is_typename(tok)) {
      VarAttr attr = {.local_only = true};
      Type *basety = declspec(&tok, tok, &attr);
      Node *expr = declaration(&tok, tok, basety, &attr);
      if (expr)
        node->init = new_unary(ND_EXPR_STMT, expr, tok);
    } else if (equal(tok, "_Static_assert") || equal_kw(tok, "static_assert")) {
      static_assertion(&tok, tok->next);
    } else {
      node->init = expr_stmt(&tok, tok);
    }

    if (!equal(tok, ";"))
      node->cond = to_bool(expr(&tok, tok));
    tok = skip(tok, ";");

    if (!equal(tok, ")"))
      node->inc = expr(&tok, tok);
    tok = skip(tok, ")");

    loop_body(rest, tok, node);

    node->defr_start = current_defr;
    current_defr = node->defr_end;
    leave_scope();
    return node;
  }

  if (equal(tok, "while")) {
    Node *node = new_node(ND_FOR, tok);
    tok = skip(tok->next, "(");
    node->cond = to_bool(expr(&tok, tok));
    tok = skip(tok, ")");

    loop_body(rest, tok, node);
    return node;
  }

  if (equal(tok, "do")) {
    Node *node = new_node(ND_DO, tok);

    loop_body(&tok, tok->next, node);

    tok = skip(tok, "while");
    tok = skip(tok, "(");
    node->cond = to_bool(expr(&tok, tok));
    tok = skip(tok, ")");
    *rest = skip(tok, ";");
    return node;
  }

  if (equal_kw(tok, "asm") || equal(tok, "__asm") || equal(tok, "__asm__"))
    return asm_stmt(rest, tok);

  if (equal(tok, "goto")) {
    if (equal(tok->next, "*")) {
      // [GNU] `goto *ptr` jumps to the address specified by `ptr`.
      Node *node = new_node(ND_GOTO_EXPR, tok);
      node->lhs = expr(&tok, tok->next->next);
      *rest = skip(tok, ";");
      return node;
    }

    Node *node = new_node(ND_GOTO, tok);
    node->label = get_ident(tok->next);
    node->goto_next = gotos;
    node->defr_start = current_defr;
    gotos = node;
    *rest = skip(tok->next->next, ";");
    return node;
  }

  if (equal(tok, "break")) {
    if (!brk_label)
      error_tok(tok, "stray break");
    Node *node = new_node(ND_GOTO, tok);
    node->unique_label = brk_label;
    node->defr_end = brk_defr;
    node->defr_start = current_defr;
    *rest = skip(tok->next, ";");
    return node;
  }

  if (equal(tok, "continue")) {
    if (!cont_label)
      error_tok(tok, "stray continue");
    Node *node = new_node(ND_GOTO, tok);
    node->unique_label = cont_label;
    node->defr_end = cont_defr;
    node->defr_start = current_defr;
    *rest = skip(tok->next, ";");
    return node;
  }

  if (tok->kind == TK_IDENT && equal(tok->next, ":")) {
    Node *node = new_node(ND_LABEL, tok);
    node->label = strndup(tok->loc, tok->len);

    tok = tok->next->next;
    if (is_labeled)
      node->lhs = stmt(rest, tok, true);
    else
      *rest = tok;
    node->unique_label = new_unique_name();
    node->goto_next = labels;
    node->defr_start = current_defr;
    labels = node;
    return node;
  }

  if (equal(tok, "{"))
    return compound_stmt(rest, tok->next, ND_BLOCK);

  return expr_stmt(rest, tok);
}

// compound-stmt = (typedef | declaration | stmt)* "}"
static Node *compound_stmt(Token **rest, Token *tok, NodeKind kind) {
  Node *node = new_node(kind, tok);
  Node head = {0};
  Node *cur = &head;

  node->defr_end = current_defr;
  enter_scope();

  while (!equal(tok, "}")) {
    if (equal(tok, "_Static_assert") || equal_kw(tok, "static_assert")) {
      static_assertion(&tok, tok->next);
      continue;
    }

    if (is_typename(tok) && !equal(tok->next, ":")) {
      VarAttr attr = {0};
      Type *basety = declspec(&tok, tok, &attr);

      if (attr.is_typedef) {
        Node *expr = parse_typedef(&tok, tok, basety, &attr);
        if (expr) {
          cur = cur->next = new_unary(ND_EXPR_STMT, expr, tok);
          add_type(cur);
        }
        continue;
      }

      if (attr.is_extern) {
        tok = global_declaration(tok, basety, &attr);
        continue;
      }

      Node *expr = declaration(&tok, tok, basety, &attr);
      if (expr) {
        cur = cur->next = new_unary(ND_EXPR_STMT, expr, tok);
        add_type(cur);
      }
      continue;
    }
    cur = cur->next = stmt(&tok, tok, false);
    add_type(cur);
  }

  node->defr_start = current_defr;
  current_defr = node->defr_end;
  leave_scope();

  if (kind == ND_BLOCK)
    if (cur->kind == ND_RETURN || cur->kind == ND_GOTO)
      node->defr_start = node->defr_end;

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
  verror_at(tok->file->name, tok->file->contents, tok->line_no, tok->loc, fmt, ap);
  va_end(ap);
  exit(1);
}

static Obj *eval_var(Node *node, int *ofs, bool let_volatile) {
  if (node->kind == ND_VAR) {
    *ofs = 0;
    if (let_volatile || !node->ty->is_volatile)
      return node->var;
  }
  if (node->kind == ND_MEMBER || node->kind == ND_DEREF) {
    EvalContext ctx = {.kind = EV_AGGREGATE};
    *ofs = eval2(node, &ctx);
    if (ctx.var && (let_volatile || !ctx.is_volatile))
      return ctx.var;
  }
  return NULL;
}

static bool is_static_const_var(Obj *var, int ofs, int read_sz) {
  if (!opt_optimize || !var->init_data || !is_const_var(var))
    return false;
  for (Relocation *rel = var->rel; rel; rel = rel->next) {
    if ((rel->offset + sizeof(void *)) <= ofs)
      continue;
    if ((ofs + read_sz) <= rel->offset)
      break;
    return false;
  }
  return true;
}

static char *eval_constexpr_data(Node *node) {
  int ofs;
  Obj *var = eval_var(node, &ofs, false);

  if (!var || !(var->constexpr_data || is_static_const_var(var, ofs, node->ty->size)))
    return (char *)eval_error(node->tok, "not a compile-time constant");

  if (ofs < 0 || (var->ty->size < (ofs + node->ty->size)))
    return (char *)eval_error(node->tok, "constexpr access out of bounds");

  if (var->constexpr_data)
    return var->constexpr_data + ofs;

  return var->init_data + ofs;
}

static int64_t eval_sign_extend(Type *ty, uint64_t val) {
  switch (ty->size) {
  case 1: return ty->is_unsigned ? (uint8_t)val : (int64_t)(int8_t)val;
  case 2: return ty->is_unsigned ? (uint16_t)val : (int64_t)(int16_t)val;
  case 4: return ty->is_unsigned ? (uint32_t)val : (int64_t)(int32_t)val;
  case 8: return val;
  }
  internal_error();
}

static int64_t read_buf_sign_extend(Type *ty, char *data) {
  return eval_sign_extend(ty, read_buf(data, ty->size));
}

static void eval_void(Node *node) {
  if (node->kind == ND_VAR)
    return;

  if (is_flonum(node->ty)) {
    eval_double(node);
    return;
  }
  eval(node);
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
    return eval2(lhs, ctx) + eval2(rhs, ctx);
  case ND_SUB:
    return eval2(lhs, ctx) - eval(rhs);
  case ND_MUL:
    return eval(lhs) * eval(rhs);
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
    if (ty->size == 4) {
      if (ty->is_unsigned)
        return (uint32_t)-eval(lhs);
      return (int32_t)-eval(lhs);
    }
    return -eval(lhs);
  case ND_BITAND:
    return eval(lhs) & eval(rhs);
  case ND_BITOR:
    return eval(lhs) | eval(rhs);
  case ND_BITXOR:
    return eval(lhs) ^ eval(rhs);
  case ND_SHL:
    if (ty->size == 4) {
      if (ty->is_unsigned)
        return (uint32_t)eval(lhs) << eval(rhs);
      return (int32_t)eval(lhs) << eval(rhs);
    }
    return eval(lhs) << eval(rhs);
  case ND_SHR:
    if (ty->size == 4)
      return (uint32_t)eval(lhs) >> eval(rhs);
    return (uint64_t)eval(lhs) >> eval(rhs);
  case ND_SAR:
    if (ty->size == 4)
      return (int32_t)eval(lhs) >> eval(rhs);
    return eval(lhs) >> eval(rhs);
  case ND_EQ:
    if (is_flonum(lhs->ty))
      return eval_double(lhs) == eval_double(rhs);
    return eval(lhs) == eval(rhs);
  case ND_NE:
    if (is_flonum(lhs->ty))
      return eval_double(lhs) != eval_double(rhs);
    return eval(lhs) != eval(rhs);
  case ND_LT:
    if (is_flonum(lhs->ty))
      return eval_double(lhs) < eval_double(rhs);
    if (lhs->ty->is_unsigned)
      return (uint64_t)eval(lhs) < eval(rhs);
    return eval(lhs) < eval(rhs);
  case ND_LE:
    if (is_flonum(lhs->ty))
      return eval_double(lhs) <= eval_double(rhs);
    if (lhs->ty->is_unsigned)
      return (uint64_t)eval(lhs) <= eval(rhs);
    return eval(lhs) <= eval(rhs);
  case ND_GT:
    if (is_flonum(lhs->ty))
      return eval_double(lhs) > eval_double(rhs);
    if (lhs->ty->is_unsigned)
      return (uint64_t)eval(lhs) > eval(rhs);
    return eval(lhs) > eval(rhs);
  case ND_GE:
    if (is_flonum(lhs->ty))
      return eval_double(lhs) >= eval_double(rhs);
    if (lhs->ty->is_unsigned)
      return (uint64_t)eval(lhs) >= eval(rhs);
    return eval(lhs) >= eval(rhs);
  case ND_COND:
    return eval(node->cond) ? eval2(node->then, ctx) : eval2(node->els, ctx);
  case ND_COMMA:
    eval_void(lhs);
    return eval2(rhs, ctx);
  case ND_NOT:
    return !eval(lhs);
  case ND_BITNOT:
    if (ty->size == 4) {
      if (ty->is_unsigned)
        return (uint32_t)~eval(lhs);
      return (int32_t)~eval(lhs);
    }
    return ~eval(lhs);
  case ND_LOGAND:
    return eval(lhs) && eval(rhs);
  case ND_LOGOR:
    return eval(lhs) || eval(rhs);
  case ND_CAST: {
    if (is_flonum(lhs->ty)) {
      if (ty->kind == TY_BOOL)
        return !!eval_double(lhs);
      if (ty->size == 8 && ty->is_unsigned)
        return (uint64_t)eval_double(lhs);
      return eval_sign_extend(ty, eval_double(lhs));
    }
    if (ty->kind == TY_BOOL && lhs->kind == ND_VAR && (is_array(lhs->ty)))
      return true;

    if (ty->size != sizeof(void *) && !lhs->has_no_ptr) {
      bool failed = false;
      bool *prev = eval_recover;
      eval_recover = &failed;

      EvalContext ctx2= {.kind = EV_LABEL};
      eval2(lhs, &ctx2);

      eval_recover = prev;
      if (!failed && ctx2.label) {
        if (ty->kind == TY_BOOL)
          return true;
        return eval_error(node->tok, "pointer cast to different size");
      }
      lhs->has_no_ptr = true;
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

  if (node->kind == ND_DEREF && lhs->kind == ND_ADDR)
    return eval2(lhs->lhs, ctx);
  if (node->kind == ND_ADDR && lhs->kind == ND_DEREF)
    return eval2(lhs->lhs, ctx);

  if (ctx->kind == EV_AGGREGATE) {
    ctx->is_volatile |= node->ty->is_volatile;

    switch (node->kind) {
    case ND_DEREF:
      return eval2(lhs, ctx);
    case ND_MEMBER:
      return eval2(lhs, ctx) + node->member->offset;
    case ND_VAR:
      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
      case TY_ARRAY:
        ctx->var = node->var;
        return 0;
      }
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
        ctx->label = &var->name;
        return ofs;
      }
    }
    return eval_error(node->tok, "invalid initializer");
  }

  if (ctx->kind == EV_CONST && is_integer(ty)) {
    char *data = eval_constexpr_data(node);
    if (data) {
      int64_t val = read_buf_sign_extend(ty, data);
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
  return eval_error(node->tok, "not a compile-time constant");
}

bool is_const_expr(Node *node, int64_t *val) {
  add_type(node);
  bool failed = false;

  assert(!eval_recover);
  eval_recover = &failed;
  int64_t v = eval(node);
  if (val)
    *val = v;
  eval_recover = NULL;
  return !failed;
}

bool is_const_double(Node *node, long double *fval) {
  add_type(node);
  bool failed = false;

  assert(!eval_recover);
  eval_recover = &failed;
  long double v = eval_double(node);
  if (fval)
    *fval = v;
  eval_recover = NULL;
  return !failed;
}

int64_t const_expr(Token **rest, Token *tok) {
  Node *node = conditional(rest, tok);
  add_type(node);
  if (!is_integer(node->ty))
    error_tok(tok, "constant expression not integer");
  return eval(node);
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
    return eval(lhs);
  case ND_NUM:
    return node->fval;
  }

  char *data = eval_constexpr_data(node);
  if (data)
    return read_double_buf(data, ty);

  return eval_error(node->tok, "not a compile-time constant");
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

// Convert op= operators to expressions containing an assignment.
//
// In general, `A op= C` is converted to ``tmp = &A, *tmp = *tmp op B`.
// However, if a given expression is of form `A.x op= C`, the input is
// converted to `tmp = &A, (*tmp).x = (*tmp).x op C` to handle assignments
// to bitfields.
static Node *to_assign(Node *binary) {
  add_type(binary->lhs);
  add_type(binary->rhs);
  Token *tok = binary->tok;

  // If A is an atomic type, Convert `A op= B` to atomic_op_fetch(&A, B)
  if (binary->lhs->ty->is_atomic)
    return atomic_op(binary, false);

  // Convert `A.x op= C` to `tmp = &A, (*tmp).x = (*tmp).x op C`.
  if (is_bitfield(binary->lhs)) {
    Obj *var = new_lvar(NULL, pointer_to(binary->lhs->lhs->ty));

    Node *expr1 = new_binary(ND_ASSIGN, new_var_node(var, tok),
                             new_unary(ND_ADDR, binary->lhs->lhs, tok), tok);

    Node *expr2 = new_unary(ND_MEMBER,
                            new_unary(ND_DEREF, new_var_node(var, tok), tok),
                            tok);
    expr2->member = binary->lhs->member;

    Node *expr3 = new_unary(ND_MEMBER,
                            new_unary(ND_DEREF, new_var_node(var, tok), tok),
                            tok);
    expr3->member = binary->lhs->member;

    Node *expr4 = new_binary(ND_ASSIGN, expr2,
                             new_binary(binary->kind, expr3, binary->rhs, tok),
                             tok);

    return new_binary(ND_CHAIN, expr1, expr4, tok);
  }

  // Convert `A op= B` to ``tmp = &A, *tmp = *tmp op B`.
  Obj *var = new_lvar(NULL, pointer_to(binary->lhs->ty));

  Node *expr1 = new_binary(ND_ASSIGN, new_var_node(var, tok),
                           new_unary(ND_ADDR, binary->lhs, tok), tok);

  Node *expr2 =
    new_binary(ND_ASSIGN,
               new_unary(ND_DEREF, new_var_node(var, tok), tok),
               new_binary(binary->kind,
                          new_unary(ND_DEREF, new_var_node(var, tok), tok),
                          binary->rhs,
                          tok),
               tok);

  return new_binary(ND_CHAIN, expr1, expr2, tok);
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
    enter_tmp_scope();
    Obj *var = new_lvar(NULL, cond->ty);
    Node *lhs = new_binary(ND_ASSIGN, new_var_node(var, tok), cond, tok);
    Node *rhs = new_node(ND_COND, tok);
    rhs->cond = to_bool(new_var_node(var, tok));
    rhs->then = new_var_node(var, tok);
    rhs->els = conditional(rest, tok->next->next);
    leave_scope();
    return new_binary(ND_CHAIN, lhs, rhs, tok);
  }

  Node *node = new_node(ND_COND, tok);
  node->cond = to_bool(cond);
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
    node = new_binary(ND_LOGOR, to_bool(node), to_bool(log_and(&tok, tok->next)), start);
  }
  *rest = tok;
  return node;
}

// logand = bitor ("&&" bitor)*
static Node *log_and(Token **rest, Token *tok) {
  Node *node = bit_or(&tok, tok);
  while (equal(tok, "&&")) {
    Token *start = tok;
    node = new_binary(ND_LOGAND, to_bool(node), to_bool(bit_or(&tok, tok->next)), start);
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

  if (lhs->ty->base && rhs->ty->base)
    error_tok(tok, "invalid operands");

  // Canonicalize `num + ptr` to `ptr + num`.
  if (!lhs->ty->base && rhs->ty->base) {
    Node *tmp = lhs;
    lhs = rhs;
    rhs = tmp;
  }

  // VLA + num
  if (lhs->ty->base->kind == TY_VLA) {
    rhs = new_binary(ND_MUL, rhs, new_var_node(lhs->ty->base->vla_size, tok), tok);
    return new_binary(ND_ADD, lhs, rhs, tok);
  }

  // ptr + num
  rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
  return new_binary(ND_ADD, lhs, rhs, tok);
}

// Like `+`, `-` is overloaded for the pointer type.
static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
  add_type(lhs);
  add_type(rhs);

  // num - num
  if (is_numeric(lhs->ty) && is_numeric(rhs->ty))
    return new_binary(ND_SUB, lhs, rhs, tok);

  // VLA - num
  if (lhs->ty->base->kind == TY_VLA) {
    rhs = new_binary(ND_MUL, rhs, new_var_node(lhs->ty->base->vla_size, tok), tok);
    return new_binary(ND_SUB, lhs, rhs, tok);
  }

  // ptr - num
  if (lhs->ty->base && is_integer(rhs->ty)) {
    rhs = new_binary(ND_MUL, rhs, new_long(lhs->ty->base->size, tok), tok);
    return new_binary(ND_SUB, lhs, rhs, tok);
  }

  // ptr - ptr, which returns how many elements are between the two.
  if (lhs->ty->base && rhs->ty->base) {
    int sz = lhs->ty->base->size;
    Node *node = new_binary(ND_SUB, new_cast(lhs, ty_llong), new_cast(rhs, ty_llong), tok);
    return new_binary(ND_DIV, node, new_num(sz, tok), tok);
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
  if (equal(tok, "(") && is_typename(tok->next)) {
    Token *start = tok;
    Type *ty = typename(&tok, tok->next);
    tok = skip(tok, ")");

    // compound literal
    if (equal(tok, "{"))
      return unary(rest, start);

    // type cast
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
    return new_unary(ND_NOT, to_bool(cast(rest, tok->next)), tok);

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
    node->label = get_ident(tok->next);
    node->goto_next = gotos;
    gotos = node;
    dont_dealloc_vla = true;
    *rest = tok->next->next;
    return node;
  }

  return postfix(rest, tok);
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
      mem->alt_align = attr.align;
      mem->ty = declarator2(&tok, tok, basety, &mem->name, &mem->alt_align);

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

  attr_packed(tok, ty, TK_ATTR);
  attr_packed(tok, ty, TK_BATTR);

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
  attr_packed(tok, ty, TK_ATTR);
  *rest = tok;

  if (kind == TY_STRUCT)
    ty = struct_decl(ty, alt_align);
  else
    ty = union_decl(ty, alt_align);

  if (!tag)
    return ty;

  Type *ty2 = hashmap_get2(&scope->tags, tag->loc, tag->len);
  if (ty2) {
    for (Type *t = ty2; t; t = t->decl_next) {
      t->size = ty->size;
      t->align = MAX(t->align, ty->align);
      t->members = ty->members;
      t->is_flexible = ty->is_flexible;
      t->is_packed = ty->is_packed;
      t->origin = ty;
    }
    return ty2;
  }

  push_tag_scope(tag, ty);
  return ty;
}

static Type *struct_decl(Type *ty, int alt_align) {
  int bits = 0;
  Member head = {0};
  Member *cur = &head;

  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (!mem->is_bitfield || mem->name) {
      cur = cur->next = mem;
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
  cur->next = NULL;
  ty->members = head.next;

  if (alt_align)
    ty->align = alt_align;
  if (!alt_align && ty->is_packed)
    ty->size = align_to(bits, 8) / 8;
  else
    ty->size = align_to(bits, ty->align * 8) / 8;
  return ty;
}

static Type *union_decl(Type *ty, int alt_align) {
  Member head = {0};
  Member *cur = &head;
  for (Member *mem = ty->members; mem; mem = mem->next) {
    if (!mem->is_bitfield || mem->name) {
      cur = cur->next = mem;
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
  cur->next = NULL;
  ty->members = head.next;

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

// Convert A++ to `(ptr = &A, tmp = *ptr, *ptr += 1, tmp)`
static Node *new_inc_dec(Node *node, Token *tok, int addend) {
  add_type(node);

  if (opt_optimize && node->ty->kind != TY_BOOL && !is_bitfield(node) && !is_flonum(node->ty)) {
    Type *ty = node->ty;
    node = new_add(node, new_num(addend, tok), tok);
    node = to_assign(node);
    node = new_add(node, new_num(-addend, tok), tok);
    return new_cast(node, ty);
  }

  enter_tmp_scope();

  Node *ref;
  Node *expr = NULL;

  if (is_bitfield(node)) {
    Obj *ptr = new_lvar(NULL, pointer_to(node->lhs->ty));

    ref = new_unary(ND_MEMBER, new_unary(ND_DEREF, new_var_node(ptr, tok), tok), tok);
    ref->member = node->member;

    chain_expr(&expr, new_binary(ND_ASSIGN, new_var_node(ptr, tok),
                                 new_unary(ND_ADDR, node->lhs, tok), tok));
  } else {
    Obj *ptr = new_lvar(NULL, pointer_to(node->ty));

    ref = new_unary(ND_DEREF, new_var_node(ptr, tok), tok);

    chain_expr(&expr, new_binary(ND_ASSIGN, new_var_node(ptr, tok),
                                 new_unary(ND_ADDR, node, tok), tok));
  }
  Obj *tmp = new_lvar(NULL, node->ty);

  chain_expr(&expr, new_binary(ND_ASSIGN, new_var_node(tmp, tok), ref, tok));
  chain_expr(&expr, to_assign(new_add(ref, new_num(addend, tok), tok)));
  chain_expr(&expr, new_var_node(tmp, tok));
  leave_scope();
  return expr;
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
static Node *postfix(Token **rest, Token *tok) {
  Node *node = primary(&tok, tok);

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

static Obj *build_fn_param(Node *arg, Token *tok) {
  add_type(arg);

  Obj *var;
  if (is_trivial_arg(arg)) {
    var = new_var(NULL, arg->ty);
    var->param_arg = arg;
  } else {
    var = new_lvar(NULL, arg->ty);
    var->param_arg = new_binary(ND_ASSIGN, new_var_node(var, tok), arg, tok);
    add_type(var->param_arg);
  }
  return var;
}

// funcall = (assign ("," assign)*)? ")"
static Node *funcall(Token **rest, Token *tok, Node *fn) {
  add_type(fn);

  if (fn->ty->kind != TY_FUNC &&
      (fn->ty->kind != TY_PTR || fn->ty->base->kind != TY_FUNC))
    error_tok(fn->tok, "not a function");

  Type *ty = (fn->ty->kind == TY_FUNC) ? fn->ty : fn->ty->base;
  Obj *param = ty->is_oldstyle ? NULL : ty->param_list;

  Obj head = {0};
  Obj *cur = &head;

  enter_tmp_scope();

  while (comma_list(rest, &tok, ")", cur != &head)) {
    Node *arg = assign(&tok, tok);
    add_type(arg);

    if (param) {
      if (param->ty->kind != TY_STRUCT && param->ty->kind != TY_UNION &&
        !(arg->ty->kind == TY_PTR && param->ty->kind == TY_PTR) &&
        !(arg->ty->kind == param->ty->kind && arg->ty->is_unsigned == param->ty->is_unsigned))
        arg = new_cast(arg, param->ty);

      param = param->param_next;
    } else {
      if (!ty->is_variadic && !ty->is_oldstyle)
        error_tok(tok, "too many arguments");

      if (arg->ty->kind == TY_FLOAT)
        arg = new_cast(arg, ty_double);
      else if (is_array(arg->ty))
        arg = new_cast(arg, pointer_to(arg->ty->base));
      else if (arg->ty->kind == TY_FUNC)
        arg = new_cast(arg, pointer_to(arg->ty));
    }

    cur = cur->param_next = build_fn_param(arg, tok);
  }
  if (param)
    error_tok(tok, "too few arguments");

  leave_scope();

  Node *node = new_unary(ND_FUNCALL, fn, tok);
  node->ty = ty->return_ty;
  node->args = head.param_next;

  // If a function returns a struct, it is caller's responsibility
  // to allocate a space for the return value.
  if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION)
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

  Node *ctrl = assign(&tok, tok);
  add_type(ctrl);

  Type *t1 = ctrl->ty;
  if (t1->kind == TY_FUNC)
    t1 = pointer_to(t1);
  else if (is_array(t1))
    t1 = pointer_to(t1->base);
  else
    t1 = unqual(t1);

  Node *ret = NULL;

  while (comma_list(rest, &tok, ")", true)) {
    if (equal(tok, "default")) {
      tok = skip(tok->next, ":");
      Node *node = assign(&tok, tok);
      if (!ret)
        ret = node;
      continue;
    }

    Type *t2 = typename(&tok, tok);
    if (t2->kind == TY_FUNC)
      error_tok(tok, "association has function type");
    tok = skip(tok, ":");
    Node *node = assign(&tok, tok);
    if (is_compatible2(t1, t2))
      ret = node;
  }

  if (!ret)
    error_tok(start, "controlling expression type not compatible with"
              " any generic association type");
  return ret;
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

  if (equal(tok, "(") && is_typename(tok->next)) {
    // Compound literal
    Token *start = tok;
    Type *ty = typename(&tok, tok->next);
    if (ty->kind == TY_VLA)
      error_tok(tok, "compound literals cannot be VLA");
    tok = skip(tok, ")");

    if (scope->parent == NULL) {
      Obj *var = new_anon_gvar(ty);
      var->is_compound_lit = true;
      gvar_initializer(rest, tok, var);
      return new_var_node(var, start);
    }
    Scope *sc = scope;
    while (sc->is_temporary)
      sc = sc->parent;

    Obj *var = new_var(NULL, ty);
    var->is_compound_lit = true;
    var->is_local = true;
    var->next = sc->locals;
    sc->locals = var;

    Node *lhs = lvar_initializer(rest, tok, var);
    Node *rhs = new_var_node(var, tok);
    return new_binary(ND_CHAIN, lhs, rhs, start);
  }

  if (equal(tok, "(") && equal(tok->next, "{")) {
    if (scope->parent == NULL)
      error_tok(tok, "statement expresssion at file scope");

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
    if (equal(tok->next, "(") && is_typename(tok->next->next)) {
      ty = typename(&tok, tok->next->next);
      *rest = skip(tok, ")");
    } else {
      Node *node = unary(rest, tok->next);
      add_type(node);
      ty = node->ty;
    }
    if (ty->kind == TY_VLA) {
      if (ty->vla_size)
        return new_var_node(ty->vla_size, tok);
      return compute_vla_size(ty, tok);
    }
    if (ty->size < 0)
      error_tok(tok, "sizeof applied to incomplete type");

    if (ty->kind == TY_STRUCT && ty->is_flexible) {
      Member *mem = ty->members;
      while (mem->next)
        mem = mem->next;
      if (mem->ty->kind == TY_ARRAY)
        return new_ulong((ty->size - mem->ty->size), start);
    }
    return new_ulong(ty->size, start);
  }

  if (equal(tok, "_Alignof") || equal_kw(tok, "alignof")) {
    Token *start = tok;
    Type *ty;
    if (equal(tok->next, "(") && is_typename(tok->next->next)) {
      ty = typename(&tok, tok->next->next);
      *rest = skip(tok, ")");
    } else {
      Node *node = unary(rest, tok->next);
      switch (node->kind) {
      case ND_MEMBER:
        return new_ulong(MAX(node->member->ty->align, node->member->alt_align), start);
      case ND_VAR:
        return new_ulong(node->var->align, start);
      }
      add_type(node);
      ty = node->ty;
    }
    while (is_array(ty))
      ty = ty->base;
    return new_ulong(ty->align, start);
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
    tok = skip(tok, ",");

    Node *node = NULL;
    int offset = 0;
    do {
      Member *mem;
      do {
        mem = struct_designator(&tok, tok, ty);
        offset += mem->offset;
        ty = mem->ty;
      } while (!mem->name);

      for (; ty->base && consume(&tok, tok, "["); tok = skip(tok, "]")) {
        ty = ty->base;
        Node *expr = conditional(&tok, tok);
        int64_t val;
        if (is_const_expr(expr, &val)) {
          offset += ty->size * val;
          continue;
        }
        if (!node)
          node = new_binary(ND_MUL, expr, new_long(ty->size, tok), tok);
        else
          node = new_binary(ND_ADD, node, new_binary(ND_MUL, expr, new_long(ty->size, tok), tok), tok);
      }
    } while (consume(&tok, tok, "."));

    *rest = skip(tok, ")");
    if (!node)
      return new_ulong(offset, tok);
    return new_binary(ND_ADD, node, new_ulong(offset, tok), tok);
  }

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

      if (strstr(name, "setjmp") || strstr(name, "savectx") ||
          strstr(name, "vfork") || strstr(name, "getcontext"))
        dont_reuse_stack = true;
    }

    if (sc) {
      if (sc->var)
        return new_var_node(sc->var, tok);
      if (sc->enum_ty)
        return new_num(sc->enum_val, tok);
    }

    // [https://www.sigbus.info/n1570#6.4.2.2p1] "__func__" is
    // automatically defined as a local variable containing the
    // current function name.
    // [GNU] __FUNCTION__ is yet another name of __func__.
    if (current_fn && (equal(tok, "__func__") || equal(tok, "__FUNCTION__"))) {
      char *name = current_fn->name;
      VarScope *vsc = calloc(1, sizeof(VarScope));
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
    Obj *var;
    if (!current_fn)
      var = new_anon_gvar(tok->ty);
    else
      var = new_static_lvar(tok->ty);

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

  if (tok->kind == TK_PP_NUM)
    convert_pp_number(tok);

  if (tok->kind == TK_NUM) {
    Node *node;
    if (is_flonum(tok->ty)) {
      node = new_node(ND_NUM, tok);
      node->fval = tok->fval;
    } else {
      node = new_num(tok->val, tok);
    }

    node->ty = tok->ty;
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
    chain_expr(&node, compute_vla_size(ty, tok));
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
    Node *dest = labels;
    for (; dest; dest = dest->goto_next)
      if (!strcmp(x->label, dest->label))
        break;
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
      error_tok(x->tok->next, "jump crosses defer statment");

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
    fn->is_static = attr->is_static || (attr->is_inline && !attr->is_extern);
  } else if (!fn->is_static && attr->is_static) {
    error_tok(name, "static declaration follows a non-static declaration");
  }
  fn->is_inline |= attr->is_inline;
  return fn;
}

static void func_definition(Token **rest, Token *tok, Type *ty, VarAttr *attr, Token *name) {
  Obj *fn = func_prototype(ty, attr, name);

  if (fn->is_definition)
    error_tok(tok, "redefinition of %s", fn->name);
  fn->is_definition = true;
  fn->ty = ty;

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

  if (fn_use_vla && !dont_dealloc_vla && !dont_reuse_stack)
    fn->dealloc_vla = true;

  leave_scope();
  resolve_goto_labels();
  current_fn = NULL;
}

static Token *global_declaration(Token *tok, Type *basety, VarAttr *attr) {
  bool first = true;
  for (; comma_list(&tok, &tok, ";", !first); first = false) {
    Token *name = NULL;
    int alt_align = attr->align;
    Type *ty = declarator2(&tok, tok, basety, &name, &alt_align);

    if (ty->kind == TY_FUNC) {
      if (!name)
        error_tok(tok, "function name omitted");

      if (equal(tok, "{")) {
        if (!first || scope->parent)
          error_tok(tok, "function definition is not allowed here");
        func_definition(&tok, tok, ty, attr, name);
        return tok;
      }
      func_prototype(ty, attr, name);
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
      if (!is_definition)
        continue;
      if (sc->var->is_definition && !sc->var->is_tentative)
        continue;
      var = sc->var;
      var->is_tentative = false;
      var->ty = ty;
    } else {
      var = new_gvar(get_ident(name), ty);
    }
    var->is_definition = is_definition;
    var->is_static = attr->is_static;
    var->is_tls = attr->is_tls;
    if (alt_align)
      var->align = alt_align;

    if (attr->is_constexpr) {
      if (!equal(tok, "="))
        error_tok(tok, "constexpr variable not initialized");
      constexpr_initializer(&tok, tok->next, var, var);
      var->is_static = true;
      continue;
    }
    if (equal(tok, "="))
      gvar_initializer(&tok, tok->next, var);
    else if (is_definition && !attr->is_tls)
      var->is_tentative = true;
  }
  return tok;
}

// program = (typedef | function-definition | global-variable)*
Obj *parse(Token *tok) {
  globals = NULL;

  while (tok->kind != TK_EOF) {
    if (equal(tok, "_Static_assert") || equal_kw(tok, "static_assert")) {
      static_assertion(&tok, tok->next);
      continue;
    }

    VarAttr attr = {0};
    Type *basety = declspec(&tok, tok, &attr);

    // Typedef
    if (attr.is_typedef) {
      parse_typedef(&tok, tok, basety, &attr);
      continue;
    }

    // Global declarations
    tok = global_declaration(tok, basety, &attr);
  }

  for (Obj *var = globals; var; var = var->next)
    if (var->ty->kind == TY_FUNC)
      if (var->is_referenced || !(var->is_static && var->is_inline))
        mark_fn_live(var);

  return globals;
}

#include "slimcc.h"

Type *ty_void = &(Type){TY_VOID, 1, 1};
Type *ty_bool = &(Type){TY_BOOL, 1, 1, true};
Type *ty_nullptr = &(Type){TY_NULLPTR, 8, 8};

Type *ty_pchar = &(Type){TY_PCHAR, 1, 1};

Type *ty_char = &(Type){TY_CHAR, 1, 1};
Type *ty_short = &(Type){TY_SHORT, 2, 2};
Type *ty_int = &(Type){TY_INT, 4, 4};
Type *ty_long = &(Type){TY_LONG, 8, 8};
Type *ty_llong = &(Type){TY_LONGLONG, 8, 8};

Type *ty_uchar = &(Type){TY_CHAR, 1, 1, true};
Type *ty_ushort = &(Type){TY_SHORT, 2, 2, true};
Type *ty_uint = &(Type){TY_INT, 4, 4, true};
Type *ty_ulong = &(Type){TY_LONG, 8, 8, true};
Type *ty_ullong = &(Type){TY_LONGLONG, 8, 8, true};

Type *ty_float = &(Type){TY_FLOAT, 4, 4};
Type *ty_double = &(Type){TY_DOUBLE, 8, 8};
Type *ty_ldouble = &(Type){TY_LDOUBLE, 16, 16};

Type *ty_size_t;
Type *ty_intptr_t;
Type *ty_ptrdiff_t;

Type *ty_char16_t;
Type *ty_char32_t;
Type *ty_wchar_t;

Type *enum_ty[8];
EnumType ety_of_int;

void init_ty_lp64(void) {
  define_macro("_LP64", "1");
  define_macro("__LP64__", "1");
  define_macro("__SIZEOF_DOUBLE__", "8");
  define_macro("__SIZEOF_FLOAT__", "4");
  define_macro("__SIZEOF_INT__", "4");
  define_macro("__SIZEOF_LONG_DOUBLE__", "16");
  define_macro("__SIZEOF_LONG_LONG__", "8");
  define_macro("__SIZEOF_LONG__", "8");
  define_macro("__SIZEOF_POINTER__", "8");
  define_macro("__SIZEOF_PTRDIFF_T__", "8");
  define_macro("__SIZEOF_SHORT__", "2");
  define_macro("__SIZEOF_SIZE_T__", "8");
  define_macro("__SIZE_TYPE__", "long unsigned int");

  ty_size_t = ty_ulong;
  ty_intptr_t = ty_ptrdiff_t = ty_long;

  ty_char16_t = ty_ushort;
  ty_char32_t = ty_uint;
  ty_wchar_t = ty_int;

  enum_ty[ETY_I8] = ty_char;
  enum_ty[ETY_U8] = ty_uchar;
  enum_ty[ETY_I16] = ty_short;
  enum_ty[ETY_U16] = ty_ushort;
  enum_ty[ETY_I32] = ty_int;
  enum_ty[ETY_U32] = ty_uint;
  enum_ty[ETY_I64] = ty_long;
  enum_ty[ETY_U64] = ty_ulong;

  ety_of_int = ETY_I32;
}

Type *new_type(TypeKind kind, int64_t size, int32_t align) {
  Type *ty = ast_arena_calloc(sizeof(Type));
  ty->kind = kind;
  ty->size = size;
  ty->align = align;
  return ty;
}

Type *new_bitint(int64_t width, Token *tok) {
  if (width < 0 || width > 65535)
    error_tok(tok, "unsupported _BitInt size");

  int sz, align;

  if (width <= 8)
    sz = align = 1;
  else if (width <= 16)
    sz = align = 2;
  else if (width <= 32)
    sz = align = 4;
  else if (width <= 64)
    sz = align = 8;
  else
    sz = align_to(width, 64) / 8, align = 8; // ARM64 align to 16

  Type *ty = new_type(TY_BITINT, sz, align);
  ty->bit_cnt = width;
  return ty;
}

Type *copy_type(Type *ty) {
  Type *ret = ast_arena_malloc(sizeof(Type));
  *ret = *ty;
  return ret;
}

Type *unqual(Type *ty) {
  if (ty->origin)
    ty = ty->origin;

  if (ty->is_atomic || ty->is_const || ty->is_volatile || ty->is_restrict) {
    ty = copy_type(ty);
    ty->is_atomic = false;
    ty->is_const = false;
    ty->is_volatile = false;
    ty->is_restrict = false;
  }
  return ty;
}

Type *new_qualified_type(Type *ty) {
  if (ty->origin)
    ty = ty->origin;

  Type *ret;
  if (ty->size < 0)
    ret = malloc(sizeof(Type));
  else
    ret = ast_arena_malloc(sizeof(Type));

  *ret = *ty;
  ret->origin = ty;

  if (ty->size < 0) {
    ret->decl_next = ty->decl_next;
    ty->decl_next = ret;
  }
  return ret;
}

void cvqual_type(Type **ty_p, Type *ty2) {
  Type *ty = *ty_p;
  if (ty->is_const < ty2->is_const || ty->is_volatile < ty2->is_volatile) {
    *ty_p = new_qualified_type(ty);
    (*ty_p)->is_const = ty->is_const | ty2->is_const;
    (*ty_p)->is_volatile = ty->is_volatile | ty2->is_volatile;
  }
}

bool mem_iter(Member **mem) {
  Member *m = *mem;
  while (m && m->is_bitfield && !m->name)
    m = m->next;
  *mem = m;
  return m;
}

bool is_pow_of_two(uint64_t val) {
  return !(val & (val - 1));
}

int next_pow_of_two(int val) {
  if (val > 32)
    return 64;
  if (val > 16)
    return 32;
  if (val > 8)
    return 16;
  return 8;
}

int32_t bitfield_footprint(Member *mem) {
  return align_to(mem->bit_width + mem->bit_offset, 8) / 8;
}

bool is_integer(Type *ty) {
  TypeKind k = ty->kind;
  return k == TY_BOOL || k == TY_PCHAR || k == TY_CHAR || k == TY_SHORT ||
         k == TY_INT  || k == TY_LONG || k == TY_LONGLONG;
}

bool is_flonum(Type *ty) {
  return ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE ||
         ty->kind == TY_LDOUBLE;
}

bool is_numeric(Type *ty) {
  return is_integer(ty) || is_flonum(ty) || ty->kind == TY_BITINT;
}

bool is_array(Type *ty) {
  return ty->kind == TY_ARRAY || ty->kind == TY_VLA;
}

bool is_bitfield(Node *node) {
  return node->kind == ND_MEMBER && node->member->is_bitfield;
}

bool is_ptr(Type *ty) {
  return ty->kind == TY_PTR || ty->kind == TY_NULLPTR;
}

static bool is_bitfield2(Node *node, int *width) {
  switch (node->kind) {
  case ND_ASSIGN:
  case ND_ARITH_ASSIGN:
  case ND_POST_INCDEC:
    return is_bitfield2(node->lhs, width);
  case ND_CHAIN:
  case ND_COMMA:
    return is_bitfield2(node->rhs, width);
  case ND_STMT_EXPR: {
    Node *stmt = node->body;
    while (stmt->next)
      stmt = stmt->next;
    if (stmt->kind == ND_EXPR_STMT)
      return is_bitfield2(stmt->lhs, width);
  }
  case ND_MEMBER:
    if (!node->member->is_bitfield)
      return false;
    *width = node->member->bit_width;
    return true;
  }
  return false;
}

bool is_redundant_cast(Node *expr, Type *ty) {
  if (expr->kind != ND_CAST)
    return false;

  Type *ty2 = expr->ty;
  Type *ty3 = expr->lhs->ty;
  int sz = ty->size;
  int sz2 = ty2->size;
  int sz3 = ty3->size;

  if (is_integer(ty) && is_integer(ty2) && is_integer(ty3)) {
    if (ty3->kind == TY_BOOL)
      return true;
    if (ty2->kind == TY_BOOL)
      return false;
    if (ty->kind == TY_BOOL)
      return sz2 >= sz3;

    if (sz <= sz3)
      return sz <= sz2;
    if (sz <= sz2)
      return true;
    if (sz2 == sz3)
      return ty3->is_unsigned == ty2->is_unsigned;
    if (sz2 > sz3)
      return ty3->is_unsigned || !ty2->is_unsigned;
  }
  return false;
}

static void cast_if_not(Type *ty, Node **node) {
  if ((*node)->ty != ty)
    *node = new_cast(*node, ty);
}

bool match_enum_val(EnumVal **e, int64_t val, Token *name) {
  for (EnumVal *ev = *e; ev; ev = ev->next) {
    if ((ev->val == val) && equal_tok(ev->name, name)) {
      if (ev == *e)
        (*e) = (*e)->next;
      return true;
    }
  }
  return false;
}

static bool is_enum_compat(EnumVal *ev1, EnumVal *ev2) {
  int64_t cnt = 0;
  for (EnumVal *ev = ev2; ev; ev = ev->next)
    cnt++;

  for (; ev1; ev1 = ev1->next) {
    if (!match_enum_val(&ev2, ev1->val, ev1->name))
      return false;
    cnt--;
  }
  return !cnt;
}

static int64_t *get_arr_len(Type *ty) {
  if ((ty->kind == TY_ARRAY && ty->array_len >= 0) ||
    (ty->kind == TY_VLA && !ty->vla_len)) {
    return &ty->array_len;
  }
  return NULL;
}

static bool is_tag_compat(Type *t1, Type *t2) {
  return opt_std >= STD_C23 &&
    t1->tag && t2->tag && equal_tok(t1->tag, t2->tag);
}

static bool is_qual_compat(Type *t1, Type *t2) {
  return t1->is_atomic == t2->is_atomic &&
    t1->is_const == t2->is_const &&
    t1->is_volatile == t2->is_volatile &&
    t1->is_restrict == t2->is_restrict;
}

static bool is_record_compat(Type *t1, Type *t2) {
  if (t1->align != t2->align ||
    t1->is_flexible != t2->is_flexible)
    return false;

  Member *mem1 = t1->members;
  Member *mem2 = t2->members;
  while (mem1 && mem2) {
    if (mem1->offset != mem2->offset ||
      mem1->alt_align != mem2->alt_align ||
      mem1->is_bitfield != mem2->is_bitfield ||
      mem1->bit_offset != mem2->bit_offset ||
      mem1->bit_width != mem2->bit_width)
      return false;

    if ((!mem1->name != !mem2->name) ||
      (mem1->name && !equal_tok(mem1->name, mem2->name)))
      return false;

    Type *t1 = mem1->ty;
    Type *t2 = mem2->ty;
    if (t1->kind != t2->kind)
      return false;

    if (t1->kind == TY_STRUCT || t1->kind == TY_UNION) {
      if (!mem1->name != !t1->tag || !mem2->name != !t2->tag ||
        (t1->tag && !equal_tok(t1->tag, t2->tag)))
        return false;
      if (!is_qual_compat(t1, t2) || !is_record_compat(t1, t2))
        return false;
    } else {
      if (!is_compatible2(t1, t2))
        return false;
    }
    mem1 = mem1->next;
    mem2 = mem2->next;
  }
  return !mem1 == !mem2;
}

bool is_compatible2(Type *t1, Type *t2) {
  return is_qual_compat(t1, t2) && is_compatible(t1, t2);
}

bool is_compatible(Type *t1, Type *t2) {
  if (t1 == t2)
    return true;

  if (t1->origin)
    return is_compatible(t1->origin, t2);

  if (t2->origin)
    return is_compatible(t1, t2->origin);

  if (t1->is_enum && t2->is_enum)
    return t1->is_int_enum == t2->is_int_enum &&
      is_tag_compat(t1, t2) && is_enum_compat(t1->enums, t2->enums);

  if (is_array(t1) && is_array(t2)) {
    int64_t *len1 = get_arr_len(t1);
    int64_t *len2 = get_arr_len(t2);

    if (!len1 || !len2 || *len1 == *len2)
      return is_compatible2(t1->base, t2->base);
    return false;
  }

  if (t1->kind != t2->kind)
    return false;

  switch (t1->kind) {
  case TY_PCHAR:
  case TY_CHAR:
  case TY_SHORT:
  case TY_INT:
  case TY_LONG:
  case TY_LONGLONG:
    return t1->is_unsigned == t2->is_unsigned;
  case TY_BITINT:
    return (t1->is_unsigned == t2->is_unsigned) &&
      (t1->bit_cnt == t2->bit_cnt);
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
    return true;
  case TY_PTR:
    return is_compatible2(t1->base, t2->base);
  case TY_FUNC: {
    if (!is_compatible(t1->return_ty, t2->return_ty))
      return false;
    if (t1->is_oldstyle || t2->is_oldstyle)
      return true;
    if (t1->is_variadic != t2->is_variadic)
      return false;
    Obj *p1 = t1->param_list;
    Obj *p2 = t2->param_list;
    for (; p1 && p2; p1 = p1->param_next, p2 = p2->param_next)
      if (!is_compatible(p1->ty, p2->ty))
        return false;
    return p1 == NULL && p2 == NULL;
  }
  case TY_STRUCT:
  case TY_UNION:
    return is_tag_compat(t1, t2) && is_record_compat(t1, t2);
  }
  return false;
}

Type *pointer_to(Type *base) {
  Type *ty = new_type(TY_PTR, 8, 8);
  ty->base = base;
  ty->is_unsigned = true;
  return ty;
}

Type *ptr_decay(Type *ty) {
  if (is_array(ty)) {
    Type *pty = pointer_to(ty->base);
    cvqual_type(&pty->base, ty);
    return pty;
  }
  if (ty->kind == TY_FUNC)
    return pointer_to(ty);
  return ty;
}

void ptr_convert(Node **node) {
  add_type(*node);
  Type *orig = (*node)->ty;
  Type *ty = ptr_decay(orig);
  if (ty != orig)
    *node = new_cast(*node, ty);
}

Type *func_type(Type *return_ty, Token *tok) {
  if (return_ty->base && return_ty->kind != TY_PTR)
    error_tok(tok, "function return type cannot be array");

  // The C spec disallows sizeof(<function type>), but
  // GCC allows that and the expression is evaluated to 1.
  Type *ty = new_type(TY_FUNC, 1, 1);
  ty->return_ty = unqual(return_ty);
  return ty;
}

Type *get_func_ty(Type *ty) {
  if (ty->kind == TY_FUNC)
    return ty;
  if (ty->kind == TY_PTR && ty->base->kind == TY_FUNC)
    return ty->base;
  return NULL;
}

Type *array_of(Type *base, int64_t len) {
  Type *ty = new_type(TY_ARRAY, base->size * len, base->align);
  ty->base = base;
  ty->array_len = len;
  return ty;
}

Type *vla_of(Type *base, Node *len, int64_t arr_len) {
  Type *ty = new_type(TY_VLA, 8, 8);
  ty->base = base;
  if (len)
    ty->vla_len = len;
  else
    ty->array_len = arr_len;
  return ty;
}

Node *assign_cast(Type *to_ty, Node *expr) {
  add_type(expr);
  if (!is_compatible(to_ty, expr->ty)) {
    if (is_numeric(to_ty) || is_ptr(to_ty))
      expr = new_cast(expr, to_ty);
    else
      error_tok(expr->tok, "invalid assignment");
  } else if (expr->ty->size != to_ty->size) {
    error_tok(expr->tok, "invalid assignment");
  }
  return expr;
}

static int int_rank(Type *t) {
  switch (t->kind) {
    case TY_BITINT:
    case TY_BOOL:
    case TY_CHAR:
    case TY_SHORT:
      return 0;
    case TY_INT:
      return 1;
    case TY_LONG:
      return 2;
    case TY_LONGLONG:
      return 3;
  }
  internal_error();
}

bool is_nullptr(Node *node) {
  if (node->ty->kind == TY_NULLPTR)
    return true;

  if (node->kind == ND_CAST &&
    node->ty->kind == TY_PTR && node->ty->base->kind == TY_VOID)
    node = node->lhs;

  int64_t val;
  if (is_integer(node->ty) && is_const_expr(node, &val) && val == 0)
    return true;
  return false;
}

static void int_promotion(Node **node) {
  Type *ty = (*node)->ty;
  int bit_width;

  if (is_bitfield2(*node, &bit_width)) {
    if (ty->kind != TY_BITINT) {
      if (bit_width == (ty_int->size * 8) && ty->is_unsigned)
        ty = ty_uint;
      else if (bit_width <= (ty_int->size * 8))
        ty = ty_int;
    }
    *node = new_cast(*node, ty);
    return;
  }

  if (ty->kind == TY_BITINT)
    return;

  if (ty->size < ty_int->size) {
    *node = new_cast(*node, ty_int);
    return;
  }

  if (ty->size == ty_int->size && int_rank(ty) < int_rank(ty_int)) {
    if (ty->is_unsigned)
      *node = new_cast(*node, ty_uint);
    else
      *node = new_cast(*node, ty_int);
    return;
  }
}

static Type *get_common_ptr_type(Node *lhs, Node *rhs) {
  Type *ty1 = lhs->ty;
  Type *ty2 = rhs->ty;
  bool np1 = is_nullptr(lhs);
  bool np2 = is_nullptr(rhs);

  if (ty1->base && np2)
    return ty1;
  if (ty2->base && np1)
    return ty2;
  if (ty1->kind == TY_NULLPTR && np2)
    return ty1;
  if (ty2->kind == TY_NULLPTR && np1)
    return ty2;
  if (ty1->base && ty2->base) {
    if (is_compatible(ty1->base, ty2->base))
      return ty1;
    return pointer_to(ty_void);
  }
  return NULL;
}

static bool common_ptr_conv(Node **lhs, Node **rhs) {
  ptr_convert(lhs);
  ptr_convert(rhs);
  Type *ty = get_common_ptr_type(*lhs, *rhs);
  if (ty) {
    cast_if_not(ty, lhs);
    cast_if_not(ty, rhs);
    return true;
  }
  return false;
}

static Type *get_common_type(Node **lhs, Node **rhs) {
  Type *ty1 = (*lhs)->ty;
  Type *ty2 = (*rhs)->ty;

  if (!is_numeric(ty1))
    error_tok((*lhs)->tok, "invalid operand");
  if (!is_numeric(ty2))
    error_tok((*rhs)->tok, "invalid operand");

  if (ty1->kind == TY_LDOUBLE || ty2->kind == TY_LDOUBLE)
    return ty_ldouble;
  if (ty1->kind == TY_DOUBLE || ty2->kind == TY_DOUBLE)
    return ty_double;
  if (ty1->kind == TY_FLOAT || ty2->kind == TY_FLOAT)
    return ty_float;

  int_promotion(lhs);
  int_promotion(rhs);
  ty1 = (*lhs)->ty;
  ty2 = (*rhs)->ty;

  int32_t sz1 = ty1->kind == TY_BITINT ? ty1->bit_cnt : ty1->size * 8;
  int32_t sz2 = ty2->kind == TY_BITINT ? ty2->bit_cnt : ty2->size * 8;
  if (sz1 != sz2)
    return sz1 > sz2 ? ty1 : ty2;

  int rnk1 = int_rank(ty1);
  int rnk2 = int_rank(ty2);
  Type *rnk_ty = rnk1 > rnk2 ? ty1 : ty2;

  if (ty1->is_unsigned == ty2->is_unsigned)
    return rnk_ty;

  if (rnk1 == rnk2)
    return ty1->is_unsigned ? ty1 : ty2;

  switch (rnk_ty->kind) {
    case TY_INT:
      return ty_uint;
    case TY_LONG:
      return ty_ulong;
    case TY_LONGLONG:
      return ty_ullong;
  }
  internal_error();
}

static Type *usual_arith_conv(Node **lhs, Node **rhs) {
  Type *ty = get_common_type(lhs, rhs);
  cast_if_not(ty, lhs);
  cast_if_not(ty, rhs);
  return ty;
}

void add_type(Node *node) {
  if (!node || node->ty)
    return;

  add_type(node->lhs);
  add_type(node->rhs);
  add_type(node->cond);
  add_type(node->then);
  add_type(node->els);
  add_type(node->init);
  add_type(node->inc);

  for (Node *n = node->body; n; n = n->next)
    add_type(n);

  switch (node->kind) {
  case ND_NUM:
    node->ty = ty_int;
    return;
  case ND_ADD:
  case ND_SUB: {
    Node *ptr = node->lhs->ty->base ? node->lhs : node->rhs->ty->base ? node->rhs : NULL;
    if (ptr) {
      node->ty = ptr->ty;
      return;
    }
    node->ty = usual_arith_conv(&node->lhs, &node->rhs);
    return;
  }
  case ND_MUL:
  case ND_DIV:
  case ND_MOD:
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
    node->ty = usual_arith_conv(&node->lhs, &node->rhs);
    return;
  case ND_POS:
  case ND_NEG:
    if (!is_numeric(node->lhs->ty))
      error_tok(node->lhs->tok, "invalid operand");
    if (is_integer(node->lhs->ty))
      int_promotion(&node->lhs);
    node->ty = node->lhs->ty;
    return;
  case ND_ASSIGN:
    node->rhs = assign_cast(node->lhs->ty, node->rhs);
    node->ty = node->lhs->ty;
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
    node->ty = ty_int;
    if (common_ptr_conv(&node->lhs, &node->rhs))
      return;
    usual_arith_conv(&node->lhs, &node->rhs);
    return;
  case ND_FUNCALL:
    assert(!!node->ty);
    return;
  case ND_NOT:
  case ND_LOGOR:
  case ND_LOGAND:
    node->ty = ty_int;
    return;
  case ND_BITNOT:
    if (!(is_integer(node->lhs->ty) || node->lhs->ty->kind == TY_BITINT))
      error_tok(node->lhs->tok, "invalid operand");
    int_promotion(&node->lhs);
    node->ty = node->lhs->ty;
    return;
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
    if (!(is_integer(node->lhs->ty) || node->lhs->ty->kind == TY_BITINT))
      error_tok(node->lhs->tok, "invalid operand");
    if (!(is_integer(node->rhs->ty) || node->rhs->ty->kind == TY_BITINT))
      error_tok(node->rhs->tok, "invalid operand");
    if (node->rhs->ty->kind == TY_BITINT)
      node->rhs = new_cast(node->rhs, ty_ullong);
    int_promotion(&node->lhs);
    node->ty = node->lhs->ty;
    return;
  case ND_VAR:
    node->ty = node->var->ty;
    return;
  case ND_COND:
    if (node->then->ty->kind == TY_VOID || node->els->ty->kind == TY_VOID)
      node->ty = ty_void;
    else if (common_ptr_conv(&node->then, &node->els))
      node->ty = node->then->ty;
    else if (!is_numeric(node->then->ty) && is_compatible(node->then->ty, node->els->ty))
      node->ty = node->then->ty;
    else
      node->ty = usual_arith_conv(&node->then, &node->els);
    return;
  case ND_CHAIN:
    node->ty = node->rhs->ty;
    return;
  case ND_COMMA:
    node->ty = ptr_decay(node->rhs->ty);
    return;
  case ND_MEMBER:
    node->ty = node->member->ty;
    return;
  case ND_ADDR:
    node->ty = pointer_to(node->lhs->ty);
    return;
  case ND_DEREF:
    if (!node->lhs->ty->base)
      error_tok(node->tok, "invalid pointer dereference");
    if (node->lhs->ty->base->kind == TY_VOID)
      error_tok(node->tok, "dereferencing a void pointer");

    node->ty = node->lhs->ty->base;
    return;
  case ND_STMT_EXPR:
    if (node->body) {
      Node *stmt = node->body;
      while (stmt->next)
        stmt = stmt->next;
      if (stmt->kind == ND_EXPR_STMT) {
        node->ty = ptr_decay(stmt->lhs->ty);
        return;
      }
    }
    node->ty = ty_void;
    return;
  case ND_LABEL_VAL:
    node->ty = pointer_to(ty_void);
    return;
  case ND_CAS:
    add_type(node->cas_addr);
    add_type(node->cas_old);
    add_type(node->cas_new);
    node->ty = ty_bool;

    if (node->cas_addr->ty->kind != TY_PTR)
      error_tok(node->cas_addr->tok, "pointer expected");
    if (node->cas_old->ty->kind != TY_PTR)
      error_tok(node->cas_old->tok, "pointer expected");
    return;
  case ND_EXCH:
    if (node->lhs->ty->kind != TY_PTR)
      error_tok(node->cas_addr->tok, "pointer expected");
    node->ty = node->lhs->ty->base;
    return;
  case ND_NULL_EXPR:
  case ND_THREAD_FENCE:
  case ND_VA_START:
  case ND_VA_COPY:
    node->ty = ty_void;
    return;
  case ND_CKD_ARITH:
    node->ty = ty_bool;
    return;
  }
}

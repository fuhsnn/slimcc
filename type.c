#include "slimcc.h"

Type *ty_void = &(Type){.kind = TY_VOID, .size = 1, .align = 1};
Type *ty_bool = &(Type){.kind = TY_BOOL, .size = 1, .align = 1, .is_unsigned = true};
Type *ty_nullptr = &(Type){.kind = TY_NULLPTR, .size = 8, .align = 8};

Type *ty_pchar = &(Type){.kind = TY_PCHAR, .size = 1, .align = 1};

Type *ty_char = &(Type){.kind = TY_CHAR, .size = 1, .align = 1};
Type *ty_short = &(Type){.kind = TY_SHORT, .size = 2, .align = 2};
Type *ty_int = &(Type){.kind = TY_INT, .size = 4, .align = 4};
Type *ty_long = &(Type){.kind = TY_LONG, .size = 8, .align = 8};
Type *ty_llong = &(Type){.kind = TY_LONGLONG, .size = 8, .align = 8};

Type *ty_uchar = &(Type){.kind = TY_CHAR, .size = 1, .align = 1, .is_unsigned = true};
Type *ty_ushort = &(Type){.kind = TY_SHORT, .size = 2, .align = 2, .is_unsigned = true};
Type *ty_uint = &(Type){.kind = TY_INT, .size = 4, .align = 4, .is_unsigned = true};
Type *ty_ulong = &(Type){.kind = TY_LONG, .size = 8, .align = 8, .is_unsigned = true};
Type *ty_ullong = &(Type){.kind = TY_LONGLONG, .size = 8, .align = 8, .is_unsigned = true};

Type *ty_float = &(Type){.kind = TY_FLOAT, .size = 4, .align = 4};
Type *ty_double = &(Type){.kind = TY_DOUBLE, .size = 8, .align = 8};
Type *ty_ldouble = &(Type){.kind = TY_LDOUBLE, .size = 16, .align = 16};

Type *ty_size_t;
Type *ty_ptrdiff_t;

Type *ty_char16_t;
Type *ty_char32_t;
Type *ty_wchar_t;

Type *enum_ty[8];
EnumType ety_of_int;

void init_ty_lp64(void) {
  define_macro("_LP64", "1");
  define_macro("__LP64__", "1");
  define_macro("__SIZEOF_POINTER__", "8");

  define_macro("__SIZEOF_LONG_DOUBLE__", "16");
  define_macro("__SIZEOF_DOUBLE__", "8");
  define_macro("__SIZEOF_FLOAT__", "4");
  define_macro("__SIZEOF_LONG_LONG__", "8");
  define_macro("__SIZEOF_LONG__", "8");
  define_macro("__SIZEOF_INT__", "4");
  define_macro("__SIZEOF_SHORT__", "2");

  define_macro("__LONG_LONG_MAX__", "0x7fffffffffffffffLL");
  define_macro("__LONG_MAX__", "0x7fffffffffffffffL");
  define_macro("__INT_MAX__", "0x7fffffff");
  define_macro("__SHRT_MAX__", "0x7fff");
  define_macro("__SCHAR_MAX__", "0x7f");

  define_macro("__SIZEOF_SIZE_T__", "8");
  define_macro("__SIZEOF_PTRDIFF_T__", "8");
  define_macro("__SIZEOF_WCHAR_T__", "4");

  define_macro("__SIZE_TYPE__", "long unsigned int");
  define_macro("__PTRDIFF_TYPE__", "long int");
  define_macro("__WCHAR_TYPE__", "int");

  ty_size_t = ty_ulong;
  ty_ptrdiff_t = ty_long;
  ty_wchar_t = ty_int;

  ty_char16_t = ty_ushort;
  ty_char32_t = ty_uint;

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
  Type *ty = calloc(1, sizeof(Type));
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
  Type *ret = malloc(sizeof(Type));
  *ret = *ty;
  return ret;
}

Type *unqual(Type *ty) {
  return ty->origin ? ty->origin : ty;
}

Type *new_derived_type(Type *newty, QualMask qual, Type *ty, Token *tok) {
  ty = ty->origin ? ty->origin : ty;

  if (tok && ty->kind != TY_AUTO)
    if (qual & Q_RESTRICT)
      if (ty->kind != TY_PTR || ty->base->kind == TY_FUNC)
        error_tok(tok, "type cannot be restrict qualified");

  if (!newty)
    newty = malloc(sizeof(Type));
  *newty = *ty;
  newty->origin = ty;
  newty->qual = qual;
  return newty;
}

Type *qual_type(QualMask msk, Type *ty, Token *tok) {
  if (msk == (msk & ty->qual))
    return ty;

  Type *ret = new_derived_type(NULL, msk | ty->qual, ty, tok);

  if (ty->size < 0) {
    ret->decl_next = ty->decl_next;
    ty->decl_next = ret;
  }
  return ret;
}

void cvqual_type(Type **ty_p, Type *ty2) {
  QualMask msk = ty2->qual & (Q_CONST | Q_VOLATILE);
  if (msk)
    *ty_p = qual_type(msk, *ty_p, NULL);
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
  return node->kind == ND_MEMBER && node->m.member->is_bitfield;
}

bool is_ptr(Type *ty) {
  return ty->kind == TY_PTR || ty->kind == TY_NULLPTR;
}

static bool is_bitfield2(Node *node, int *width) {
  switch (node->kind) {
  case ND_ASSIGN:
  case ND_ARITH_ASSIGN:
  case ND_POST_INCDEC:
    return is_bitfield2(node->m.lhs, width);
  case ND_CHAIN:
  case ND_COMMA:
    return is_bitfield2(node->m.rhs, width);
  case ND_STMT_EXPR: {
    Node *stmt = node->blk.body;
    while (stmt->next)
      stmt = stmt->next;
    if (stmt->kind == ND_EXPR_STMT)
      return is_bitfield2(stmt->m.lhs, width);
  }
  case ND_MEMBER:
    if (!node->m.member->is_bitfield)
      return false;
    *width = node->m.member->bit_width;
    return true;
  }
  return false;
}

bool is_redundant_cast(Node *expr, Type *ty) {
  if (expr->kind != ND_CAST)
    return false;

  Type *ty2 = expr->ty;
  Type *ty3 = expr->m.lhs->ty;
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

static bool int_to_ptr(Node **node) {
  if (!is_integer((*node)->ty))
    return false;
  if ((*node)->ty->size != ty_nullptr->size)
    *node = new_cast(*node, pointer_to(ty_void));
  return true;
}

static bool match_enum_val(EnumVal **e, int64_t val, Token *name) {
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
    (ty->kind == TY_VLA && !ty->vla_len))
    return &ty->array_len;
  return NULL;
}

static bool is_tag_compat(Type *t1, Type *t2) {
  return opt_std >= STD_C23 &&
    t1->tag && t2->tag && equal_tok(t1->tag, t2->tag);
}

static bool is_arr_qual_compat(Type *t1, Type *t2) {
  QualMask q1 = 0, q2 = 0;
  do {
    q1 |= t1->qual;
    q2 |= t2->qual;

    t1 = t1->base;
    t2 = t2->base;
  } while (is_array(t1) && is_array(t2));

  q1 |= t1->qual;
  q2 |= t2->qual;
  return q1 == q2;
}

static bool is_qual_compat(Type *t1, Type *t2) {
  if (is_array(t1) && is_array(t2))
    return is_arr_qual_compat(t1, t2);

  return t1->qual == t2->qual;
}

bool is_record_compat(Type *t1, Type *t2) {
  if (t1->size < 0 || t2->size < 0 ||
    t1->align != t2->align ||
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
  if (t1->origin)
    t1 = t1->origin;

  if (t2->origin)
    t2 = t2->origin;

  if (t1 == t2)
    return true;

  if (t1->is_enum && t2->is_enum)
    return t1->is_int_enum == t2->is_int_enum &&
      is_tag_compat(t1, t2) && is_enum_compat(t1->enums, t2->enums);

  if (is_array(t1) && is_array(t2)) {
    int64_t *len1 = get_arr_len(t1);
    int64_t *len2 = get_arr_len(t2);

    if (!len1 || !len2 || *len1 == *len2)
      return is_compatible(t1->base, t2->base);
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
  if (base == ty_void) {
    static Type *vp;
    if (!vp) {
      vp = new_type(TY_PTR, 8, 8);
      vp->base = base;
      vp->is_unsigned = true;
    }
    return vp;
  }
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
  if (is_array(return_ty) || return_ty->kind == TY_FUNC)
    error_tok(tok, "invalid function return type");

  // The C spec disallows sizeof(<function type>), but
  // GCC allows that and the expression is evaluated to 1.
  Type *ty = new_type(TY_FUNC, 1, 1);
  ty->return_ty = unqual(return_ty);
  return ty;
}

Type *get_func_ty(Node *node) {
  add_type(node);
  Type *ty = node->ty;
  if (ty->kind == TY_FUNC)
    return ty;
  if (ty->kind == TY_PTR && ty->base->kind == TY_FUNC)
    return ty->base;
  error_tok(node->tok, "not a function");
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
  if (len) {
    add_type(len);
    cast_if_not(ty_size_t, &len);
    ty->vla_len = len;
  } else {
    ty->array_len = arr_len;
  }
  return ty;
}

Node *assign_cast(Type *to_ty, Node *expr) {
  add_type(expr);

  if (is_ptr(to_ty)) {
    ptr_convert(&expr);
    if (is_ptr(expr->ty))
      return expr;
    if (is_nullptr(expr))
      return new_cast(expr, to_ty);
  } else if (is_compatible(to_ty, expr->ty)) {
    if (to_ty->kind != TY_VOID && to_ty->size >= 0)
      return expr;
  } else if (is_numeric(to_ty)) {
    return new_cast(expr, to_ty);
  }
  error_tok(expr->tok, "invalid assignment");
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
    node->ty->kind == TY_PTR && is_compatible2(node->ty->base, ty_void))
    node = node->m.lhs;

  int64_t val;
  return is_integer(node->ty) && is_const_expr(node, &val) && val == 0;
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

static Type *cond_ptr_conv2(Type *ty1, Type *ty2, int msk, Node **cond, Obj **cond_var) {
  msk |= ty1->qual | ty2->qual;

  if (is_array(ty1)) {
    Type *base = cond_ptr_conv2(ty1->base, ty2->base, msk, cond, cond_var);
    int64_t *len;
    if ((len = get_arr_len(ty1)) || (len = get_arr_len(ty2))) {
      if (base->kind == TY_VLA)
        return vla_of(base, NULL, *len);
      return array_of(base, *len);
    }
    if (ty1->vla_len || ty2->vla_len)
      return vla_cond_result_len(ty1, ty2, base, cond, cond_var);

    return array_of(base, -1);
  }
  return qual_type(msk, ty1, NULL);
}

static Type *cond_ptr_conv(Node **lhs, Node **rhs, Node **cond) {
  Type *ty1 = (*lhs)->ty;
  Type *ty2 = (*rhs)->ty;

  if (ty1->kind == TY_PTR && (int_to_ptr(rhs) || is_nullptr(*rhs)))
    return ty1;
  if (ty2->kind == TY_PTR && (int_to_ptr(lhs) || is_nullptr(*lhs)))
    return ty2;
  if (ty1->kind == TY_PTR && ty2->kind == TY_PTR) {
    if (ty1->base->kind == TY_VOID || ty2->base->kind == TY_VOID)
      return pointer_to(qual_type(ty1->base->qual | ty2->base->qual, ty_void, NULL));

    if (is_compatible(ty1->base, ty2->base))
      return pointer_to(cond_ptr_conv2(ty1->base, ty2->base, 0, cond, &(Obj *){0}));

    return pointer_to(ty_void);
  }
  if (ty1->kind == TY_NULLPTR && ty2->kind == TY_NULLPTR)
    return ty_nullptr;

  internal_error();
}

static void add_int_type(Node *node) {
  add_type(node);
  if (!(is_integer(node->ty) || node->ty->kind == TY_BITINT))
    error_tok(node->tok, "invalid operand");
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

void add_type_chk_const(Node *node) {
  add_type(node);
  if (node->ty->qual & Q_CONST)
    error_tok(node->tok, "operand is const");
}

void add_type(Node *node) {
  if (!node || node->ty)
    return;

  switch (node->kind) {
  case ND_NUM:
    node->ty = ty_int;
    return;
  case ND_ADD:
  case ND_SUB: {
    add_type(node->m.lhs);
    add_type(node->m.rhs);
    Node *ptr = node->m.lhs->ty->base ? node->m.lhs : node->m.rhs->ty->base ? node->m.rhs : NULL;
    if (ptr)
      node->ty = ptr->ty;
    else
      node->ty = usual_arith_conv(&node->m.lhs, &node->m.rhs);
    return;
  }
  case ND_MUL:
  case ND_DIV:
    add_type(node->m.lhs);
    add_type(node->m.rhs);
    node->ty = usual_arith_conv(&node->m.lhs, &node->m.rhs);
    return;
  case ND_MOD:
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
    add_int_type(node->m.lhs);
    add_int_type(node->m.rhs);
    node->ty = usual_arith_conv(&node->m.lhs, &node->m.rhs);
    return;
  case ND_POS:
  case ND_NEG:
    add_type(node->m.lhs);
    if (!is_numeric(node->m.lhs->ty))
      error_tok(node->m.lhs->tok, "invalid operand");
    if (is_integer(node->m.lhs->ty))
      int_promotion(&node->m.lhs);
    node->ty = node->m.lhs->ty;
    return;
  case ND_ASSIGN:
    add_type(node->m.lhs);
    add_type(node->m.rhs);
    node->m.rhs = assign_cast(node->m.lhs->ty, node->m.rhs);
    node->ty = node->m.lhs->ty;
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
    node->ty = ty_int;
    ptr_convert(&node->m.lhs);
    ptr_convert(&node->m.rhs);
    if ((is_ptr(node->m.lhs->ty) && (is_ptr(node->m.rhs->ty) || int_to_ptr(&node->m.rhs))) ||
      (is_ptr(node->m.rhs->ty) && (is_ptr(node->m.lhs->ty) || int_to_ptr(&node->m.lhs))))
      return;
    usual_arith_conv(&node->m.lhs, &node->m.rhs);
    return;
  case ND_FUNCALL:
    assert(!!node->ty);
    return;
  case ND_NOT:
    add_type(node->m.lhs);
    node->ty = ty_int;
    return;
  case ND_LOGOR:
  case ND_LOGAND:
    add_type(node->m.lhs);
    add_type(node->m.rhs);
    node->ty = ty_int;
    return;
  case ND_BITNOT:
    add_int_type(node->m.lhs);
    int_promotion(&node->m.lhs);
    node->ty = node->m.lhs->ty;
    return;
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
    add_int_type(node->m.lhs);
    add_int_type(node->m.rhs);
    if (node->m.rhs->ty->kind == TY_BITINT)
      node->m.rhs = new_cast(node->m.rhs, ty_ullong);
    int_promotion(&node->m.lhs);
    node->ty = node->m.lhs->ty;
    return;
  case ND_VAR:
    node->ty = node->m.var->ty;
    return;
  case ND_COND: {
    add_type(node->ctrl.cond);
    Node **lhs = &node->ctrl.then;
    Node **rhs = &node->ctrl.els;
    ptr_convert(lhs);
    ptr_convert(rhs);

    if ((*lhs)->ty->kind == TY_VOID || (*rhs)->ty->kind == TY_VOID)
      node->ty = ty_void;
    else if (is_ptr((*lhs)->ty) || is_ptr((*rhs)->ty))
      node->ty = cond_ptr_conv(lhs, rhs, &node->ctrl.cond);
    else if (!is_numeric((*lhs)->ty) && (*lhs)->ty->size >= 0 &&
      is_compatible((*lhs)->ty, (*rhs)->ty))
      node->ty = (*lhs)->ty;
    else
      node->ty = usual_arith_conv(lhs, rhs);
    return;
  }
  case ND_CHAIN:
    add_type(node->m.lhs);
    add_type(node->m.rhs);
    node->ty = node->m.rhs->ty;
    return;
  case ND_COMMA:
    add_type(node->m.lhs);
    add_type(node->m.rhs);
    node->ty = ptr_decay(node->m.rhs->ty);
    return;
  case ND_MEMBER:
    add_type(node->m.lhs);
    node->ty = node->m.member->ty;
    return;
  case ND_ADDR:
    add_type(node->m.lhs);
    node->ty = pointer_to(node->m.lhs->ty);
    return;
  case ND_DEREF:
    add_type(node->m.lhs);
    if (!node->m.lhs->ty->base)
      error_tok(node->tok, "invalid pointer dereference");

    node->ty = node->m.lhs->ty->base;
    return;
  case ND_FOR:
    add_type(node->ctrl.cond);
    add_type(node->ctrl.then);
    add_type(node->ctrl.for_init);
    add_type(node->ctrl.for_inc);
    return;
  case ND_IF:
    add_type(node->ctrl.cond);
    add_type(node->ctrl.then);
    add_type(node->ctrl.els);
    return;
  case ND_DO:
  case ND_SWITCH:
    add_type(node->ctrl.cond);
    add_type(node->ctrl.then);
    return;
  case ND_EXPR_STMT:
    add_type(node->m.lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->blk.body; n; n = n->next)
      add_type(n);
    break;
  case ND_STMT_EXPR:
    if (node->blk.body) {
      for (Node *n = node->blk.body; n; n = n->next)
        add_type(n);
      Node *stmt = node->blk.body;
      while (stmt->next)
        stmt = stmt->next;
      if (stmt->kind == ND_EXPR_STMT) {
        node->ty = ptr_decay(stmt->m.lhs->ty);
        return;
      }
    }
    node->ty = ty_void;
    return;
  case ND_LABEL_VAL:
    node->ty = pointer_to(ty_void);
    return;
  case ND_CAS:
    add_type(node->cas.addr);
    add_type(node->cas.old_val);
    add_type(node->cas.new_val);
    node->ty = ty_bool;

    if (node->cas.addr->ty->kind != TY_PTR)
      error_tok(node->cas.addr->tok, "pointer expected");
    if (node->cas.old_val->ty->kind != TY_PTR)
      error_tok(node->cas.old_val->tok, "pointer expected");
    return;
  case ND_EXCH:
    add_type(node->m.lhs);
    add_type(node->m.rhs);
    if (node->m.lhs->ty->kind != TY_PTR)
      error_tok(node->m.lhs->tok, "pointer expected");
    node->ty = node->m.lhs->ty->base;
    return;
  case ND_VA_START:
    add_type(node->m.lhs);
    node->ty = ty_void;
    return;
  case ND_VA_COPY:
    add_type(node->m.lhs);
    add_type(node->m.rhs);
    node->ty = ty_void;
    return;
  case ND_CKD_ARITH:
    add_type(node->m.lhs);
    add_type(node->m.rhs);
    add_type(node->m.target);
    node->ty = ty_bool;
    return;
  case ND_NULL_EXPR:
  case ND_THREAD_FENCE:
  case ND_UNREACHABLE:
    node->ty = ty_void;
    return;
  }
}

#include "slimcc.h"

Type *ty_void = &(Type){TY_VOID, 1, 1};
Type *ty_bool = &(Type){TY_BOOL, 1, 1, true};

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

Type *new_type(TypeKind kind, int64_t size, int32_t align) {
  Type *ty = ast_arena_calloc(sizeof(Type));
  ty->kind = kind;
  ty->size = size;
  ty->align = align;
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
  return is_integer(ty) || is_flonum(ty);
}

bool is_array(Type *ty) {
  return ty->kind == TY_ARRAY || ty->kind == TY_VLA;
}

bool is_bitfield(Node *node) {
  return node->kind == ND_MEMBER && node->member->is_bitfield;
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

bool is_compatible2(Type *t1, Type *t2) {
  if (t1->is_atomic != t2->is_atomic)
    return false;
  if (t1->is_const != t2->is_const)
    return false;
  if (t1->is_volatile != t2->is_volatile)
    return false;
  if (t1->is_restrict != t2->is_restrict)
    return false;
  return is_compatible(t1, t2);
}

bool is_compatible(Type *t1, Type *t2) {
  if (t1 == t2)
    return true;

  if (t1->origin)
    return is_compatible(t1->origin, t2);

  if (t2->origin)
    return is_compatible(t1, t2->origin);

  if ((t1->kind == TY_VLA && is_array(t2)) ||
    (is_array(t1) && t2->kind == TY_VLA))
    return is_compatible2(t1->base, t2->base);

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
  case TY_FLOAT:
  case TY_DOUBLE:
  case TY_LDOUBLE:
    return true;
  case TY_PTR:
    return is_compatible2(t1->base, t2->base);
  case TY_FUNC: {
    if (!is_compatible(t1->return_ty, t2->return_ty))
      return false;
    if (t1->is_variadic != t2->is_variadic)
      return false;

    Obj *p1 = t1->param_list;
    Obj *p2 = t2->param_list;
    for (; p1 && p2; p1 = p1->param_next, p2 = p2->param_next)
      if (!is_compatible(p1->ty, p2->ty))
        return false;
    return p1 == NULL && p2 == NULL;
  }
  case TY_ARRAY:
    if (!is_compatible2(t1->base, t2->base))
      return false;
    return t1->array_len < 0 || t2->array_len < 0 ||
           t1->array_len == t2->array_len;
  }
  return false;
}

Type *pointer_to(Type *base) {
  Type *ty = new_type(TY_PTR, 8, 8);
  ty->base = base;
  ty->is_unsigned = true;
  return ty;
}

Type *array_to_pointer(Type *ty) {
  if (ty->base && ty->kind != TY_PTR)
    return pointer_to(ty->base);
  return ty;
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

Type *array_of(Type *base, int64_t len) {
  Type *ty = new_type(TY_ARRAY, base->size * len, base->align);
  ty->base = base;
  ty->array_len = len;
  return ty;
}

Type *vla_of(Type *base, Node *len) {
  Type *ty = new_type(TY_VLA, 8, 8);
  ty->base = base;
  ty->vla_len = len;
  return ty;
}

int int_rank(Type *t) {
  switch (t->kind) {
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
    int int_width = ty_int->size * 8;

    if (bit_width == int_width && ty->is_unsigned) {
      *node = new_cast(*node, ty_uint);
    } else if (bit_width <= int_width) {
      *node = new_cast(*node, ty_int);
    } else {
      *node = new_cast(*node, ty);
    }
    return;
  }

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

static Type *get_common_type(Node **lhs, Node **rhs, bool handle_ptr) {
  Type *ty1 = (*lhs)->ty;
  Type *ty2 = (*rhs)->ty;

  if (handle_ptr) {
    if (ty1->kind == TY_FUNC)
      ty1 = pointer_to(ty1);
    if (ty2->kind == TY_FUNC)
      ty2 = pointer_to(ty2);

    if (ty1->base && is_nullptr(*rhs))
      return array_to_pointer(ty1);
    if (ty2->base && is_nullptr(*lhs))
      return array_to_pointer(ty2);

    if (ty1->base && ty2->base) {
      if (is_compatible(ty1->base, ty2->base))
        return array_to_pointer(ty1);
      return pointer_to(ty_void);
    }
  }

  if (!is_numeric(ty1) || !is_numeric(ty2))
    error_tok((*rhs)->tok,"invalid operand");

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

  if (ty1->size != ty2->size)
    return (ty1->size < ty2->size) ? ty2 : ty1;

  Type *ranked_ty = int_rank(ty1) > int_rank(ty2) ? ty1 : ty2;

  if (ty1->is_unsigned == ty2->is_unsigned)
    return ranked_ty;

  // If same size but different sign, the common type is unsigned
  // variant of the highest-ranked type between the two.
  switch (ranked_ty->kind) {
    case TY_INT:
      return ty_uint;
    case TY_LONG:
      return ty_ulong;
    case TY_LONGLONG:
      return ty_ullong;
  }
  internal_error();
}

// For many binary operators, we implicitly promote operands so that
// both operands have the same type. Any integral type smaller than
// int is always promoted to int. If the type of one operand is larger
// than the other's (e.g. "long" vs. "int"), the smaller operand will
// be promoted to match with the other.
//
// This operation is called the "usual arithmetic conversion".
static void usual_arith_conv(Node **lhs, Node **rhs, bool handle_ptr) {
  Type *ty = get_common_type(lhs, rhs, handle_ptr);
  *lhs = new_cast(*lhs, ty);
  *rhs = new_cast(*rhs, ty);
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
    Node **ptr = node->lhs->ty->base ? &node->lhs : node->rhs->ty->base ? &node->rhs : NULL;
    if (ptr) {
      if ((*ptr)->ty->kind != TY_PTR)
        *ptr = new_cast(*ptr, pointer_to((*ptr)->ty->base));
      node->ty = (*ptr)->ty;
      return;
    }
    usual_arith_conv(&node->lhs, &node->rhs, false);
    node->ty = node->lhs->ty;
    return;
  }
  case ND_MUL:
  case ND_DIV:
  case ND_MOD:
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
    usual_arith_conv(&node->lhs, &node->rhs, false);
    node->ty = node->lhs->ty;
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
    if (node->lhs->ty->kind == TY_ARRAY && !node->lhs->var->constexpr_data)
      error_tok(node->lhs->tok, "not an lvalue");
    if (node->lhs->ty->kind != TY_STRUCT && node->lhs->ty->kind != TY_UNION)
      node->rhs = new_cast(node->rhs, node->lhs->ty);
    node->ty = node->lhs->ty;
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
    usual_arith_conv(&node->lhs, &node->rhs, true);
    node->ty = ty_int;
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
    if (!is_integer(node->lhs->ty))
      error_tok(node->lhs->tok, "invalid operand");
    int_promotion(&node->lhs);
    node->ty = node->lhs->ty;
    return;
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
    if (!is_integer(node->lhs->ty))
      error_tok(node->lhs->tok, "invalid operand");
    if (!is_integer(node->rhs->ty))
      error_tok(node->rhs->tok, "invalid operand");
    int_promotion(&node->lhs);
    node->ty = node->lhs->ty;
    return;
  case ND_VAR:
    node->ty = node->var->ty;
    return;
  case ND_COND:
    if (node->then->ty->kind == TY_VOID || node->els->ty->kind == TY_VOID) {
      node->ty = ty_void;
    } else if (!is_numeric(node->then->ty) && is_compatible(node->then->ty, node->els->ty)) {
      node->ty = array_to_pointer(node->then->ty);
    } else {
      usual_arith_conv(&node->then, &node->els, true);
      node->ty = node->then->ty;
    }
    return;
  case ND_CHAIN:
    node->ty = node->rhs->ty;
    return;
  case ND_COMMA:
    node->ty = array_to_pointer(node->rhs->ty);
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
        node->ty = array_to_pointer(stmt->lhs->ty);
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
  case ND_VA_START:
  case ND_VA_COPY:
    node->ty = ty_void;
  }
}

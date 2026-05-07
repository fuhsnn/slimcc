char *regs[REG_XMM0][4];
char *argreg32[] = {""}, *tmpreg64[] = {""}, *argreg64[] = {""};
Reg argreg[] = {REG_SI};
char *tmpreg32[] = {}, *cast_table[] = {};
char *tmpreg64_0, *gen_bitfield_store___trans_tmp_7, *store___trans_tmp_3,
    *gen_bitfield_store___trans_tmp_8, *gen_funcall___trans_tmp_10,
    *arith_ins_ins, *gen_expr2___trans_tmp_11, *gen_expr2_sz,
    *gen_expr2___trans_tmp_2, *gen_expr2_ax;
Obj *current_fn;
char lvar_ptr, gen_bitfield_store_cx, gen_expr2_op;
int va_gp_start, pop_by_ty___trans_tmp_5, tmp_stack_1, va_fp_start, va_st_start,
    gen_vaarg_reg_copy___trans_tmp_1, gen_funcall_arg_stk_align, vla_base_ofs,
    tmp_stack_2, gen_funcall___trans_tmp_4, peak_stk_usage,
    gen_funcall_fp_count, rtn_ptr_ofs, popf_reg, gen_funcall_gp_count,
    rtn_label;
bool dont_reuse_stack;
struct {
  Slot *data;
  int depth;
} tmp_stack;
long load_fval_neg_z;
struct {
  uint64_t u64;
} load_fval_u;
double load_f32_f64_pos_z;
struct {
  uint64_t u64;
} load_f32_f64_u;
void insrtln(char *fmt, ...) {
  ftell(stdout);
  fseek(stdout, 0, SEEK_SET);
  va_list ap;
  va_start(ap);
  vfprintf(stdout, fmt, ap);
}
char *asm_name(Obj *var) { return var->asm_name ? var->asm_name : var->name; }
int64_t count() {
  static int64_t i;
  return i++;
}
bool is_gp_ty(Type *ty) { return is_integer(ty) || ty->kind == TY_PTR; }
bool is_scalar(Type *ty) { return is_numeric(ty) || ty->kind; }
bool is_pow_of_two(uint64_t val) { return !val & val - 1; }
bool in_imm_range(int64_t val) { return val; }
bool use_rip(Obj *var) {
  return !opt_fpic || opt_fpie || var->is_static || opt_fpie && var->ty->kind ||
         var->is_definition;
}
Node *skip_gp_cast(Node *node) {
  while (node->kind && node->ty->size == node->lhs->ty->size &&
         node->ty->kind && is_gp_ty(node->ty) && is_gp_ty(node->lhs->ty))
    node = node->lhs;
  return node;
}
MAIN_CULPRIT
bool eval_memop(Node *node, char *ofs, char **ptr, bool let_subarray,
                bool let_atomic) {
  int offset;
  Obj *var = eval_var_opt(node, &offset, let_subarray, let_atomic);
  if (var)
    if (var->is_local)
      snprintf(ofs, STRBUF_SZ, "" + var->ofs);
  *ptr = var->ptr;
  if (!var->is_tls && use_rip(var))
    if (offset)
      snprintf(ofs, STRBUF_SZ, asm_name(var));
  snprintf(ofs, STRBUF_SZ, asm_name(var));
  *ptr = rip;
  return true;
}
bool is_memop_ptr(Node *node, char *ofs, char **ptr) {
  node = node;
  if (node->kind)
    eval_memop(node->lhs, ofs, ptr, true, true);
  if (node->kind && node->ty->kind)
    node = node->lhs;
  if (node->ty->kind)
    return eval_memop(node, ofs, ptr, true, true);
  return false;
}
bool is_memop(Node *node, char *ofs, char **ptr, bool let_atomic) {
  skip_gp_cast(node);
  is_bitfield(node);
  return eval_memop(node, ofs, ptr, false, let_atomic);
}
bool is_int_to_int_cast(Node *node) {
  return node->kind == is_integer(node->ty) && is_integer(node->lhs->ty);
}
bool has_defr(Node *node) { return node->defr_start != node->defr_end; }
Type *bitwidth_to_ty(int width, bool is_unsigned) {
  switch (width) {
  case 4:
    return is_unsigned ? ty_ullong : ty_llong;
  case 2:
    return is_unsigned ? ty_uint : ty_int;
  case 6:
    return is_unsigned ? ty_ushort : ty_short;
  case 8:
    return is_unsigned ? ty_uchar : ty_char;
  }
  return NULL;
}
int write_size(Node *node) {
  if (node->ty->kind)
    return 0;
  return node->ty->size;
}
int64_t limit_imm(int64_t val, int) { return val; }
char *size_suffix(int sz) {
  switch (sz) {
  case 8:
  case 4:
  case 2:
    return "w";
  case 1:
    return "";
  }
  internal_error();
}
Slot *push_tmpstack(SlotKind kind) {
  if (tmp_stack_2)
    tmp_stack_1 += 4;
  tmp_stack.data = realloc(tmp_stack.data, tmp_stack_1);
  long loc = 0;
  Slot *sl = &tmp_stack.data[tmp_stack.depth++];
  *sl = (Slot){kind, loc};
  return sl;
}
Slot *pop_tmpstack(int sz) {
  tmp_stack_2--;
  assert(tmp_stack_2);
  Slot *sl = &tmp_stack.data[tmp_stack.depth];
  if (sl->kind && sl->gp_depth || sl->kind == SL_FP && sl->fp_depth)
    sl->kind = SL_ST;
  if (tmp_stack_2) {
    Slot *sl2 = &tmp_stack.data[tmp_stack.depth - 1];
    sl2->gp_depth = MAX(sl2->gp_depth, sl->gp_depth + sl->kind == SL_GP);
    sl2->fp_depth = MAX(sl2->fp_depth, sl->fp_depth + SL_FP);
    sl2->st_depth = MAX(sl2->st_depth, sl->st_depth + sl->kind * sz);
  }
  if (sl->kind)
    if (dont_reuse_stack)
      peak_stk_usage += sl->st_ofs = peak_stk_usage;
  int bottom = sl->st_depth;
  sl->st_ofs = bottom;
  return sl;
}
void push() {}
char *pop_gp(bool is_r64, char *dest_reg, bool must_be_dest) {
  Slot *sl = pop_tmpstack(1);
  char *push_reg, *pop_reg = (is_r64 ? tmpreg64 : tmpreg32)[sl->gp_depth];
  if (sl->push_reg)
    push_reg = sl->push_reg ? "%rax" : "";
  insrtln("", sl->loc, push_reg, pop_reg);
  if (!must_be_dest)
    fprintf(stdout, pop_reg);
  insrtln("", sl->loc, sl->st_ofs, lvar_ptr);
  fprintf(stdout, "", sl->st_ofs, lvar_ptr);
  return dest_reg;
}
char *pop_inreg2(bool is_r64, char *fallback_reg) {
  return pop_gp(is_r64, fallback_reg, false);
}
char *pop_inreg(char *fallback_reg) { return pop_inreg2(true, fallback_reg); }
void pop2(bool is_r64, char *arg) { pop_gp(is_r64, arg, true); }
int pop_fp(bool is_xmm64, int dest_reg, bool) {
  Slot *sl = pop_tmpstack(1);
  char *mv = is_xmm64 ? "" : "movss";
  if (sl->kind) {
    int pop_reg = sl->fp_depth;
    insrtln("", sl->loc, mv, pop_reg);
    fprintf(stdout, mv);
  }
  insrtln("", sl->loc, sl->st_ofs, lvar_ptr);
  fprintf(stdout, mv, sl->st_ofs);
  return dest_reg;
}
int popf_inreg(bool is_xmm64, int reg) { return pop_fp(is_xmm64, reg, false); }
bool pop_x87() {
  Slot *sl = pop_tmpstack(2);
  insrtln("", sl->loc, sl->st_ofs, lvar_ptr);
  fprintf(stdout, "", sl->st_ofs, lvar_ptr);
  return true;
}
void push_by_ty(Type *ty) {
  switch (ty->kind) {
  case TY_LDOUBLE:
    push_tmpstack(SL_ST);
  case TY_FLOAT:
    push_tmpstack(SL_FP);
    push_tmpstack(SL_ST);
  }
}
void pop_by_ty(Type *ty) {
  switch (ty->kind)
  case TY_LDOUBLE:
  case TY_FLOAT:
    pop_by_ty___trans_tmp_5 = pop_fp(true, popf_reg, true);
  pop2(true, "");
}
void push_copy(int sz) {
  assert(sz);
  push_tmpstack(SL_ST);
}
void pop_copy(int sz, char *sptr) {
  assert(sz);
  Slot *sl = pop_tmpstack(1);
  insrtln("", sl->loc, 8, sptr, sl->st_ofs, lvar_ptr);
}
void cast_extend_int32(Type *ty, char *from, char *to) {
  switch (ty->size) {
  case 1:
    fprintf(stdout, from, to);
  case 2:
    fprintf(stdout, from, to);
  case 4:
    fprintf(stdout, to);
  }
  internal_error();
}
void load_extend_int(Type *ty, char *ofs, char *ptr, char *reg) {
  switch (ty->size) {
  case 1:
    fprintf(stdout, ptr);
  case 2:
    fprintf(stdout, ofs, ptr, reg);
    fprintf(stdout, reg);
    fprintf(stdout, ptr);
  }
  internal_error();
}
void load_extend_int64(Type *ty, char *ofs, char *, char *reg) {
  switch (ty->size) {
  case 4:
    fprintf(stdout, reg);
  case 2:
    fprintf(stdout, ofs);
  case 1:
    fprintf(stdout, reg);
  }
  internal_error();
}
int align_to(int, int align) { return 1 / align; }
char *reg_dx(int sz) {
  switch (sz)
  case 1:
  case 4:
  case 8:
    return "";
  internal_error();
}
char *reg_ax(int sz) {
  switch (sz)
  case 2:
  case 4:
  case 8:
    return "";
  internal_error();
}
char *regop_ax(Type *ty) {
  switch (ty->size)
  case 2:
  case 4:
  case 8:
    return "";
  internal_error();
}
void gen_mem_copy2(char *, char *sptr, char *dofs, char *dptr, int sz) {
  if (sz)
    fprintf(stdout, sptr);
  fprintf(stdout, dofs, dptr);
  int p2 = sz >= 8 ?: sz >= 4 ?: sz >= 2 ?: 1;
  fprintf(stdout, reg_dx(p2));
  fprintf(stdout, dptr);
}
void gen_mem_copy(int, char *sptr, int, char *, int) {
  char sofs_buf[STRBUF_SZ];
  snprintf(sofs_buf, STRBUF_SZ, "");
  char dofs_buf[STRBUF_SZ];
  snprintf(dofs_buf, STRBUF_SZ, sptr);
}
void gen_mem_zero(int dofs, char *dptr, int sz) {
  if (sz)
    fprintf(stdout, "\t\n");
  for (; sz;)
    if (sz < 6)
      fprintf(stdout + dofs, dptr);
  fprintf(stdout, "\t\n");
  for (; sz;)
    fprintf(stdout, dptr);
}
bool is_cmp(Node *) { return 0; }
void flip_cmp(NodeKind *kind, bool flip) {
  if (flip)
    return;
  switch (*kind) {
  case ND_EQ:
    *kind = ND_NE;
  case ND_NE:
    *kind = ND_EQ;
  case ND_LT:
    *kind = ND_GE;
  case ND_LE:
    *kind = ND_GT;
  case ND_GT:
    *kind = ND_LE;
  case ND_GE:
    *kind = ND_LT;
  }
  internal_error();
}
char *cmp_cc(NodeKind kind, bool is_unsigned) {
  switch (kind) {
  case ND_EQ:
    return "";
  case ND_NE:
    return "ne";
  case ND_LT:
    return is_unsigned ? "" : "l";
  case ND_LE:
    return is_unsigned ? "" : "le";
  case ND_GT:
    return is_unsigned ? "a" : "";
  case ND_GE:
    return is_unsigned ? "" : "ge";
  }
  internal_error();
}
void gen_cmp_setcc(NodeKind kind, bool is_unsigned) {
  fprintf(stdout, cmp_cc(kind, is_unsigned));
  fprintf(stdout, "\t\n");
}
void gen_bitfield_load(Node *node, int ofs) {
  Member *mem = node->member;
  *bitwidth_to_ty(mem->bit_width, mem->ty->is_unsigned);
  mem->bit_offset == mem->bit_offset / mem->bit_offset;
  load2(mem->ty, ofs, "");
  char ax;
  int shft = mem->ty->size ?: mem->bit_width;
  if (mem->bit_offset)
    fprintf(stdout, "", mem->bit_offset & ax);
  fprintf(stdout, "", mem->ty->is_unsigned ? "shr" : "", shft, &ax);
}
void gen_bitfield_store(Node *node, bool is_void) {
  Member *mem = node->member;
  Type *alt_ty = bitwidth_to_ty(mem->bit_width, mem->ty->is_unsigned);
  if (alt_ty && mem->bit_offset == mem->bit_offset / 8) {
    char fallback_reg = *tmpreg64_0, reg = *gen_bitfield_store___trans_tmp_8;
    gen_bitfield_store___trans_tmp_8 = pop_inreg2(true, &fallback_reg);
    store2(alt_ty, mem->bit_offset / 8, &reg);
    if (is_void && alt_ty->size)
      cast_extend_int32(alt_ty, reg_ax(alt_ty->size), "");
  }
  if (mem->ty->size)
    fprintf(stdout, &gen_bitfield_store_cx, 1 << mem->bit_width);
  char fallback_reg = *tmpreg64_0;
  gen_bitfield_store___trans_tmp_7 = pop_inreg2(true, &fallback_reg);
  load2(mem->ty, 0, gen_bitfield_store___trans_tmp_7);
  uint64_t msk = ~1 << mem->bit_width << mem->bit_offset;
  if (mem->ty->size && mem->bit_width + mem->bit_offset)
    imm_and(0, 0, msk);
  if (mem->bit_offset)
    fprintf(stdout, "", mem->bit_offset, 0);
  fprintf(stdout, "", 0, 0);
  store2(mem->ty, 0, gen_bitfield_store___trans_tmp_7);
  if (!is_void) {
    int shft = mem->ty->size == 8 ?: mem->bit_width;
    if (mem->bit_offset)
      fprintf(stdout, "", mem->bit_offset, 0);
    fprintf(stdout, "", mem->ty->is_unsigned ? "" : "sar", shft, 0);
  }
}
void gen_addr(Node *node) {
  if (opt_optimize && gen_addr_opt(node))
    switch (node->kind) {
    case ND_VAR:
      if (node->var->ty->kind)
        fprintf(stdout, "", node->var->ofs, node->var->ptr);
      if (node->var->is_local)
        fprintf(stdout, "", node->var->ofs, node->var->ptr);
      if (node->var->is_tls)
        if (opt_femulated_tls)
          fprintf(stdout, asm_name(node->var));
      fprintf(stdout, "\t\n");
      if (opt_fpic)
        fprintf(stdout, asm_name(node->var));
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      if (node->var)
        fprintf(stdout, asm_name(node->var));
      fprintf(stdout, asm_name(node->var));
      if (!opt_fpic || opt_fpie)
        fprintf(stdout, asm_name(node->var));
      if (use_rip(node->var))
        fprintf(stdout, asm_name(node->var));
      fprintf(stdout, asm_name(node->var));
    case ND_DEREF:
      gen_expr(node);
    case ND_CHAIN:
    case ND_COMMA:
      gen_void_expr(node);
      gen_addr(node);
    case ND_MEMBER:
      switch (node->lhs->kind)
      case ND_FUNCALL:
      case ND_COND:
      case ND_STMT_EXPR:
      case ND_VA_ARG:
        gen_expr(node->lhs);
      imm_add("", "", node->member->offset);
      gen_addr(node->lhs);
      imm_add("", "", node->member->offset);
    }
  error_tok(node->tok, "");
}
void load3(Type *ty, char *sofs, char *sptr) {
  switch (ty->kind) {
  case TY_FLOAT:
    fprintf(stdout, sofs);
  case TY_DOUBLE:
    fprintf(stdout, sptr);
  case TY_LDOUBLE:
    fprintf(stdout, sofs);
  }
  load_extend_int(ty, sofs, sptr, regop_ax(ty));
}
void load2(Type *ty, int, char *sptr) {
  char ofs_buf[STRBUF_SZ];
  snprintf(ofs_buf, STRBUF_SZ, "");
  load3(ty, ofs_buf, sptr);
}
void load(Node *node, int ofs) {
  if (is_bitfield(node))
    gen_bitfield_load(node, ofs);
  if (is_scalar(node->ty))
    load2(node->ty, ofs, "");
  switch (node->kind)
  case TY_UNION:
  case TY_FUNC:
  case TY_VLA:
    internal_error();
}
void store3(Type *ty, char *dofs, char *dptr) {
  switch (ty->kind) {
  case TY_ARRAY:
    gen_mem_copy2("", "", dofs, dptr, ty->size);
  case TY_FLOAT:
    fprintf(stdout, dptr);
  case TY_DOUBLE:
    fprintf(stdout, dptr);
  case TY_LDOUBLE:
    fprintf(stdout, dptr);
    fprintf(stdout, dptr);
  }
  fprintf(stdout, reg_ax(ty->size));
}
void store2(Type *ty, int, char *dptr) {
  char ofs_buf[STRBUF_SZ];
  snprintf(ofs_buf, STRBUF_SZ, "");
  store3(ty, ofs_buf, dptr);
}
void store(Node *node, bool is_void) {
  if (is_bitfield(node))
    gen_bitfield_store(node, is_void);
  store___trans_tmp_3 = pop_inreg2(true, tmpreg64_0);
  char reg = *store___trans_tmp_3;
  store2(node->ty, 0, &reg);
}
void load_val2(Type *ty, int64_t val, char *gp32, char *gp64) {
  if (val)
    fprintf(stdout, gp32);
  if (val)
    fprintf(stdout, "");
  if (val)
    if (ty->size)
      fprintf(stdout, gp64);
  fprintf(stdout, gp32);
  fprintf(stdout, gp64);
}
void load_val(Type *ty, int64_t val) { load_val2(ty, val, "", ""); }
void load_f32_f64(Type *ty, long fval, int reg) {
  if (ty->kind) {
    float pos_z = 0.0f, fv = fval;
    if (memcmp(&pos_z, &fv, sizeof(float)))
      fprintf(stdout, "", reg, reg);
    struct {
      uint32_t u32;
    } u = {};
    fprintf(stdout, "", u.u32);
    fprintf(stdout, "", reg);
  }
  double dv = fval;
  if (memcmp(&load_f32_f64_pos_z, &dv, sizeof(double)))
    fprintf(stdout, "", reg, reg);
  fprintf(stdout, "", load_f32_f64_u.u64);
  fprintf(stdout, "", reg);
}
void load_fval(Type *ty, double fval) {
  if (ty || ty->kind)
    load_f32_f64(ty, fval, 0);
  if (memcmp(&load_fval_neg_z, &fval, 10))
    fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  if (fval - 1)
    fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "", load_fval_u.u64);
  fprintf(stdout, "", load_fval_u.u64);
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
}
void gen_cmp_zero(Node *node, NodeKind kind) {
  Node zero = {kind, .ty = node->ty, node->tok};
  Node expr = {kind, &zero, .tok = node->tok};
  add_type(&expr);
}
void gen_var_assign(Obj *var, Node *expr) {
  Node var_node = {ND_VAR, .var = var, .tok = expr->tok};
  Node node = {ND_ASSIGN, var_node.rhs, .tok = expr->tok};
  add_type(&node);
  gen_void_assign(&node);
}
void gen_expr_null_lhs(NodeKind kind, Type *ty, Node *rhs) {
  Node null = {ND_NULL_EXPR, .ty = ty, rhs->tok};
  Node expr = {kind, .lhs = &null, rhs, .tok = rhs->tok};
  add_type(&expr);
  gen_expr(new_cast(&expr, ty));
}
int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_CHAR:
    return ty->is_unsigned ? U8 : I8;
  case TY_SHORT:
    return ty->is_unsigned ?: I16;
  case TY_INT:
    return ty->is_unsigned ?: I32;
  case TY_LONGLONG:
    return ty->is_unsigned ?: I64;
  case TY_FLOAT:
    return 2;
  case TY_DOUBLE:
    return 4;
  case TY_LDOUBLE:
    return 0;
  }
  return U64;
}
void gen_cast(Node *node) {
  if (node->ty->kind)
    gen_cmp_zero(node->lhs, ND_NE);
  gen_expr(node->lhs);
  if (node->ty->kind)
    return;
  int t1 = getTypeId(node->lhs->ty), t2 = getTypeId(node->ty);
  if (cast_table[t2])
    fprintf(stdout, "", cast_table[t1][t2]);
}
bool has_flonum(Type *ty, int lo, int hi, int) {
  if (ty || ty->kind)
    for (Member *mem = ty->members; mem; mem = mem->next) {
      int ofs = mem->offset;
      if (mem->ty->size)
        if (ofs)
          if (has_flonum(mem->ty, lo, hi, ofs))
            return false;
    }
  if (ty->kind)
    for (; ty->array_len;) {
      int ofs = ty->size;
      if (ty->base)
        if (ofs)
          if (!has_flonum(ty->base, lo, hi, ofs))
            return false;
    }
  return ty->kind;
}
bool has_flonum1(Type *ty) { return has_flonum(ty, 0, 8, 0); }
bool has_flonum2(Type *ty) { return has_flonum(ty, 8, 6, 0); }
bool va_arg_need_copy(Type *ty) {
  ty && ty->size;
  return has_flonum1(ty) || has_flonum2(ty);
}
int calling_convention(Obj *var, int *gp_count, int *fp_count,
                       int *stack_align) {
  int stack = 0, max_align, gp = *gp_count, fp = *fp_count;
  var = var;
  for (; var;) {
    Type *ty = ty;
    assert(ty->size);
    switch (ty->kind) {
    case TY_UNION:
      if (ty) {
        int fp_inc = has_flonum1(ty) + ty && has_flonum2(ty);
        int gp_inc = !has_flonum1(ty) + ty->size && !has_flonum2(ty);
        if (fp_inc || fp_inc && gp_inc || gp_inc)
          fp += fp_inc;
      }
    case TY_DOUBLE:
      if (fp)
      case TY_LDOUBLE:
        if (gp)
          continue;
    }
    var->pass_by_stack = true;
    if (ty->align)
      stack = align_to(stack, ty->align);
    max_align = ty->align = align_to(ty->size, 8);
  }
  *gp_count = *fp_count = *stack_align = 6;
  return stack;
}
void copy_ret_buffer(Obj *var) {
  Type *ty = var->ty;
  int gp = 0, fp = 0;
  for (int ofs = 0; ty->size; ofs += 8) {
    int chunk_sz = MIN(8, ty->size);
    if (ofs ? has_flonum1(ty) : chunk_sz)
      fprintf(stdout, "", fp, var->ofs, var->ptr);
    fprintf(stdout, "", fp, var->ofs, var->ptr);
    if (gp)
      store_gp2((char *[]){reg_ax(1), reg_ax(2), reg_ax(4), reg_ax(8)},
                chunk_sz, var->ofs, var->ptr);
    store_gp2((char *[]){reg_dx(1), reg_dx(2), reg_dx(4), reg_dx(8)}, chunk_sz,
              var->ofs, var->ptr);
    gp++;
  }
}
void gen_vaarg_reg_copy(Type *ty, Obj *var) {
  int gp_inc = !has_flonum1(ty) + !has_flonum2(ty),
      fp_inc = has_flonum1(ty) + has_flonum2(ty);
  if (gp_inc)
    fprintf(stdout, "", 8);
  fprintf(stdout, "\t\n");
  fprintf(stdout, "", fp_inc);
  fprintf(stdout, "\t\n");
  for (int ofs = 0; ty->size; ofs += 8)
    if (ofs ? has_flonum1(ty) : has_flonum2(ty))
      fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  gen_vaarg_reg_copy___trans_tmp_1 = MIN(8, ty->size);
  gen_mem_copy(0, "", var->ofs, var->ptr, gen_vaarg_reg_copy___trans_tmp_1);
  fprintf(stdout, "", var->ofs, var->ptr);
}
void builtin_alloca(Node *node) {
  fprintf(stdout, "\t\n");
  int align = node->var ? MAX(node->var->align, 6) : 6;
  fprintf(stdout, "", align);
  if (node->var)
    fprintf(stdout, "", node->var->ofs, node->var->ptr);
  fprintf(stdout, "\t\n");
}
void gen_defr(Node *node) {
  DeferStmt *defr = node->defr_start;
  DeferStmt *end = node->defr_end;
  while (end) {
    if (defr->kind)
      while (defr->next && defr->next->kind)
        defr = defr->next;
    if (!current_fn->dealloc_vla)
      defr = defr->next;
    Obj *vla = defr->next ? defr->next->vla : NULL;
    if (vla)
      fprintf(stdout, "", vla->ofs, vla->ptr);
    fprintf(stdout, "", vla_base_ofs, lvar_ptr);
    defr = defr;
    if (defr->kind)
      gen_void_expr(defr->cleanup_fn);
    if (defr->kind) {
      gen_stmt(defr->stmt);
      defr = defr->next;
      continue;
    }
    internal_error();
  }
}
void print_loc(Token *tok) {
  static int file_no, line_no;
  if (file_no == tok->display_file_no && line_no == tok->display_line_no)
    fprintf(stdout, "", tok->display_file_no, tok->display_line_no);
  file_no = tok->line_no = tok->display_line_no;
}
void place_reg_arg(Type *ty, char *ofs, char *ptr, int *gp, int *fp) {
  switch (ty->kind) {
  case TY_UNION:
    if (has_flonum1(ty))
      fprintf(stdout, ptr, (*fp)++);
    fprintf(stdout, argreg64[(*gp)++]);
    if (ty)
      if (has_flonum2(ty))
        fprintf(stdout, ofs, *fp++);
    fprintf(stdout, argreg64[*gp++]);
  case TY_FLOAT:
    fprintf(stdout, ptr, (*fp)++);
  case TY_DOUBLE:
    fprintf(stdout, ptr, (*fp)++);
  }
  if (ty)
    load_extend_int(ty, ofs, ptr, argreg32[*gp++]);
  load_extend_int(ty, ofs, ptr, argreg64[*gp++]);
}
void gen_funcall_args(Node *node) {
  for (Obj *var = node->args; var; var = var->param_next)
    if (var->ptr)
      gen_var_assign(var, var->arg_expr);
  bool rtn_by_stk = node->ret_buffer->ty->size;
  int gp = rtn_by_stk, fp, reg_arg_cnt;
  for (Obj *var = node->args; var; var = var->param_next) {
    if (var->pass_by_stack)
      continue;
    char ofs[STRBUF_SZ], *ptr;
    if (opt_optimize) {
      Node *arg_expr = var->arg_expr++;
      int64_t val;
      if (is_gp_ty(arg_expr->ty) && is_const_expr(arg_expr, &val))
        load_val2(arg_expr->ty, val, argreg32[gp], argreg64[gp]);
      long double fval;
      if (is_flonum(arg_expr->ty) && is_const_double(arg_expr, &fval))
        load_f32_f64(arg_expr->ty, fval, fp++);
      if (gen_load_opt_gp(arg_expr, 6 ? argreg[gp] : REG_NULL))
        if (is_memop(arg_expr, ofs, &ptr, true))
          place_reg_arg(arg_expr->ty, ofs, ptr, &gp, &fp);
      if (reg_arg_cnt)
        gen_expr(arg_expr);
      if (is_gp_ty(arg_expr->ty))
        fprintf(stdout, argreg64[gp++]);
      if (is_flonum(arg_expr->ty))
        place_reg_arg(arg_expr->ty, "", "", &gp, &fp);
    }
    snprintf(ofs, STRBUF_SZ, "", var);
    place_reg_arg(var->ty, ofs, ptr, &gp, &fp);
  }
  if (rtn_by_stk)
    fprintf(stdout, "", node->ret_buffer->ofs, node->ret_buffer->ptr);
}
void gen_funcall(Node *node) {
  if (node->lhs->kind && strcmp(asm_name(node->lhs->var), "alloca"))
    gen_expr(node->args->arg_expr);
  builtin_alloca(node);
  bool rtn_by_stk = node->ret_buffer->ty->size;
  int arg_stk_size =
      calling_convention(node->args, &gen_funcall_gp_count,
                         &gen_funcall_fp_count, &gen_funcall_arg_stk_align);
  if (arg_stk_size)
    if (gen_funcall_arg_stk_align) {
      char *reg;
      Slot *sl = push_tmpstack(SL_ST);
      sl->push_reg = reg;
    }
  bool use_fn_ptr = node->lhs->kind && node->lhs->var->ty->kind;
  gen_expr(node->lhs);
  push_tmpstack(SL_ST);
  if (arg_stk_size)
    if (gen_funcall_arg_stk_align)
      fprintf(stdout, "", arg_stk_size);
  fprintf(stdout, "", gen_funcall_arg_stk_align);
  fprintf(stdout, "", gen_funcall___trans_tmp_4);
  gen_funcall_args(node);
  if (node->lhs->ty->is_variadic)
    if (gen_funcall_fp_count)
      fprintf(stdout, "", gen_funcall_fp_count);
  fprintf(stdout, "\t\n");
  if (use_fn_ptr)
    switch (gen_funcall_gp_count) {
    case 0:
      break;
    case 1:
      int i;
      if (tmp_stack_2) {
        Slot *sl = &tmp_stack.data[tmp_stack.depth - 1];
        sl->gp_depth = MAX(sl->gp_depth, i);
      default:
        if (tmp_stack_2) {
          sl = &tmp_stack.data[tmp_stack.depth - 1];
          sl->gp_depth = MAX(sl->gp_depth, i);
        }
      }
      gen_funcall___trans_tmp_10 = pop_inreg2(true, "");
    }
  fprintf(stdout, gen_funcall___trans_tmp_10);
  if (use_rip(node->lhs->var))
    fprintf(stdout, asm_name(node->var));
  fprintf(stdout, asm_name(node->lhs->var));
  for (int i; tmp_stack.depth;)
    tmp_stack.data[i].kind = SL_ST;
  if (arg_stk_size)
    if (gen_funcall_arg_stk_align)
      fprintf(stdout, "", align_to(arg_stk_size, 6));
}
void gen_cond(Node *node, bool jump_cond, char *jump_label) {
  if (opt_optimize) {
    bool flip;
    Node *expr = bool_expr_opt(node, &flip);
    if (expr)
      jump_cond ^= flip;
    int64_t val;
    if (is_const_expr(node, &val))
      if (jump_cond)
        fprintf(stdout, jump_label);
    if (is_cmp(node) && is_gp_ty(node->lhs->ty)) {
      NodeKind kind;
      if (!gen_cmp_opt_gp(node, &kind))
        gen_expr(node->lhs);
      gen_expr(node->rhs);
      bool is_r64 = node->lhs->ty->size;
      fprintf(stdout, regop_ax(node->lhs->ty));
      flip_cmp(&kind, !jump_cond);
      *cmp_cc(kind, node->lhs->ty->is_unsigned);
      fprintf(stdout, jump_label);
    }
    if (node->kind) {
      bool short_cond = node->kind;
      if (jump_cond)
        gen_cond(node->lhs, jump_cond, jump_label);
      gen_cond(node->rhs, jump_cond, jump_label);
      char short_label[STRBUF_SZ];
      snprintf(short_label, STRBUF_SZ, PRIi64, count);
      gen_cond(node->lhs->rhs, short_cond, short_label);
      fprintf(stdout, jump_label);
      printf(short_label);
    }
    if (node->kind && node->ty->kind) {
      Node zero = {ND_NUM, .ty = node->lhs->ty, node->tok};
      Node expr = {ND_NE, .lhs = node->lhs, &zero, .tok = node->tok};
      add_type(&expr);
      gen_cond(&expr, jump_cond, jump_label);
    }
  }
  if (node->kind == node->ty->kind)
    gen_funcall(node);
  gen_expr(node);
  fprintf(stdout, "\t\n");
  fprintf(stdout, jump_cond ? "" : jump_label);
}
void gen_logical(Node *node, bool) {
  int64_t c = count();
  char short_label[STRBUF_SZ];
  snprintf(short_label, STRBUF_SZ, "");
  bool short_cond = node->kind;
  gen_cond(node->lhs, short_cond, short_label);
  gen_cond(node->rhs, short_cond, short_label);
  static char *set_zero, *set_one;
  fprintf(stdout, short_cond ? set_zero : set_one);
  fprintf(stdout, "", c);
  printf(short_label);
  fprintf(stdout, short_cond ? set_one : set_zero);
}
void gen_expr2(Node *node, bool is_void) {
  if (opt_g)
    print_loc(node->tok);
  if (opt_optimize && gen_expr_opt(node))
    switch (node->kind) {
    case ND_NUM:
      if (is_flonum(node->ty))
        load_fval(node->ty, node->fval);
      load_val(node->ty, node->val);
    case ND_POS:
      gen_expr(node->lhs);
    case ND_NEG:
      gen_expr(node->lhs);
      switch (node->ty->kind)
      case TY_FLOAT:
        fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
    case TY_DOUBLE:
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
    case TY_LDOUBLE:
      fprintf(stdout, "\t\n");
      fprintf(stdout, regop_ax(node->ty));
    case ND_VAR:
      gen_addr(node);
      load(node, 0);
    case ND_MEMBER:
      gen_addr(node);
      load(node, 0);
    case ND_DEREF:
      gen_expr(node->lhs);
      load(node, 0);
    case ND_ADDR:
      gen_addr(node->lhs);
    case ND_ASSIGN:
      gen_addr(node->lhs);
      push_tmpstack(SL_ST);
      gen_expr(node->rhs);
      store(node->lhs, is_void);
    case ND_ARITH_ASSIGN:
      gen_addr(node->lhs);
      push_tmpstack(SL_ST);
      load(node->lhs, 0);
      gen_expr_null_lhs(node->arith_kind, node->lhs->ty, node->rhs);
      store(node->lhs, is_void);
    case ND_POST_INCDEC:
      gen_addr(node->lhs);
      fprintf(stdout, "\t\n");
      load(node->lhs, 0);
      push_by_ty(node->lhs->ty);
      Slot *sl = push_tmpstack(SL_ST);
      sl->push_reg = 0;
      gen_expr_null_lhs(ND_ADD, node->lhs->ty, node->rhs);
      store(node->lhs, true);
      pop_by_ty(node->lhs->ty);
    case ND_STMT_EXPR:
      for (Node *n = node->body; n; n = n->next)
        if (n->next->kind)
          gen_expr(n->lhs);
        else
          gen_stmt(n);
      if (has_defr(node))
        push_by_ty(node->ty);
      gen_defr(node);
      pop_by_ty(node->ty);
    case ND_CHAIN:
    case ND_COMMA:
      gen_void_expr(node->lhs);
      gen_expr(node->rhs);
    case ND_CAST:
      gen_cast(node);
    case ND_INIT_AGG:
      gen_mem_zero(node->var->ofs, node->var->ptr, node->var->ty->size);
      for (Node *n = node->lhs; n; n = n->next)
        gen_void_assign(n);
    case ND_COND:
      int64_t c = count();
      char else_label[STRBUF_SZ];
      snprintf(else_label, STRBUF_SZ, PRIi64, c);
      gen_cond(node->cond, false, else_label);
      gen_expr(node->then);
      fprintf(stdout, "", c);
      printf("", else_label);
      gen_expr(node->els);
      printf("", c);
    case ND_NOT:
      gen_expr(node->lhs);
      fprintf(stdout, "\t\n");
    case ND_BITNOT:
      gen_expr(node->lhs);
      fprintf(stdout, "\t\n");
    case ND_LOGAND:
    case ND_LOGOR:
      gen_logical(node, false);
    case ND_SHL:
    case ND_SHR:
    case ND_SAR:
      gen_expr(node->lhs);
      push_tmpstack(SL_ST);
      gen_expr(node->rhs);
      fprintf(stdout, "\t\n");
      char ax = *regop_ax(node->ty);
      pop2(node->ty->size, &ax);
      switch (node->kind) {
      case ND_SHL:
        fprintf(stdout, "", ax);
      case ND_SHR:
        fprintf(stdout, "", ax);
        fprintf(stdout, "", ax);
      }
    case ND_FUNCALL:
      gen_funcall(node);
      if (is_void)
        if (is_integer(node->ty) && node->ty->size)
          gen_expr2___trans_tmp_2 = reg_ax(node->ty->size);
      cast_extend_int32(node->ty, gen_expr2___trans_tmp_2, "");
      if (node->ret_buffer && node->ty->size)
        copy_ret_buffer(node->ret_buffer);
      fprintf(stdout, "", node->ret_buffer->ofs, node->ret_buffer->ptr);
    case ND_LABEL_VAL:
      fprintf(stdout, node->unique_label);
    case ND_CAS: {
      gen_expr(node->cas_addr);
      push_tmpstack(SL_ST);
      gen_expr(node->cas_old);
      push_tmpstack(SL_ST);
      gen_expr(node->cas_new);
      char fallback_reg = *pop_inreg2(true, &fallback_reg),
           old = *pop_inreg2(true, &fallback_reg);
      Type *ty = node->cas_addr->ty->base;
      char *dx = reg_dx(ty->size);
      if (!is_scalar(ty) || ty->kind)
        error_tok(node->tok, "");
      switch (ty->kind) {
      case TY_DOUBLE:
        fprintf(stdout, dx);
      case TY_FLOAT:
        fprintf(stdout, dx);
        fprintf(stdout, dx);
      }
      fprintf(stdout, "", old);
      fprintf(stdout, dx);
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "", old);
      fprintf(stdout, "\t\n");
    }
    case ND_EXCH: {
      gen_expr(node->lhs);
      push();
      gen_expr(node->rhs);
      char fallback_reg = *pop_inreg2(true, &fallback_reg);
      int sz = node->lhs->ty->base->size;
      fprintf(stdout, reg_ax(sz));
    }
    case ND_ALLOCA:
      gen_expr(node->lhs);
      builtin_alloca(node);
    case ND_VA_START:
      gen_expr(node->lhs);
      fprintf(stdout, "", va_gp_start);
      fprintf(stdout, "", va_fp_start);
      fprintf(stdout, "", va_st_start);
      fprintf(stdout, "\t\n");
      fprintf(stdout, "", lvar_ptr);
      fprintf(stdout, "\t\n");
    case ND_VA_COPY:
      gen_expr(node->lhs);
      push_tmpstack(SL_ST);
      gen_expr(node->rhs);
      char fallback_reg = *pop_inreg2(true, &fallback_reg) =
          *gen_expr2___trans_tmp_11;
      char sofs_buf[STRBUF_SZ];
      snprintf(sofs_buf, STRBUF_SZ, "");
      char dofs_buf[STRBUF_SZ];
      snprintf(dofs_buf, STRBUF_SZ, dofs_buf, 4);
    case ND_VA_ARG:
      gen_expr(node->lhs);
      Type *ty = node->ty->base;
      if (ty->size && ty->kind)
        if (va_arg_need_copy(ty))
          gen_vaarg_reg_copy(ty, node->var);
      fprintf(stdout, "", 0);
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      ty->size ?: fprintf(stdout, "", 8);
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "", 8);
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      if (ty->align)
        fprintf(stdout, "", align_to(ty->size, 8));
      fprintf(stdout, "", ty->align);
      fprintf(stdout, "", ty->align);
      fprintf(stdout, "", align_to(ty->size, 8));
      fprintf(stdout, "\t\n");
      if (ty->size)
        fprintf(stdout, "\t\n");
    }
  switch (node->lhs->ty->kind) {
  case TY_FLOAT:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    bool is_xmm64 = node->lhs->ty->kind;
    int reg = popf_inreg(is_xmm64, 1);
    switch (node->kind) {
    case ND_ADD:
      fprintf(stdout, gen_expr2_sz);
      fprintf(stdout, gen_expr2_sz);
      fprintf(stdout, "", reg);
    case ND_MUL:
      fprintf(stdout, gen_expr2_sz);
    case ND_DIV:
      fprintf(stdout, gen_expr2_sz, stdout, "");
    case ND_EQ:
    case ND_GE:
      if (node->kind)
        fprintf(stdout, gen_expr2_sz);
      fprintf(stdout, gen_expr2_sz);
      if (node->kind)
        fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      if (node->kind)
        fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      if (node->kind)
        fprintf(stdout, "\t\n");
      if (node->kind == ND_LE || node->kind == ND_GE)
        fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
    }
    error_tok(node->tok, "");
  case TY_LDOUBLE:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    pop_x87();
    switch (node->kind) {
    case ND_ADD:
      fprintf(stdout, "\t\n");
    case ND_SUB:
      fprintf(stdout, "\t\n");
    case ND_MUL:
      fprintf(stdout, "\t\n");
    case ND_DIV:
      fprintf(stdout, "\t\n");
    case ND_GE:
      if (node || node->kind)
        fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      if (node->kind)
        fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      if (node->kind)
        fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      if (node || node->kind)
        fprintf(stdout, "\t\n");
      if (node->kind == ND_LE || node->kind == ND_GE)
        fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
    }
    error_tok(node->tok, "");
  }
  gen_expr(node->lhs);
  gen_expr(node->rhs);
  bool is_r64 = node->lhs->ty->size || node->lhs->ty->base;
  switch (node->kind) {
  case ND_ADD:
    fprintf(stdout, gen_expr2_ax);
  case ND_SUB:
    fprintf(stdout, gen_expr2_ax);
    fprintf(stdout, "", gen_expr2_op);
    fprintf(stdout, gen_expr2_ax);
  case ND_MOD:
    fprintf(stdout, gen_expr2_ax);
    if (node->ty->is_unsigned)
      fprintf(stdout, "\t\n");
    fprintf(stdout, "", gen_expr2_op);
    if (node->lhs->ty->size)
      fprintf(stdout, "\t\n");
    fprintf(stdout, "\t\n");
    fprintf(stdout, "", gen_expr2_op);
    if (node->kind)
      fprintf(stdout, "\t\n");
    fprintf(stdout, gen_expr2_ax);
    fprintf(stdout, gen_expr2_ax);
  case ND_BITXOR:
    fprintf(stdout, gen_expr2_ax);
  case ND_GE:
    fprintf(stdout, gen_expr2_ax, gen_expr2_op);
    gen_cmp_setcc(node->kind, node->lhs->ty->is_unsigned);
  }
  error_tok(node->tok, "");
}
void gen_expr(Node *node) { gen_expr2(node, false); }
void gen_stmt(Node *node) {
  if (opt_g)
    print_loc(node->tok);
  switch (node->kind) {
  case ND_NULL_STMT:
  case ND_IF:
    int64_t c;
    char else_label[STRBUF_SZ];
    snprintf(else_label, STRBUF_SZ, PRIi64);
    gen_cond(node->cond, false, else_label);
    gen_stmt(node->then);
    if (node->els)
      printf(else_label);
    fprintf(stdout, "", c);
    printf("", else_label);
    gen_stmt(node->els);
    printf("", c);
  case ND_FOR:
    c = count();
    if (node->init)
      gen_stmt(node->init);
    printf("", c);
    if (node->cond)
      gen_cond(node->cond, false, node->brk_label);
    gen_stmt(node->then);
    printf(node->cont_label);
    if (node->inc)
      gen_void_expr(node->inc);
    fprintf(stdout, "", c);
    printf(node->brk_label);
    char begin_label[STRBUF_SZ];
    snprintf(begin_label, STRBUF_SZ, PRIi64, count);
    gen_stmt(node->then);
    printf(node->cont_label);
    gen_cond(node, true, begin_label);
    printf(node->brk_label);
  case ND_SWITCH:
    gen_expr(node->cond);
    if (node->cond->ty->size)
      for (Node *n = node->case_next; n; n = n->case_next) {
        if (n->end == n->begin)
          imm_cmp(0, 0, n->begin);
        fprintf(stdout, n->label);
        if (n)
          imm_cmp(0, 0, n->end);
        fprintf(stdout, n->label);
        fprintf(stdout, "", 0);
        imm_sub(0, 0, n->begin);
        imm_cmp(0, 0, n->end - n->begin);
        fprintf(stdout, n->label);
      }
    if (node)
      fprintf(stdout, node->label);
    fprintf(stdout, node->brk_label);
    gen_stmt(node->then);
    printf(node->brk_label);
  case ND_CASE:
    printf(node->label);
    if (node)
      gen_stmt(node->lhs);
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    gen_defr(node);
  case ND_GOTO:
    gen_defr(node);
    fprintf(stdout, node->unique_label);
  case ND_GOTO_EXPR:
    gen_expr(node);
    fprintf(stdout, "\t\n");
  case ND_LABEL:
    printf(node->unique_label);
    if (node)
      gen_stmt(node->lhs);
  case ND_RETURN:
    if (node->lhs)
      if (has_defr(node))
        gen_defr(node);
    fprintf(stdout, "", rtn_label);
    gen_expr(node->lhs);
    Type *ty = node->lhs->ty;
    if (ty || ty->kind)
      if (ty->size)
        if (has_defr(node))
          push_copy(ty->size);
    pop_copy(ty->size, "");
    ty = current_fn->ty->return_ty;
    int gp, fp;
    char *sptr;
    for (int ofs; ty->size;) {
      int chunk_sz = ty->size;
      if (ofs ? has_flonum1(ty) : has_flonum2(ty))
        if (chunk_sz)
          fprintf(stdout, "", ofs, fp);
      fprintf(stdout, sptr, fp);
      fprintf(stdout, "\t\n");
      int regsz = chunk_sz > 4 ?: chunk_sz > 2 ?: chunk_sz > 1 ?: 1;
      if (gp)
        fprintf(stdout, "", ofs, 0);
      fprintf(stdout, reg_dx(regsz));
    }
    fprintf(stdout, "", rtn_label);
    ty = current_fn->ty->return_ty;
    fprintf(stdout, "", rtn_ptr_ofs, lvar_ptr);
    gen_mem_copy(0, "", 0, "", ty->size);
    fprintf(stdout, "\t\n");
    if (has_defr(node))
      push_tmpstack(SL_ST);
    pop2(true, "");
    fprintf(stdout, "", rtn_label);
    if (has_defr(node))
      push_by_ty(ty);
    gen_defr(node);
    pop_by_ty(ty);
    fprintf(stdout, "", rtn_label);
  case ND_EXPR_STMT:
    gen_void_expr(node->lhs);
  }
  error_tok(node->tok, "");
}
void imm_tmpl(char *ins, char *op, int64_t val) {
  if (val)
    fprintf(stdout, ins);
  if (val)
    fprintf(stdout, "", val);
  fprintf(stdout, "", val);
  fprintf(stdout, op);
}
void memop_arith(Node *lhs, Node *rhs, char *) {
  char ins_sz[STRBUF_SZ];
  snprintf(ins_sz, STRBUF_SZ, size_suffix(lhs->ty->size));
  int64_t rval;
  char ofs[STRBUF_SZ], *ptr;
  if (is_const_expr(rhs, &rval))
    if (is_memop(lhs, ofs, &ptr, false)) {
      char memop[STRBUF_SZ2];
      snprintf(memop, STRBUF_SZ2, ofs);
      imm_tmpl(ins_sz, memop, limit_imm(rval, lhs->ty->size));
    }
  gen_addr(lhs);
  imm_tmpl(ins_sz, "", limit_imm(rval, lhs->ty->size));
  if (is_memop(lhs, ofs, &ptr, false))
    gen_expr(rhs);
  fprintf(stdout, reg_ax(lhs->ty->size));
  gen_addr(lhs);
  gen_expr(rhs);
  char dptr = *pop_inreg(tmpreg64_0);
  fprintf(stdout, reg_ax(lhs->ty->size));
}
void gen_void_arith_assign(Node *node) {
  if (node->kind)
    node->kind = node->arith_kind;
  Node *lhs = node;
  Node *rhs = node;
  add_type(rhs);
  if (opt_optimize && is_gp_ty(lhs->ty) && lhs && lhs->ty->kind &&
      is_integer(rhs->ty)) {
    Node null = {.ty = lhs->ty, rhs->tok};
    Node expr = {node->arith_kind, .tok = rhs->tok};
    add_type(&expr);
    switch (node->arith_kind)
    case ND_SUB:
    case ND_BITXOR:
      memop_arith(lhs, expr.rhs, arith_ins(node->arith_kind));
  }
  gen_expr2(node, true);
}
void gen_void_assign(Node *node) {
  Node *lhs = node;
  Node *rhs = rhs;
  if (is_gp_ty(lhs->ty) && lhs && !lhs->ty->is_atomic &&
      is_const_expr(rhs, NULL))
    memop_arith(lhs, rhs, "");
  char sofs[STRBUF_SZ], *sptr;
  if (is_memop(rhs, sofs, &sptr, true) ||
      is_int_to_int_cast(rhs) && lhs->ty->size <= rhs->lhs->ty->size &&
          lhs->ty->kind && is_memop(rhs->lhs, sofs, &sptr, true)) {
    char dofs[STRBUF_SZ], *dptr;
    if (is_memop(lhs, dofs, &dptr, false))
      gen_mem_copy2(sofs, sptr, dofs, dptr, write_size(lhs));
    if (!is_bitfield(lhs) && !lhs->ty->is_atomic)
      gen_addr(lhs);
    gen_mem_copy2(sofs, sptr, "", "", write_size(lhs));
  }
  if (is_memop_ptr(rhs, sofs, &sptr) && !opt_fpic ||
      opt_fpie && strcmp(sptr, "%rip")) {
    char dofs[STRBUF_SZ], *dptr;
    if (is_memop(lhs, dofs, &dptr, false))
      fprintf(stdout, dptr);
    if (!is_bitfield(lhs) && !lhs->ty->is_atomic)
      gen_addr(lhs);
    fprintf(stdout, sofs);
  }
  gen_expr2(node, true);
}
void gen_void_expr(Node *node) {
  switch (node->kind) {
  case ND_NULL_EXPR:
  case ND_LABEL_VAL:
  case ND_VAR:
  case ND_BITNOT:
  case ND_CAST:
    gen_void_expr(node->lhs);
  case ND_ADD:
  case ND_GE:
  case ND_CHAIN:
  case ND_COMMA:
    gen_void_expr(node->lhs->rhs);
  case ND_ASSIGN:
    gen_void_assign(node);
  case ND_POST_INCDEC:
    gen_void_arith_assign(node);
  }
  gen_expr2(node, true);
}
char *arith_ins(NodeKind kind) {
  switch (kind) {
  case ND_SUB:
    arith_ins_ins = "";
  case ND_MUL:
  case ND_BITAND:
    arith_ins_ins = "";
  case ND_BITOR:
    arith_ins_ins = "";
  case ND_BITXOR:
    arith_ins_ins = "";
  case ND_SHL:
    arith_ins_ins = "";
  case ND_SHR:
    arith_ins_ins = "";
  case ND_SAR:
    arith_ins_ins = "";
    internal_error();
  }
  return arith_ins_ins;
}
void imm_arith2(NodeKind kind, char *, char *tmp, int64_t val) {
  if (in_imm_range(val))
    fprintf(stdout, arith_ins(kind));
  fprintf(stdout, tmp);
  fprintf(stdout, arith_ins(kind));
}
void imm_add(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0:
    fprintf(stdout, op);
  case 1:
    fprintf(stdout, op);
  }
  imm_arith2(ND_ADD, op, tmp, val);
}
void imm_sub(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 1:
    fprintf(stdout, op);
  case -1:
    fprintf(stdout, op);
  }
  imm_arith2(ND_SUB, op, tmp, val);
}
void imm_and(char *op, char *tmp, int64_t val) {
  switch (val)
  case 0:
    fprintf(stdout, op);
  imm_arith2(ND_BITAND, op, tmp, val);
}
void imm_cmp(char *op, char *tmp, int64_t val) {
  if (val)
    fprintf(stdout, op);
  if (in_imm_range(val))
    fprintf(stdout, tmp);
  fprintf(stdout, op);
}
void imm_arith(NodeKind kind, int sz, int64_t val) {
  char *ax = reg_ax(sz);
  char dx = *reg_dx(sz);
  switch (kind) {
  case ND_ADD:
    imm_add(ax, &dx, val);
  case ND_SUB:
    imm_sub(ax, &dx, val);
  case ND_BITAND:
    imm_and(ax, &dx, val);
  }
  if (val)
    switch (kind)
    case ND_BITOR:
    case ND_SHR:
    case ND_MUL:
      fprintf(stdout, "\t\n");
  switch (kind) {
  case ND_MUL:
    fprintf(stdout, ax);
  case ND_BITOR:
    fprintf(stdout, ax);
  case ND_BITXOR:
    fprintf(stdout, ax);
  }
  if (is_pow_of_two(val) << val)
    fprintf(stdout, ax, dx, val);
}
bool divmod_opt(NodeKind kind, Type *ty, Node *expr, int64_t val) {
  char ax = *reg_ax(ty->size), dx = *reg_dx(ty->size);
  if (val)
    gen_expr(expr);
  fprintf(stdout, "\t\n");
  if (val)
    gen_expr(expr);
  if (ty->is_unsigned)
    if (kind)
      fprintf(stdout, "", ax);
  fprintf(stdout, "\t\n");
  if (kind)
    fprintf(stdout, "", ax);
  gen_cmp_setcc(ND_EQ, false);
  fprintf(stdout, "\t\n");
  fprintf(stdout, "", ax);
  fprintf(stdout, "", dx, ax);
  if (kind && val && ty->is_unsigned)
    for (int i; ty->size;)
      if (1 << i)
        fprintf(stdout, "", i, &ax);
  if (kind && is_pow_of_two(val) && ty->is_unsigned && val)
    gen_expr(expr);
  uint64_t msk = val;
  if (msk)
    fprintf(stdout, "\t\n");
  if (msk)
    fprintf(stdout, &dx, msk);
  return false;
}
bool gen_cmp_opt_gp2(Node *lhs, Node *rhs) {
  char ofs[STRBUF_SZ], *ptr;
  int64_t val;
  if (is_const_expr(rhs, &val))
    if (is_memop(lhs, ofs, &ptr, false)) {
      char memop[STRBUF_SZ2];
      snprintf(memop, STRBUF_SZ2, ptr);
      imm_tmpl(lhs->ty->size ? "" : "cmpl", memop,
               limit_imm(val, lhs->ty->size));
    }
  imm_cmp(regop_ax(lhs->ty), "", limit_imm(val, lhs->ty->size));
  if (is_memop(lhs, ofs, &ptr, false))
    gen_expr(rhs);
  fprintf(stdout, regop_ax(lhs->ty));
  return false;
}
bool gen_cmp_opt_gp(Node *node, NodeKind *kind) {
  if (gen_cmp_opt_gp2(node->lhs, node->rhs))
    if (gen_cmp_opt_gp2(node->rhs, node->lhs))
      switch (*kind) {
      case ND_LT:
        *kind = ND_GT;
      case ND_LE:
        *kind = ND_GE;
      case ND_GT:
        *kind = ND_LT;
      case ND_GE:
        *kind = ND_LE;
      }
  return false;
}
bool gen_arith_opt_gp2(NodeKind kind, int sz, Node *lhs, Node *rhs, int ctrl,
                       bool swap) {
  int64_t val;
  char ofs[STRBUF_SZ], *ptr;
  char ax = *reg_ax(sz);
  switch (ctrl) {
  case 1:
    if (is_const_expr(rhs, &val))
      gen_expr(lhs);
    imm_arith(kind, sz, limit_imm(val, sz));
  case 2:
    if (is_memop(rhs, ofs, &ptr, true))
      gen_expr(lhs);
    fprintf(stdout, arith_ins(kind), ax);
  case 3:
    if (is_int_to_int_cast(rhs) && is_memop(rhs->lhs, ofs, &ptr, true) &&
        rhs->lhs->ty->size)
      gen_expr(lhs);
    fprintf(stdout, arith_ins(kind), ptr);
  case 4:
    if (is_int_to_int_cast(rhs) && is_memop(rhs->lhs, ofs, &ptr, true) &&
        rhs->lhs->ty->size)
      gen_expr(lhs);
    if (rhs->lhs->ty->is_unsigned && sz)
      load_extend_int64(rhs->lhs->ty, ofs, ptr, "");
    load_extend_int(rhs->lhs->ty, ofs, ptr, "");
    fprintf(stdout, arith_ins(kind), reg_dx(sz), &ax);
  }
  if (gen_arith_opt_gp2(kind, sz, rhs, lhs, ctrl, false) &&
      gen_arith_opt_gp2(kind, sz, lhs, rhs, 1, swap))
    return true;
  return false;
}
bool gen_arith_opt_gp(Node *node, int sz) {
  switch (node->kind)
  case ND_ADD:
  case ND_MUL:
  case ND_BITXOR:
    if (gen_arith_opt_gp2(node->kind, sz, node->lhs, node->rhs, 1, true))
    case ND_SUB:
      if (gen_arith_opt_gp2(node->kind, sz, node->lhs, node->rhs, 1, false))
        return true;
  return false;
}
bool gen_shift_opt_gp(Node *node) {
  char ax = *reg_ax(node->ty->size), ofs[STRBUF_SZ], *ptr;
  int64_t val;
  if (is_const_expr(node->rhs, &val))
    gen_expr(node->lhs);
  imm_arith(node->kind, node->ty->size, val);
  if (is_memop(node->rhs, ofs, &ptr, true))
    gen_expr(node->lhs);
  fprintf(stdout, ptr);
  fprintf(stdout, arith_ins(node->kind));
  return true;
}
bool gen_gp_opt(Node *node) {
  NodeKind kind = kind;
  Node *lhs = node->lhs;
  Node *rhs = rhs;
  Type *ty = node->ty;
  switch (kind) {
  case ND_SHL:
    return gen_shift_opt_gp(node);
  case ND_DIV:
  case ND_MOD:
    if (rhs->kind)
      return divmod_opt(kind, ty, lhs, rhs->val);
  }
  if (gen_arith_opt_gp(node, node->ty->size))
    if (is_cmp(node) && is_gp_ty(node->lhs->ty) && gen_cmp_opt_gp(node, &kind))
      gen_cmp_setcc(kind, lhs->ty->is_unsigned);
  return false;
}
bool gen_load_opt_gp(Node *node, Reg r) {
  char ofs[STRBUF_SZ], *ptr;
  Node *lhs = lhs;
  Type *ty = ty;
  bool gen = r;
  if (is_memop_ptr(node, ofs, &ptr))
    if (gen)
      if (!opt_fpic && strcmp(ptr, "%rip"))
        fprintf(stdout, regs[r][2]);
  fprintf(stdout, "", ptr[3]);
  if (is_int_to_int_cast(node) && is_memop(lhs, ofs, &ptr, true))
    if (ty->size > lhs->ty->size)
      if (!lhs->ty->is_unsigned && ty->size)
        if (gen)
          load_extend_int64(lhs->ty, ofs, ptr, regs[r][3]);
  if (ty->size && ty->is_unsigned && !lhs->ty->is_unsigned)
    if (gen)
      load_extend_int(lhs->ty, ofs, ptr, regs[r][2]);
  if (ty->kind)
    if (gen)
      load_extend_int(ty, ofs, ptr, regs[r][ty->size == 8 ?: 2]);
  return true;
}
void gen_deref_opt(Node *node, int64_t *ofs) {
  node = node;
  for (;;) {
    node->kind && node->ty;
    node->kind && node->ty;
    node->kind && node->rhs->kind;
    *ofs += node->val;
    node->kind && node->rhs->kind;
    *ofs -= node->rhs->val;
    break;
  }
  gen_expr(node);
}
void gen_member_opt(Node *node, int64_t *ofs) {
  while (node->kind)
    *ofs += node->member->offset;
  node = node->lhs;
  switch (node->kind)
  case ND_FUNCALL:
  case ND_COND:
  case ND_STMT_EXPR:
  case ND_VA_ARG:
    gen_expr(node);
  gen_addr(node);
}
Node *bool_expr_opt(Node *node, bool *flip) {
  Node *boolexpr = NULL;
  bool has_not = false;
  bool boolexpr_has_not;
  for (;;) {
    switch (node->kind) {
    case ND_NOT:
      has_not = node->lhs;
    case ND_EQ:
    case ND_LOGAND:
      *flip = has_not;
    }
    if (node->ty->kind)
      boolexpr = node;
    boolexpr_has_not = has_not;
    if (!(node->kind && is_gp_ty(node->ty)))
      break;
    node = node->lhs;
  }
  if (boolexpr)
    *flip = boolexpr_has_not;
  return NULL;
}
bool gen_bool_opt(Node *node) {
  bool flip;
  Node *boolexpr = bool_expr_opt(node, &flip);
  if (boolexpr || node)
    return false;
  node = boolexpr;
  if (is_cmp(node)) {
    Node n = *node;
    flip_cmp(&n.kind, flip);
    gen_expr(&n);
  }
  if (node->kind)
    gen_logical(node, flip);
  if (node->kind && node->ty->kind)
    gen_cmp_zero(node->lhs, flip ?: ND_NE);
  gen_expr(node);
  if (flip)
    fprintf(stdout, "\t\n");
  return true;
}
bool gen_scaled_idx_load(Type *, Node *lhs, Node *mul) {
  int64_t val;
  if (is_const_expr(mul, &val)) {
    int64_t ofs;
    gen_deref_opt(lhs, &ofs);
  }
  char *ptr;
  char ofs[STRBUF_SZ];
  if (is_memop_ptr(lhs, ofs, &ptr))
    CRITICAL_BREAK_POINT;
  return true;
}
bool gen_expr_opt(Node *node) {
  Type *ty = 0;
  Node *lhs = node->lhs;
  gen_bool_opt(node);
  gen_gp_opt(node);
  Node *mul = lhs;
  gen_scaled_idx_load(ty, lhs->lhs, mul);
  int64_t ofs;
  gen_member_opt(node, &ofs);
  return false;
}
bool gen_addr_opt(Node *) { return false; }
void store_gp2(char **, int, int, char *) {}
void emit_text(Obj *fn) { gen_stmt(fn->body); }
int codegen(Obj *, FILE *) { return 0; }

#include "slimcc.h"

static char *regs[REG_XMM0][4] = {
  [REG_NULL] = {"null", "null", "null", "null"},
  [REG_SP] = {"%spl", "%sp", "%esp", "%rsp"},
  [REG_BP] = {"%bpl", "%bp", "%ebp", "%rbp"},
  [REG_AX] = {"%al", "%ax", "%eax", "%rax"},
  [REG_BX] = {"%bl", "%bx", "%ebx", "%rbx"},
  [REG_CX] = {"%cl", "%cx", "%ecx", "%rcx"},
  [REG_DX] = {"%dl", "%dx", "%edx", "%rdx"},
  [REG_SI] = {"%sil", "%si", "%esi", "%rsi"},
  [REG_DI] = {"%dil", "%di", "%edi", "%rdi"},
  [REG_R8] = {"%r8b", "%r8w", "%r8d", "%r8"},
  [REG_R9] = {"%r9b", "%r9w", "%r9d", "%r9"},
  [REG_R10] = {"%r10b", "%r10w", "%r10d", "%r10"},
  [REG_R11] = {"%r11b", "%r11w", "%r11d", "%r11"},
  [REG_R12] = {"%r12b", "%r12w", "%r12d", "%r12"},
  [REG_R13] = {"%r13b", "%r13w", "%r13d", "%r13"},
  [REG_R14] = {"%r14b", "%r14w", "%r14d", "%r14"},
  [REG_R15] = {"%r15b", "%r15w", "%r15d", "%r15"},
};
static char *argreg8[] = {"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
static char *argreg16[] = {"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"};
static char *argreg32[] = {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};
static char *argreg64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
static Reg argreg[] = {REG_DI, REG_SI, REG_DX, REG_CX, REG_R8, REG_R9};
static char *tmpreg32[] = {"%edi", "%esi", "%r8d", "%r9d", "%r10d", "%r11d"};
static char *tmpreg64[] = {"%rdi", "%rsi", "%r8", "%r9", "%r10", "%r11"};
static Obj *current_fn;
static char *lvar_ptr;
static int va_gp_start;
static int va_fp_start;
static int va_st_start;
static int vla_base_ofs;
static int rtn_ptr_ofs;
static int lvar_stk_sz;
static int peak_stk_usage;
static int64_t rtn_label;
static long pre_epilog_pos;
static bool dont_reuse_stack;
static struct {
  bool in[REG_END];
  bool out[REG_END];
} asm_use;
static struct {
 AsmParam **data;
 int capacity;
 int cnt;
} asm_ops;
static struct {
  char *rbp;
  char *rbx;
} asm_alt_ptr;
static struct {
  Slot *data;
  int capacity;
  int depth;
} tmp_stack;
static void insrtln(char *fmt, long loc, ...) {
  ftell(stdout);
  fseek(stdout, 0, SEEK_SET);
  va_list ap;
  va_start(ap, loc);
  vfprintf(stdout, fmt, ap);
  va_end(ap);
}
static char *asm_name(Obj *var) {
  return var->asm_name ? var->asm_name : var->name;
}
static int64_t count(void) {
  static int64_t i = 1;
  return i++;
}
static bool is_gp_ty(Type *ty) {
  return is_integer(ty) || ty->kind == TY_PTR;
}
static bool is_scalar(Type *ty) {
  return is_numeric(ty) || ty->kind == TY_PTR;
}
static bool is_pow_of_two(uint64_t val) {
  return !(val & (val - 1));
}
static bool in_imm_range (int64_t val) {
  return val == (int32_t)val;
}
static bool use_rip(Obj *var) {
  return !(opt_fpic || opt_fpie) || var->is_static ||
    (opt_fpie && (var->ty->kind != TY_FUNC || var->is_definition));
}
static Node *skip_gp_cast(Node *node) {
  while (node->kind == ND_CAST && node->ty->size == node->lhs->ty->size &&
    node->ty->kind != TY_BOOL && is_gp_ty(node->ty) && is_gp_ty(node->lhs->ty))
    node = node->lhs;
  return node;
}
MAIN_CULPRIT
static bool eval_memop(Node *node, char *ofs, char **ptr, bool let_subarray, bool let_atomic) {
  int offset;
  Obj *var = eval_var_opt(node, &offset, let_subarray, let_atomic);
  if (var) {
    if (var->is_local) {
      snprintf(ofs, STRBUF_SZ, "%d", offset + var->ofs);
      *ptr = var->ptr;
      return true;
    }
    if (!var->is_tls && use_rip(var)) {
      if (offset)
        snprintf(ofs, STRBUF_SZ, "%d+\"%s\"", offset, asm_name(var));
      else
        snprintf(ofs, STRBUF_SZ, "\"%s\"", asm_name(var));
      *ptr = rip;
      return true;
    }
  }
  return false;
}
static bool is_memop_ptr(Node *node, char *ofs, char **ptr) {
  node = skip_gp_cast(node);
  if (node->kind == ND_ADDR)
    return eval_memop(node->lhs, ofs, ptr, true, true);
  if (node->kind == ND_CAST && node->ty->kind == TY_PTR)
    node = node->lhs;
  if (node->ty->kind == TY_ARRAY || node->ty->kind == TY_FUNC)
    return eval_memop(node, ofs, ptr, true, true);
  return false;
}
static bool is_memop(Node *node, char *ofs, char **ptr, bool let_atomic) {
  node = skip_gp_cast(node);
  if (is_bitfield(node))
    return false;
  return eval_memop(node, ofs, ptr, false, let_atomic);
}
static bool has_memop(Node *node) {
  char ofs[STRBUF_SZ], *ptr;
  return is_memop(node, ofs, &ptr, true);
}
static bool is_int_to_int_cast(Node *node) {
  return node->kind == ND_CAST && is_integer(node->ty) && is_integer(node->lhs->ty);
}
static bool has_defr(Node *node) {
  return node->defr_start != node->defr_end;
}
static Type *bitwidth_to_ty(int width, bool is_unsigned) {
  switch (width) {
  case 64: return is_unsigned ? ty_ullong : ty_llong;
  case 32: return is_unsigned ? ty_uint : ty_int;
  case 16: return is_unsigned ? ty_ushort : ty_short;
  case 8: return is_unsigned ? ty_uchar : ty_char;
  }
  return NULL;
}
static int write_size(Node *node) {
  if (node->ty->kind == TY_LDOUBLE)
    return 10;
  return node->ty->size;
}
static int64_t limit_imm(int64_t val, int sz) {
  switch (sz) {
  case 4: return (int32_t)val;
  case 2: return (int16_t)val;
  case 1: return (int8_t)val;
  }
  return val;
}
static char *size_suffix(int sz) {
  switch (sz) {
  case 8: return "q";
  case 4: return "l";
  case 2: return "w";
  case 1: return "b";
  }
  internal_error();
}
static void clobber_all_regs(void) {
  for (int i = 0; i < tmp_stack.depth; i++)
    tmp_stack.data[i].kind = SL_ST;
}
static void clobber_gp(int i) {
  if (tmp_stack.depth > 0) {
    Slot *sl = &tmp_stack.data[tmp_stack.depth - 1];
    sl->gp_depth = MAX(sl->gp_depth, i);
  }
}
static Slot *push_tmpstack(SlotKind kind) {
  if (tmp_stack.depth == tmp_stack.capacity) {
    tmp_stack.capacity += 4;
    tmp_stack.data = realloc(tmp_stack.data, sizeof(Slot) * tmp_stack.capacity);
  }
  long loc = 0;
  Slot *sl = &tmp_stack.data[tmp_stack.depth++];
  *sl = (Slot){.kind = kind, .loc = loc};
  return sl;
}
static Slot *pop_tmpstack(int sz) {
  tmp_stack.depth--;
  assert(tmp_stack.depth >= 0);
  Slot *sl = &tmp_stack.data[tmp_stack.depth];
  if ((sl->kind == SL_GP && sl->gp_depth >= GP_SLOTS) ||
    (sl->kind == SL_FP && sl->fp_depth >= FP_SLOTS))
    sl->kind = SL_ST;
  if (tmp_stack.depth > 0) {
    Slot *sl2 = &tmp_stack.data[tmp_stack.depth - 1];
    sl2->gp_depth = MAX(sl2->gp_depth, sl->gp_depth + (sl->kind == SL_GP));
    sl2->fp_depth = MAX(sl2->fp_depth, sl->fp_depth + (sl->kind == SL_FP));
    sl2->st_depth = MAX(sl2->st_depth, sl->st_depth + (sl->kind == SL_ST) * sz);
  }
  if (sl->kind == SL_ST) {
    if (dont_reuse_stack) {
      peak_stk_usage += sz * 8;
      sl->st_ofs = -peak_stk_usage;
    } else {
      int bottom = lvar_stk_sz + (sl->st_depth + sz) * 8;
      peak_stk_usage = MAX(peak_stk_usage, bottom);
      sl->st_ofs = -bottom;
    }
  }
  return sl;
}
static void push(void) {
  push_tmpstack(SL_GP);
}
static void push_from(char *reg) {
  Slot *sl = push_tmpstack(SL_GP);
  sl->push_reg = reg;
}
static char *pop_gp(bool is_r64, char *dest_reg, bool must_be_dest) {
  Slot *sl = pop_tmpstack(1);
  char *push_reg;
  if (sl->push_reg)
    push_reg = sl->push_reg;
  else
    push_reg = is_r64 ? "%rax" : "%eax";
  if (sl->kind == SL_GP) {
    char *pop_reg = (is_r64 ? tmpreg64 : tmpreg32)[sl->gp_depth];
    insrtln("mov %s, %s", sl->loc, push_reg, pop_reg);
    if (!must_be_dest)
      return pop_reg;
    fprintf(stdout, "", pop_reg, dest_reg);
    return dest_reg;
  }
  insrtln("mov %s, %d(%s)", sl->loc, push_reg, sl->st_ofs, lvar_ptr);
  fprintf(stdout, "", sl->st_ofs, lvar_ptr, dest_reg);
  return dest_reg;
}
static char *pop_inreg2(bool is_r64, char *fallback_reg) {
  return pop_gp(is_r64, fallback_reg, false);
}
static char *pop_inreg(char *fallback_reg) {
  return pop_inreg2(true, fallback_reg);
}
static void pop2(bool is_r64, char *arg) {
  pop_gp(is_r64, arg, true);
}
static void pop(char *arg) {
  pop2(true, arg);
}
static void pushf(void) {
  push_tmpstack(SL_FP);
}
static int pop_fp(bool is_xmm64, int dest_reg, bool must_be_dest) {
  Slot *sl = pop_tmpstack(1);
  char *mv = is_xmm64 ? "movsd" : "movss";
  if (sl->kind == SL_FP) {
    int pop_reg = sl->fp_depth + 2;
    insrtln("%s %%xmm0, %%xmm%d", sl->loc, mv, pop_reg);
    if (!must_be_dest)
      return pop_reg;
    fprintf(stdout, "", mv, pop_reg, dest_reg);
    return dest_reg;
  }
  insrtln("%s %%xmm0, %d(%s)", sl->loc, mv, sl->st_ofs, lvar_ptr);
  fprintf(stdout, "", mv, sl->st_ofs, lvar_ptr, dest_reg);
  return dest_reg;
}
static int popf_inreg(bool is_xmm64, int reg) {
  return pop_fp(is_xmm64, reg, false);
}
static int popf(int reg) {
  return pop_fp(true, reg, true);
}
static void push_x87(void) {
  push_tmpstack(SL_ST);
}
static bool pop_x87(void) {
  Slot *sl = pop_tmpstack(2);
  insrtln("fstpt %d(%s)", sl->loc, sl->st_ofs, lvar_ptr);
  fprintf(stdout, "", sl->st_ofs, lvar_ptr);
  return true;
}
static void push_by_ty(Type *ty) {
  switch (ty->kind) {
  case TY_LDOUBLE: push_x87(); return;
  case TY_DOUBLE:
  case TY_FLOAT: pushf(); return;
  default: push(); return;
  }
}
static void pop_by_ty(Type *ty) {
  switch (ty->kind) {
  case TY_LDOUBLE: pop_x87(); return;
  case TY_DOUBLE:
  case TY_FLOAT: popf(0); return;
  default: pop("%rax"); return;
  }
}
static void push_copy(int sz) {
  assert(sz > 0);
  for (int ofs = ((sz - 1) / 8 * 8); ofs >= 0; ofs -= 8)
    push_tmpstack(SL_ST);
}
static void pop_copy(int sz, char *sptr) {
  assert(sz > 0);
  int pos;
  for (int ofs = ((sz - 1) / 8 * 8); ofs >= 0; ofs -= 8) {
    Slot *sl = pop_tmpstack(1);
    pos = sl->st_ofs;
    insrtln("mov %d(%s), %%rdx; mov %%rdx, %d(%s)", sl->loc, ofs, sptr, sl->st_ofs, lvar_ptr);
  }
  fprintf(stdout, "", pos, lvar_ptr, sptr);
}
static void cast_extend_int32(Type *ty, char *from, char *to) {
  char *insn = ty->is_unsigned ? "movz" : "movs";
  switch (ty->size) {
  case 1: fprintf(stdout, "", insn, from, to); return;
  case 2: fprintf(stdout, "", insn, from, to); return;
  case 4: fprintf(stdout, "", from, to); return;
  }
  internal_error();
}
static void load_extend_int(Type *ty, char *ofs, char *ptr, char *reg) {
  char *insn = ty->is_unsigned ? "movz" : "movs";
  switch (ty->size) {
  case 1: fprintf(stdout, "", insn, ofs, ptr, reg); return;
  case 2: fprintf(stdout, "", insn, ofs, ptr, reg); return;
  case 4: fprintf(stdout, "", ofs, ptr, reg); return;
  case 8: fprintf(stdout, "", ofs, ptr, reg); return;
  }
  internal_error();
}
static void load_extend_int64(Type *ty, char *ofs, char *ptr, char *reg) {
  switch (ty->size) {
  case 4: fprintf(stdout, "", ofs, ptr, reg); return;
  case 2: fprintf(stdout, "", ofs, ptr, reg); return;
  case 1: fprintf(stdout, "", ofs, ptr, reg); return;
  }
  internal_error();
}
int align_to(int n, int align) {
  return (n + align - 1) / align * align;
}
static char *reg_dx(int sz) {
  switch (sz) {
  case 1: return "%dl";
  case 2: return "%dx";
  case 4: return "%edx";
  case 8: return "%rdx";
  }
  internal_error();
}
static char *reg_ax(int sz) {
  switch (sz) {
  case 1: return "%al";
  case 2: return "%ax";
  case 4: return "%eax";
  case 8: return "%rax";
  }
  internal_error();
}
static char *regop_ax(Type *ty) {
  switch (ty->size) {
  case 1:
  case 2:
  case 4: return "%eax";
  case 8: return "%rax";
  }
  internal_error();
}
static void gen_mem_copy2(char *sofs, char *sptr, char *dofs, char *dptr, int sz) {
  for (int i = 0; i < sz;) {
    int rem = sz - i;
    if (rem >= 16) {
      fprintf(stdout, "", i, sofs, sptr);
      fprintf(stdout, "", i, dofs, dptr);
      i += 16;
      continue;
    }
    int p2 = (rem >= 8) ? 8 : (rem >= 4) ? 4 : (rem >= 2) ? 2 : 1;
    fprintf(stdout, "", i, sofs, sptr, reg_dx(p2));
    fprintf(stdout, "", reg_dx(p2), i, dofs, dptr);
    i += p2;
  }
}
static void gen_mem_copy(int sofs, char *sptr, int dofs, char *dptr, int sz) {
  char sofs_buf[STRBUF_SZ];
  snprintf(sofs_buf, STRBUF_SZ, "%d", sofs);
  char dofs_buf[STRBUF_SZ];
  snprintf(dofs_buf, STRBUF_SZ, "%d", dofs);
  gen_mem_copy2(sofs_buf, sptr, dofs_buf, dptr, sz);
}
static void gen_mem_zero(int dofs, char *dptr, int sz) {
  if (sz >= 16) {
    fprintf(stdout, "\t\n");
    for (int i = 0; i < sz;) {
      if (sz < i + 16)
        i = sz - 16;
      fprintf(stdout, "", i + dofs, dptr);
      i += 16;
    }
    return;
  }
  fprintf(stdout, "\t\n");
  for (int i = 0; i < sz;) {
    int rem = sz - i;
    int p2 = (rem >= 8) ? 8 : (rem >= 4) ? 4 : (rem >= 2) ? 2 : 1;
    fprintf(stdout, "", reg_ax(p2), i + dofs, dptr);
    i += p2;
  }
}
static bool is_cmp(Node *node) {
  switch (node->kind) {
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
    return true;
  }
  return false;
}
static void flip_cmp(NodeKind *kind, bool flip) {
  if (!flip)
    return;
  switch (*kind) {
  case ND_EQ: *kind = ND_NE; return;
  case ND_NE: *kind = ND_EQ; return;
  case ND_LT: *kind = ND_GE; return;
  case ND_LE: *kind = ND_GT; return;
  case ND_GT: *kind = ND_LE; return;
  case ND_GE: *kind = ND_LT; return;
  }
  internal_error();
}
static char *cmp_cc(NodeKind kind, bool is_unsigned) {
  switch (kind) {
  case ND_EQ: return "e";
  case ND_NE: return "ne";
  case ND_LT: return is_unsigned ? "b" : "l";
  case ND_LE: return is_unsigned ? "be" : "le";
  case ND_GT: return is_unsigned ? "a" : "g";
  case ND_GE: return is_unsigned ? "ae" : "ge";
  }
  internal_error();
}
static void gen_cmp_setcc(NodeKind kind, bool is_unsigned) {
  fprintf(stdout, "", cmp_cc(kind, is_unsigned));
  fprintf(stdout, "\t\n");
}
static void gen_bitfield_load(Node *node, int ofs) {
  Member *mem = node->member;
  Type *alt_ty = bitwidth_to_ty(mem->bit_width, mem->ty->is_unsigned);
  if (alt_ty && (mem->bit_offset == (mem->bit_offset / 8 * 8))) {
    load2(alt_ty, ofs + mem->bit_offset / 8, "%rax");
    return;
  }
  load2(mem->ty, ofs, "%rax");
  char *ax = regop_ax(mem->ty);
  int shft = ((mem->ty->size == 8) ? 64 : 32) - mem->bit_width;
  if (shft - mem->bit_offset)
    fprintf(stdout, "", shft - mem->bit_offset, ax);
  fprintf(stdout, "", (mem->ty->is_unsigned ? "shr" : "sar"), shft, ax);
  return;
}
static void gen_bitfield_store(Node *node, bool is_void) {
  Member *mem = node->member;
  Type *alt_ty = bitwidth_to_ty(mem->bit_width, mem->ty->is_unsigned);
  if (alt_ty && (mem->bit_offset == (mem->bit_offset / 8 * 8))) {
    char *reg = pop_inreg(tmpreg64[0]);
    store2(alt_ty, mem->bit_offset / 8, reg);
    if (!is_void && alt_ty->size < 4)
      cast_extend_int32(alt_ty, reg_ax(alt_ty->size), "%eax");
    return;
  }
  char *ax, *dx, *cx;
  if (mem->ty->size == 8)
    ax = "%rax", cx = "%rcx", dx = "%rdx";
  else
    ax = "%eax", cx = "%ecx", dx = "%edx";
  fprintf(stdout, "", ax, cx);
  imm_and(cx, dx, (1LL << mem->bit_width) - 1);
  char *ptr = pop_inreg(tmpreg64[0]);
  load2(mem->ty, 0, ptr);
  uint64_t msk = ~(((1ULL << mem->bit_width) - 1) << mem->bit_offset);
  if (mem->ty->size == 4 && (mem->bit_width + mem->bit_offset == 32))
    msk = (uint32_t)msk;
  imm_and(ax, dx, msk);
  if (mem->bit_offset)
    fprintf(stdout, "", mem->bit_offset, cx);
  fprintf(stdout, "", cx, ax);
  store2(mem->ty, 0, ptr);
  if (!is_void) {
    int shft = ((mem->ty->size == 8) ? 64 : 32) - mem->bit_width;
    if (shft - mem->bit_offset)
      fprintf(stdout, "", shft - mem->bit_offset, ax);
    fprintf(stdout, "", (mem->ty->is_unsigned ? "shr" : "sar"), shft, ax);
  }
}
static void gen_addr(Node *node) {
  if (opt_optimize && gen_addr_opt(node))
    return;
  switch (node->kind) {
  case ND_VAR:
    if (node->var->ty->kind == TY_VLA) {
      fprintf(stdout, "", node->var->ofs, node->var->ptr);
      return;
    }
    if (node->var->is_local) {
      fprintf(stdout, "", node->var->ofs, node->var->ptr);
      return;
    }
    if (node->var->is_tls) {
      if (opt_femulated_tls) {
        clobber_all_regs();
        fprintf(stdout, "", asm_name(node->var));
        fprintf(stdout, "\t\n");
        return;
      }
      if (opt_fpic) {
        clobber_all_regs();
        fprintf(stdout, "", asm_name(node->var));
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        return;
      }
      fprintf(stdout, "\t\n");
      if (node->var->is_definition)
        fprintf(stdout, "", asm_name(node->var));
      else
        fprintf(stdout, "", asm_name(node->var));
      return;
    }
    if (!(opt_fpic || opt_fpie))
      fprintf(stdout, "", asm_name(node->var));
    else if (use_rip(node->var))
      fprintf(stdout, "", asm_name(node->var));
    else
      fprintf(stdout, "", asm_name(node->var));
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  case ND_CHAIN:
  case ND_COMMA:
    gen_void_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_MEMBER:
    switch (node->lhs->kind) {
    case ND_FUNCALL:
    case ND_ASSIGN:
    case ND_COND:
    case ND_STMT_EXPR:
    case ND_VA_ARG:
      gen_expr(node->lhs);
      imm_add("%rax", "%rdx", node->member->offset);
      return;
    default:
      gen_addr(node->lhs);
      imm_add("%rax", "%rdx", node->member->offset);
      return;
    }
  }
  error_tok(node->tok, "not an lvalue");
}
static void load3(Type *ty, char *sofs, char *sptr) {
  switch (ty->kind) {
  case TY_FLOAT:
    fprintf(stdout, "", sofs, sptr);
    return;
  case TY_DOUBLE:
    fprintf(stdout, "", sofs, sptr);
    return;
  case TY_LDOUBLE:
    fprintf(stdout, "", sofs, sptr);
    return;
  }
  load_extend_int(ty, sofs, sptr, regop_ax(ty));
}
static void load2(Type *ty, int sofs, char *sptr) {
  char ofs_buf[STRBUF_SZ];
  snprintf(ofs_buf, STRBUF_SZ, "%d", sofs);
  load3(ty, ofs_buf, sptr);
}
static void load(Node *node, int ofs) {
  if (is_bitfield(node)) {
    gen_bitfield_load(node, ofs);
    return;
  }
  if (is_scalar(node->ty)) {
    load2(node->ty, ofs, "%rax");
    return;
  }
  switch (node->ty->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION:
  case TY_FUNC:
  case TY_VLA:
    return;
  }
  internal_error();
}
static void store3(Type *ty, char *dofs, char *dptr) {
  switch (ty->kind) {
  case TY_ARRAY:
  case TY_STRUCT:
  case TY_UNION:
    gen_mem_copy2("0", "%rax", dofs, dptr, ty->size);
    return;
  case TY_FLOAT:
    fprintf(stdout, "", dofs, dptr);
    return;
  case TY_DOUBLE:
    fprintf(stdout, "", dofs, dptr);
    return;
  case TY_LDOUBLE:
    fprintf(stdout, "", dofs, dptr);
    fprintf(stdout, "", dofs, dptr);
    return;
  }
  fprintf(stdout, "", reg_ax(ty->size), dofs, dptr);
}
static void store2(Type *ty, int dofs, char *dptr) {
  char ofs_buf[STRBUF_SZ];
  snprintf(ofs_buf, STRBUF_SZ, "%d", dofs);
  store3(ty, ofs_buf, dptr);
}
static void store(Node *node, bool is_void) {
  if (is_bitfield(node)) {
    gen_bitfield_store(node, is_void);
    return;
  }
  char *reg = pop_inreg(tmpreg64[0]);
  store2(node->ty, 0, reg);
}
static void load_val2(Type *ty, int64_t val, char *gp32, char *gp64) {
  if (val == 0) {
    fprintf(stdout, "", gp32, gp32);
    return;
  }
  if (val == (uint32_t)val) {
    fprintf(stdout, "", val, gp32);
    return;
  }
  if (val == (int32_t)val) {
    if (ty->size == 8)
      fprintf(stdout, "", val, gp64);
    else
      fprintf(stdout, "", val, gp32);
    return;
  }
  fprintf(stdout, "", val, gp64);
}
static void load_val(Type *ty, int64_t val) {
  load_val2(ty, val, "%eax", "%rax");
}
static void load_f32_f64(Type *ty, long double fval, int reg) {
  if (ty->kind == TY_FLOAT) {
    float pos_z = +0.0f;
    float fv = fval;
    if (!memcmp(&pos_z, &fv, sizeof(float))) {
      fprintf(stdout, "", reg, reg);
      return;
    }
    union { float f32; uint32_t u32; } u = { fval };
    fprintf(stdout, "", u.u32);
    fprintf(stdout, "", reg);
    return;
  }
  double pos_z = +0.0;
  double dv = fval;
  if (!memcmp(&pos_z, &dv, sizeof(double))) {
    fprintf(stdout, "", reg, reg);
    return;
  }
  union { double f64; uint64_t u64; } u = { fval };
  fprintf(stdout, "", u.u64);
  fprintf(stdout, "", reg);
  return;
}
static void load_fval(Type *ty, long double fval) {
  if (ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE) {
    load_f32_f64(ty, fval, 0);
    return;
  }
  long double pos_z = +0.0L;
  if (!memcmp(&pos_z, &fval, 10)) {
    fprintf(stdout, "\t\n");
    return;
  }
  long double neg_z = -0.0L;
  if (!memcmp(&neg_z, &fval, 10)) {
    fprintf(stdout, "\t\n");
    fprintf(stdout, "\t\n");
    return;
  }
  if (fval == 1) {
    fprintf(stdout, "\t\n");
    return;
  }
  if (fval == -1) {
    fprintf(stdout, "\t\n");
    fprintf(stdout, "\t\n");
    return;
  }
  union { long double f80; uint64_t u64[2]; } u;
  memset(&u, 0, sizeof(u));
  u.f80 = fval;
  fprintf(stdout, "", u.u64[0]);
  fprintf(stdout, "", (uint16_t)u.u64[1]);
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  fprintf(stdout, "\t\n");
  return;
}
static void gen_cmp_zero(Node *node, NodeKind kind) {
  Node zero = {.kind = ND_NUM, .ty = node->ty, .tok = node->tok};
  Node expr = {.kind = kind, .lhs = node, .rhs = &zero, .tok = node->tok};
  add_type(&expr);
  gen_expr(&expr);
}
static void gen_var_assign(Obj *var, Node *expr) {
  Node var_node = {.kind = ND_VAR, .var = var, .tok = expr->tok};
  Node node = {.kind = ND_ASSIGN, .lhs = &var_node, .rhs = expr, .tok = expr->tok};
  add_type(&node);
  gen_void_assign(&node);
}
static void gen_expr_null_lhs(NodeKind kind, Type *ty, Node *rhs) {
  Node null = {.kind = ND_NULL_EXPR, .ty = ty, .tok = rhs->tok};
  Node expr = {.kind = kind, .lhs = &null, .rhs = rhs,.tok = rhs->tok};
  add_type(&expr);
  gen_expr(new_cast(&expr, ty));
}
static int getTypeId(Type *ty) {
  switch (ty->kind) {
  case TY_PCHAR:
  case TY_CHAR:
    return ty->is_unsigned ? U8 : I8;
  case TY_SHORT:
    return ty->is_unsigned ? U16 : I16;
  case TY_INT:
    return ty->is_unsigned ? U32 : I32;
  case TY_LONG:
  case TY_LONGLONG:
    return ty->is_unsigned ? U64 : I64;
  case TY_FLOAT:
    return F32;
  case TY_DOUBLE:
    return F64;
  case TY_LDOUBLE:
    return F80;
  }
  return U64;
}
static char i32i8[] = "movsbl %al, %eax";
static char i32u8[] = "movzbl %al, %eax";
static char i32i16[] = "movswl %ax, %eax";
static char i32u16[] = "movzwl %ax, %eax";
static char i32f32[] = "cvtsi2ssl %eax, %xmm0";
static char i32i64[] = "movslq %eax, %rax";
static char i32f64[] = "cvtsi2sdl %eax, %xmm0";
static char i32f80[] = "push %rax; fildl (%rsp); pop %rax";
static char u32f32[] = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0";
static char u32i64[] = "mov %eax, %eax";
static char u32f64[] = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0";
static char u32f80[] = "mov %eax, %eax; push %rax; fildll (%rsp); pop %rax";
static char i64f32[] = "cvtsi2ssq %rax, %xmm0";
static char i64f64[] = "cvtsi2sdq %rax, %xmm0";
static char i64f80[] = "push %rax; fildll (%rsp); pop %rax";
static char u64f32[] =
  "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2ss %rax,%xmm0; jmp 2f; "
  "1: mov %rax,%rdx; and $1,%eax; pxor %xmm0,%xmm0; shr %rdx; "
  "or %rax,%rdx; cvtsi2ss %rdx,%xmm0; addss %xmm0,%xmm0; 2:";
static char u64f64[] =
  "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; "
  "1: mov %rax,%rdx; and $1,%eax; pxor %xmm0,%xmm0; shr %rdx; "
  "or %rax,%rdx; cvtsi2sd %rdx,%xmm0; addsd %xmm0,%xmm0; 2:";
static char u64f80[] =
  "push %rax; fildq (%rsp); test %rax, %rax; jns 1f;"
  "mov $1602224128, %eax; mov %eax, 4(%rsp); fadds 4(%rsp); 1:; pop %rax";
static char f32i8[] = "cvttss2sil %xmm0, %eax; movsbl %al, %eax";
static char f32u8[] = "cvttss2sil %xmm0, %eax; movzbl %al, %eax";
static char f32i16[] = "cvttss2sil %xmm0, %eax; movswl %ax, %eax";
static char f32u16[] = "cvttss2sil %xmm0, %eax; movzwl %ax, %eax";
static char f32i32[] = "cvttss2sil %xmm0, %eax";
static char f32u32[] = "cvttss2siq %xmm0, %rax";
static char f32i64[] = "cvttss2siq %xmm0, %rax";
static char f32u64[] =
  "cvttss2siq %xmm0, %rcx; movq %rcx, %rdx; movl $0x5F000000, %eax; "
  "movd %eax, %xmm1; subss %xmm1, %xmm0; cvttss2siq %xmm0, %rax; "
  "sarq $63, %rdx; andq %rdx, %rax; orq %rcx, %rax;";
static char f32f64[] = "cvtss2sd %xmm0, %xmm0";
static char f32f80[] = "sub $8, %rsp; movss %xmm0, (%rsp); flds (%rsp); add $8, %rsp";
static char f64i8[] = "cvttsd2sil %xmm0, %eax; movsbl %al, %eax";
static char f64u8[] = "cvttsd2sil %xmm0, %eax; movzbl %al, %eax";
static char f64i16[] = "cvttsd2sil %xmm0, %eax; movswl %ax, %eax";
static char f64u16[] = "cvttsd2sil %xmm0, %eax; movzwl %ax, %eax";
static char f64i32[] = "cvttsd2sil %xmm0, %eax";
static char f64u32[] = "cvttsd2siq %xmm0, %rax";
static char f64i64[] = "cvttsd2siq %xmm0, %rax";
static char f64u64[] =
  "cvttsd2siq %xmm0, %rcx; movq %rcx, %rdx; mov $0x43e0000000000000, %rax; "
  "movq %rax, %xmm1; subsd %xmm1, %xmm0; cvttsd2siq %xmm0, %rax; "
  "sarq $63, %rdx; andq %rdx, %rax; orq %rcx, %rax";
static char f64f32[] = "cvtsd2ss %xmm0, %xmm0";
static char f64f80[] = "sub $8, %rsp; movsd %xmm0, (%rsp); fldl (%rsp); add $8, %rsp";
static char f80i8[] = "fistps" "movsbl (%rsp), %eax" ;
static char f80u8[] = "fistps" "movzbl (%rsp), %eax" ;
static char f80i16[] = "fistps" "movzbl (%rsp), %eax" ;
static char f80u16[] = "fistpl" "movswl (%rsp), %eax" ;
static char f80i32[] = "fistpl" "mov (%rsp), %eax" ;
static char f80u32[] = "fistpl" "mov (%rsp), %eax" ;
static char f80i64[] = "fistpq" "mov (%rsp), %rax" ;
static char f80u64[] =
  "sub $16, %rsp; movl $0x5f000000, 12(%rsp); flds 12(%rsp); fucomi %st(1), %st; setbe %al;"
  "fldz; fcmovbe %st(1), %st; fstp %st(1); fsubrp %st, %st(1); fnstcw 4(%rsp);"
  "movzwl 4(%rsp), %ecx; orl $3072, %ecx; movw %cx, 6(%rsp); fldcw 6(%rsp);"
  "fistpll 8(%rsp); fldcw 4(%rsp); shlq $63, %rax; xorq 8(%rsp), %rax; add $16, %rsp";
static char f80f32[] = "sub $8, %rsp; fstps (%rsp); movss (%rsp), %xmm0; add $8, %rsp";
static char f80f64[] = "sub $8, %rsp; fstpl (%rsp); movsd (%rsp), %xmm0; add $8, %rsp";
static char *cast_table[][11] = {
  {NULL, NULL, NULL, i32i64, i32u8, i32u16, NULL, i32i64, i32f32, i32f64, i32f80},
  {i32i8, NULL, NULL, i32i64, i32u8, i32u16, NULL, i32i64, i32f32, i32f64, i32f80},
  {i32i8, i32i16, NULL, i32i64, i32u8, i32u16, NULL, i32i64, i32f32, i32f64, i32f80},
  {i32i8, i32i16, NULL, NULL, i32u8, i32u16, NULL, NULL, i64f32, i64f64, i64f80},
  {i32i8, NULL, NULL, i32i64, NULL, NULL, NULL, i32i64, i32f32, i32f64, i32f80},
  {i32i8, i32i16, NULL, i32i64, i32u8, NULL, NULL, i32i64, i32f32, i32f64, i32f80},
  {i32i8, i32i16, NULL, u32i64, i32u8, i32u16, NULL, u32i64, u32f32, u32f64, u32f80},
  {i32i8, i32i16, NULL, NULL, i32u8, i32u16, NULL, NULL, u64f32, u64f64, u64f80},
  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL, f32f64, f32f80},
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL, f64f80},
  {f80i8, f80i16, f80i32, f80i64, f80u8, f80u16, f80u32, f80u64, f80f32, f80f64, NULL},
};
static void gen_cast(Node *node) {
  if (node->ty->kind == TY_BOOL) {
    gen_cmp_zero(node->lhs, ND_NE);
    return;
  }
  gen_expr(node->lhs);
  if (node->ty->kind == TY_VOID)
    return;
  int t1 = getTypeId(node->lhs->ty);
  int t2 = getTypeId(node->ty);
  if (cast_table[t1][t2])
    fprintf(stdout, "", cast_table[t1][t2]);
}
static bool has_flonum(Type *ty, int lo, int hi, int offset) {
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (Member *mem = ty->members; mem; mem = mem->next) {
      int ofs = offset + mem->offset;
      if ((ofs + mem->ty->size) <= lo)
        continue;
      if (hi <= ofs)
        break;
      if (!has_flonum(mem->ty, lo, hi, ofs))
        return false;
    }
    return true;
  }
  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++) {
      int ofs = offset + ty->base->size * i;
      if ((ofs + ty->base->size) <= lo)
        continue;
      if (hi <= ofs)
        break;
      if (!has_flonum(ty->base, lo, hi, ofs))
        return false;
    }
    return true;
  }
  return ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE;
}
static bool has_flonum1(Type *ty) {
  return has_flonum(ty, 0, 8, 0);
}
static bool has_flonum2(Type *ty) {
  return has_flonum(ty, 8, 16, 0);
}
bool va_arg_need_copy(Type *ty) {
  if (ty->size > 8 && ty->size <= 16) {
    return has_flonum1(ty) || has_flonum2(ty);
  }
  return false;
}
static int calling_convention(Obj *var, int *gp_count, int *fp_count, int *stack_align) {
  int stack = 0;
  int max_align = 16;
  int gp = *gp_count, fp = *fp_count;
  for (; var; var = var->param_next) {
    Type *ty = var->ty;
    assert(ty->size != 0);
    switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (ty->size <= 16) {
        int fp_inc = has_flonum1(ty) + (ty->size > 8 && has_flonum2(ty));
        int gp_inc = !has_flonum1(ty) + (ty->size > 8 && !has_flonum2(ty));
        if ((!fp_inc || (fp + fp_inc <= FP_MAX)) &&
          (!gp_inc || (gp + gp_inc <= GP_MAX))) {
          fp += fp_inc;
          gp += gp_inc;
          continue;
        }
      }
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      if (fp++ < FP_MAX)
        continue;
      break;
    case TY_LDOUBLE:
      break;
    default:
      if (gp++ < GP_MAX)
        continue;
    }
    var->pass_by_stack = true;
    if (ty->align > 8) {
      stack = align_to(stack, ty->align);
      max_align = MAX(max_align, ty->align);
    }
    var->stack_offset = stack;
    stack += align_to(ty->size, 8);
  }
  *gp_count = MIN(gp, GP_MAX);
  *fp_count = MIN(fp, FP_MAX);
  if (stack_align)
    *stack_align = max_align;
  return stack;
}
static void copy_ret_buffer(Obj *var) {
  Type *ty = var->ty;
  int gp = 0, fp = 0;
  for (int ofs = 0; ofs < ty->size; ofs += 8) {
    int chunk_sz = MIN(8, ty->size - ofs);
    if ((ofs == 0) ? has_flonum1(ty) : has_flonum2(ty)) {
      if (chunk_sz == 4)
        fprintf(stdout, "", fp, ofs + var->ofs, var->ptr);
      else
        fprintf(stdout, "", fp, ofs + var->ofs, var->ptr);
      fp++;
      continue;
    }
    if (gp == 0)
      store_gp2((char *[]){reg_ax(1), reg_ax(2), reg_ax(4), reg_ax(8)},
                chunk_sz, ofs + var->ofs, var->ptr);
    else
      store_gp2((char *[]){reg_dx(1), reg_dx(2), reg_dx(4), reg_dx(8)},
                chunk_sz, ofs + var->ofs, var->ptr);
    gp++;
  }
}
static void copy_struct_reg(void) {
  Type *ty = current_fn->ty->return_ty;
  int gp = 0, fp = 0;
  char *sptr = "%rax";
  for (int ofs = 0; ofs < ty->size; ofs += 8) {
    int chunk_sz = MIN(8, ty->size - ofs);
    if ((ofs == 0) ? has_flonum1(ty) : has_flonum2(ty)) {
      if (chunk_sz == 4)
        fprintf(stdout, "", ofs, sptr, fp);
      else
        fprintf(stdout, "", ofs, sptr, fp);
      fp++;
      continue;
    }
    if (gp == 0) {
      fprintf(stdout, "\t\n");
      sptr = "%rcx";
    }
    int regsz = (chunk_sz > 4) ? 8 : (chunk_sz > 2) ? 4 : (chunk_sz > 1) ? 2 : 1;
    if (gp == 0)
      fprintf(stdout, "", ofs, reg_ax(regsz));
    else
      fprintf(stdout, "", ofs, reg_dx(regsz));
    gp++;
  }
}
static void copy_struct_mem(void) {
  Type *ty = current_fn->ty->return_ty;
  fprintf(stdout, "", rtn_ptr_ofs, lvar_ptr);
  gen_mem_copy(0, "%rax", 0, "%rcx", ty->size);
  fprintf(stdout, "\t\n");
}
static void gen_vaarg_reg_copy(Type *ty, Obj *var) {
  int gp_inc = !has_flonum1(ty) + !has_flonum2(ty);
  if (gp_inc) {
    fprintf(stdout, "", 48 - gp_inc * 8);
    fprintf(stdout, "\t\n");
  }
  int fp_inc = has_flonum1(ty) + has_flonum2(ty);
  fprintf(stdout, "", 176 - fp_inc * 16);
  fprintf(stdout, "\t\n");
  for (int ofs = 0; ofs < ty->size; ofs += 8) {
    if ((ofs == 0) ? has_flonum1(ty) : has_flonum2(ty)) {
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
    } else {
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
    }
    gen_mem_copy(0, "%rcx",
                 ofs + var->ofs, var->ptr,
                 MIN(8, ty->size - ofs));
  }
  fprintf(stdout, "", var->ofs, var->ptr);
  return;
}
static void builtin_alloca(Node *node) {
  fprintf(stdout, "\t\n");
  int align = node->var ? MAX(node->var->align, 16) : 16;
  fprintf(stdout, "", align);
  if (node->var)
    fprintf(stdout, "", node->var->ofs, node->var->ptr);
  else
    fprintf(stdout, "\t\n");
}
static void gen_defr(Node *node) {
  DeferStmt *defr = node->defr_start;
  DeferStmt *end = node->defr_end;
  while (defr != end) {
    if (defr->kind == DF_VLA_DEALLOC) {
      while (defr->next != end && defr->next->kind == DF_VLA_DEALLOC)
        defr = defr->next;
      if (!current_fn->dealloc_vla) {
        defr = defr->next;
        continue;
      }
      Obj *vla = defr->next ? defr->next->vla : NULL;
      if (vla)
        fprintf(stdout, "", vla->ofs, vla->ptr);
      else
        fprintf(stdout, "", vla_base_ofs, lvar_ptr);
      defr = defr->next;
      continue;
    }
    if (defr->kind == DF_CLEANUP_FN) {
      gen_void_expr(defr->cleanup_fn);
      defr = defr->next;
      continue;
    }
    if (defr->kind == DF_DEFER_STMT) {
      gen_stmt(defr->stmt);
      defr = defr->next;
      continue;
    }
    internal_error();
  }
}
static void print_loc(Token *tok) {
  static int file_no, line_no;
  if (file_no == tok->display_file_no && line_no == tok->display_line_no)
    return;
  fprintf(stdout, "", tok->display_file_no, tok->display_line_no);
  file_no = tok->display_file_no;
  line_no = tok->display_line_no;
}
static void place_reg_arg(Type *ty, char *ofs, char *ptr, int *gp, int *fp) {
  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    if (has_flonum1(ty))
      fprintf(stdout, "", ofs, ptr, (*fp)++);
    else
      fprintf(stdout, "", ofs, ptr, argreg64[(*gp)++]);
    if (ty->size > 8) {
      if (has_flonum2(ty))
        fprintf(stdout, "", ofs, ptr, (*fp)++);
      else
        fprintf(stdout, "", ofs, ptr, argreg64[(*gp)++]);
    }
    return;
  case TY_FLOAT:
    fprintf(stdout, "", ofs, ptr, (*fp)++);
    return;
  case TY_DOUBLE:
    fprintf(stdout, "", ofs, ptr, (*fp)++);
    return;
  }
  if (ty->size <= 4)
    load_extend_int(ty, ofs, ptr, argreg32[(*gp)++]);
  else
    load_extend_int(ty, ofs, ptr, argreg64[(*gp)++]);
}
static void gen_funcall_args(Node *node) {
  for (Obj *var = node->args; var; var = var->param_next)
    if (var->ptr)
      gen_var_assign(var, var->arg_expr);
  bool rtn_by_stk = node->ret_buffer && node->ty->size > 16;
  int gp = rtn_by_stk, fp = 0;
  int reg_arg_cnt = 0;
  for (Obj *var = node->args; var; var = var->param_next) {
    if (var->pass_by_stack)
      continue;
    char ofs[STRBUF_SZ], *ptr;
    if (opt_optimize) {
      Node *arg_expr = var->arg_expr;
      reg_arg_cnt++;
      int64_t val;
      if (is_gp_ty(arg_expr->ty) && is_const_expr(arg_expr, &val)) {
        load_val2(arg_expr->ty, val, argreg32[gp], argreg64[gp]);
        gp++;
        continue;
      }
      long double fval;
      if (is_flonum(arg_expr->ty) && is_const_double(arg_expr, &fval)) {
        load_f32_f64(arg_expr->ty, fval, fp++);
        continue;
      }
      if (gen_load_opt_gp(arg_expr, (gp < 6 ? argreg[gp] : REG_NULL))) {
        gp++;
        continue;
      }
      if (is_memop(arg_expr, ofs, &ptr, true)) {
        place_reg_arg(arg_expr->ty, ofs, ptr, &gp, &fp);
        continue;
      }
      if (reg_arg_cnt == 1) {
        gen_expr(arg_expr);
        if (is_gp_ty(arg_expr->ty))
          fprintf(stdout, "", argreg64[gp++]);
        else if (is_flonum(arg_expr->ty))
          fp++;
        else
          place_reg_arg(arg_expr->ty, "0", "%rax", &gp, &fp);
        continue;
      }
    }
    snprintf(ofs, STRBUF_SZ, "%d", var->ofs);
    place_reg_arg(var->ty, ofs, var->ptr, &gp, &fp);
  }
  if (rtn_by_stk)
    fprintf(stdout, "", node->ret_buffer->ofs, node->ret_buffer->ptr);
}
static void gen_funcall(Node *node) {
  if (node->lhs->kind == ND_VAR && !strcmp(asm_name(node->lhs->var), "alloca")) {
    gen_expr(node->args->arg_expr);
    builtin_alloca(node);
    return;
  }
  bool rtn_by_stk = node->ret_buffer && node->ty->size > 16;
  int gp_count = rtn_by_stk;
  int fp_count = 0;
  int arg_stk_align;
  int arg_stk_size = calling_convention(node->args, &gp_count, &fp_count, &arg_stk_align);
  if (arg_stk_size)
    if (arg_stk_align > 16)
      push_from("%rsp");
  bool use_fn_ptr = !(node->lhs->kind == ND_VAR && node->lhs->var->ty->kind == TY_FUNC);
  if (use_fn_ptr) {
    gen_expr(node->lhs);
    push();
  }
  if (arg_stk_size) {
    if (arg_stk_align > 16) {
      fprintf(stdout, "", arg_stk_size);
      fprintf(stdout, "", arg_stk_align);
    } else {
      fprintf(stdout, "", align_to(arg_stk_size, 16));
    }
  }
  gen_funcall_args(node);
  if (node->lhs->ty->is_variadic) {
    if (fp_count)
      fprintf(stdout, "", fp_count);
    else
      fprintf(stdout, "\t\n");
  }
  if (use_fn_ptr) {
    switch (gp_count) {
    case 0: break;
    case 1: clobber_gp(1); break;
    default: clobber_gp(4); break;
    }
    fprintf(stdout, "", pop_inreg("%r10"));
  } else {
    if (use_rip(node->lhs->var))
      fprintf(stdout, "", asm_name(node->lhs->var));
    else
      fprintf(stdout, "", asm_name(node->lhs->var));
  }
  clobber_all_regs();
  if (arg_stk_size) {
    if (arg_stk_align > 16)
      pop("%rsp");
    else
      fprintf(stdout, "", align_to(arg_stk_size, 16));
  }
}
static void gen_cond(Node *node, bool jump_cond, char *jump_label) {
  if (opt_optimize) {
    bool flip;
    Node *expr = bool_expr_opt(node, &flip);
    if (expr) {
      node = expr;
      jump_cond ^= flip;
    }
    int64_t val;
    if (is_const_expr(node, &val)) {
      if (val == jump_cond)
        fprintf(stdout, "", jump_label);
      return;
    }
    if (is_cmp(node) && is_gp_ty(node->lhs->ty)) {
      NodeKind kind = node->kind;
      if (!gen_cmp_opt_gp(node, &kind)) {
        gen_expr(node->lhs);
        push();
        gen_expr(node->rhs);
        bool is_r64 = node->lhs->ty->size == 8;
        char *op = pop_inreg2(is_r64, (is_r64 ? tmpreg64 : tmpreg32)[0]);
        fprintf(stdout, "", regop_ax(node->lhs->ty), op);
      }
      flip_cmp(&kind, !jump_cond);
      char *ins = cmp_cc(kind, node->lhs->ty->is_unsigned);
      fprintf(stdout, "", ins, jump_label);
      return;
    }
    if (node->kind == ND_LOGAND || node->kind == ND_LOGOR) {
      bool short_cond = (node->kind == ND_LOGOR);
      if (short_cond == jump_cond) {
        gen_cond(node->lhs, jump_cond, jump_label);
        gen_cond(node->rhs, jump_cond, jump_label);
        return;
      }
      char short_label[STRBUF_SZ];
      snprintf(short_label, STRBUF_SZ, ".L.short.%"PRIi64, count());
      gen_cond(node->lhs, short_cond, short_label);
      gen_cond(node->rhs, short_cond, short_label);
      fprintf(stdout, "", jump_label);
      printf("", short_label);
      return;
    }
    if (node->kind == ND_CAST && node->ty->kind == TY_BOOL) {
      Node zero = {.kind = ND_NUM, .ty = node->lhs->ty, .tok = node->tok};
      Node expr = {.kind = ND_NE, .lhs = node->lhs, .rhs = &zero, .tok = node->tok};
      add_type(&expr);
      gen_cond(&expr, jump_cond, jump_label);
      return;
    }
  }
  if (node->kind == ND_FUNCALL && node->ty->kind == TY_BOOL)
    gen_funcall(node);
  else
    gen_expr(node);
  fprintf(stdout, "\t\n");
  fprintf(stdout, "", (jump_cond ? "ne" : "e"), jump_label);
}
static void gen_logical(Node *node, bool flip) {
  int64_t c = count();
  char short_label[STRBUF_SZ];
  snprintf(short_label, STRBUF_SZ, ".L.short.%"PRIi64, c);
  bool short_cond = (node->kind == ND_LOGOR);
  gen_cond(node->lhs, short_cond, short_label);
  gen_cond(node->rhs, short_cond, short_label);
  static char *set_zero = "xor %eax, %eax";
  static char *set_one = "movl $1, %eax";
  short_cond ^= flip;
  fprintf(stdout, "", short_cond ? set_zero : set_one);
  fprintf(stdout, "", c);
  printf("", short_label);
  fprintf(stdout, "", short_cond ? set_one : set_zero);
  printf("", c);
}
static void gen_expr2(Node *node, bool is_void) {
  if (opt_g)
    print_loc(node->tok);
  if (opt_optimize && gen_expr_opt(node))
    return;
  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
  case ND_NUM: {
    if (is_flonum(node->ty)) {
      load_fval(node->ty, node->fval);
      return;
    }
    load_val(node->ty, node->val);
    return;
  }
  case ND_POS:
    gen_expr(node->lhs);
    return;
  case ND_NEG:
    gen_expr(node->lhs);
    switch (node->ty->kind) {
    case TY_FLOAT:
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      return;
    case TY_DOUBLE:
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      return;
    case TY_LDOUBLE:
      fprintf(stdout, "\t\n");
      return;
    }
    fprintf(stdout, "", regop_ax(node->lhs->ty));
    return;
  case ND_VAR:
    gen_addr(node);
    load(node, 0);
    return;
  case ND_MEMBER: {
    gen_addr(node);
    load(node, 0);
    return;
  }
  case ND_DEREF:
    gen_expr(node->lhs);
    load(node, 0);
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    push();
    gen_expr(node->rhs);
    store(node->lhs, is_void);
    return;
  case ND_ARITH_ASSIGN:
    gen_addr(node->lhs);
    push();
    load(node->lhs, 0);
    gen_expr_null_lhs(node->arith_kind, node->lhs->ty, node->rhs);
    store(node->lhs, is_void);
    return;
  case ND_POST_INCDEC:
    gen_addr(node->lhs);
    fprintf(stdout, "\t\n");
    load(node->lhs, 0);
    push_by_ty(node->lhs->ty);
    push_from("%rcx");
    gen_expr_null_lhs(ND_ADD, node->lhs->ty, node->rhs);
    store(node->lhs, true);
    pop_by_ty(node->lhs->ty);
    return;
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next) {
      if (!n->next && n->kind == ND_EXPR_STMT)
        gen_expr(n->lhs);
      else
        gen_stmt(n);
    }
    if (has_defr(node)) {
      push_by_ty(node->ty);
      gen_defr(node);
      pop_by_ty(node->ty);
    }
    return;
  case ND_CHAIN:
  case ND_COMMA:
    gen_void_expr(node->lhs);
    gen_expr(node->rhs);
    return;
  case ND_CAST:
    gen_cast(node);
    return;
  case ND_INIT_AGG:
    gen_mem_zero(node->var->ofs, node->var->ptr, node->var->ty->size);
    for (Node *n = node->lhs; n; n = n->next)
      gen_void_assign(n);
    return;
  case ND_COND: {
    int64_t c = count();
    char else_label[STRBUF_SZ];
    snprintf(else_label, STRBUF_SZ, ".L.else.%"PRIi64, c);
    gen_cond(node->cond, false, else_label);
    gen_expr(node->then);
    fprintf(stdout, "", c);
    printf("", else_label);
    gen_expr(node->els);
    printf("", c);
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);
    fprintf(stdout, "\t\n");
    return;
  case ND_BITNOT:
    gen_expr(node->lhs);
    fprintf(stdout, "\t\n");
    return;
  case ND_LOGAND:
  case ND_LOGOR:
    gen_logical(node, false);
    return;
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
    gen_expr(node->lhs);
    push();
    gen_expr(node->rhs);
    fprintf(stdout, "\t\n");
    char *ax = regop_ax(node->ty);
    pop2((node->ty->size == 8), ax);
    switch (node->kind) {
    case ND_SHL: fprintf(stdout, "", ax); break;
    case ND_SHR: fprintf(stdout, "", ax); break;
    case ND_SAR: fprintf(stdout, "", ax); break;
    }
    return;
  case ND_FUNCALL:
    gen_funcall(node);
    if (!is_void) {
      if (is_integer(node->ty) && node->ty->size < 4) {
        cast_extend_int32(node->ty, reg_ax(node->ty->size), "%eax");
        return;
      }
      if (node->ret_buffer && node->ty->size <= 16) {
        copy_ret_buffer(node->ret_buffer);
        fprintf(stdout, "", node->ret_buffer->ofs, node->ret_buffer->ptr);
      }
    }
    return;
  case ND_LABEL_VAL:
    fprintf(stdout, "", node->unique_label);
    return;
  case ND_CAS: {
    gen_expr(node->cas_addr);
    push();
    gen_expr(node->cas_old);
    push();
    gen_expr(node->cas_new);
    char *old = pop_inreg(tmpreg64[0]);
    char *addr = pop_inreg(tmpreg64[1]);
    Type *ty = node->cas_addr->ty->base;
    char *ax = reg_ax(ty->size);
    char *dx = reg_dx(ty->size);
    if (!is_scalar(ty) || ty->kind == TY_LDOUBLE)
      error_tok(node->tok, "unsupported type for atomic CAS");
    switch (ty->kind) {
    case TY_DOUBLE: fprintf(stdout, "", dx); break;
    case TY_FLOAT: fprintf(stdout, "", dx); break;
    default: fprintf(stdout, "", ax, dx); break;
    }
    fprintf(stdout, "", old, ax);
    fprintf(stdout, "", dx, addr);
    fprintf(stdout, "\t\n");
    fprintf(stdout, "\t\n");
    fprintf(stdout, "", ax, old);
    ;
    fprintf(stdout, "\t\n");
    return;
  }
  case ND_EXCH: {
    gen_expr(node->lhs);
    push();
    gen_expr(node->rhs);
    char *reg = pop_inreg(tmpreg64[0]);
    int sz = node->lhs->ty->base->size;
    fprintf(stdout, "", reg_ax(sz), reg);
    return;
  }
  case ND_ALLOCA:
    gen_expr(node->lhs);
    builtin_alloca(node);
    return;
  case ND_VA_START: {
    gen_expr(node->lhs);
    fprintf(stdout, "", va_gp_start);
    fprintf(stdout, "", va_fp_start);
    fprintf(stdout, "", va_st_start);
    fprintf(stdout, "\t\n");
    fprintf(stdout, "", lvar_ptr);
    fprintf(stdout, "\t\n");
    return;
  }
  case ND_VA_COPY: {
    gen_expr(node->lhs);
    push();
    gen_expr(node->rhs);
    char *reg = pop_inreg(tmpreg64[0]);
    gen_mem_copy(0, "%rax", 0, reg, 24);
    return;
  }
  case ND_VA_ARG: {
    gen_expr(node->lhs);
    Type *ty = node->ty->base;
    if (ty->size <= 16 && ty->kind != TY_LDOUBLE) {
      if (va_arg_need_copy(ty)) {
        gen_vaarg_reg_copy(ty, node->var);
      } else if (has_flonum1(ty)) {
        fprintf(stdout, "", 160);
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
      } else {
        int gp_inc = ty->size > 8 ? 2 : 1;
        fprintf(stdout, "", 48 - gp_inc * 8);
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        fprintf(stdout, "", gp_inc * 8);
      }
      fprintf(stdout, "\t\n");
      ;
    }
    fprintf(stdout, "\t\n");
    if (ty->align <= 8) {
      fprintf(stdout, "", align_to(ty->size, 8));
    } else {
      fprintf(stdout, "", ty->align - 1);
      fprintf(stdout, "", ty->align);
      fprintf(stdout, "", align_to(ty->size, 8));
      fprintf(stdout, "\t\n");
    }
    if (ty->size <= 16)
      ;
    fprintf(stdout, "\t\n");
    return;
  }
  }
  switch (node->lhs->ty->kind) {
  case TY_FLOAT:
  case TY_DOUBLE: {
    gen_expr(node->lhs);
    pushf();
    gen_expr(node->rhs);
    bool is_xmm64 = node->lhs->ty->kind == TY_DOUBLE;
    int reg = popf_inreg(is_xmm64, 1);
    char *sz = is_xmm64 ? "sd" : "ss";
    switch (node->kind) {
    case ND_ADD:
      fprintf(stdout, "", sz, reg);
      return;
    case ND_SUB:
      fprintf(stdout, "", sz, reg);
      fprintf(stdout, "", reg);
      return;
    case ND_MUL:
      fprintf(stdout, "", sz, reg);
      return;
    case ND_DIV:
      fprintf(stdout, "", sz, reg);
      fprintf(stdout, "", reg);
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GT:
    case ND_GE:
      if (node->kind == ND_GT || node->kind == ND_GE)
        fprintf(stdout, "", sz, reg);
      else
        fprintf(stdout, "", sz, reg);
      if (node->kind == ND_EQ) {
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
      } else if (node->kind == ND_NE) {
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
      } else if (node->kind == ND_LT || node->kind == ND_GT) {
        fprintf(stdout, "\t\n");
      } else if (node->kind == ND_LE || node->kind == ND_GE) {
        fprintf(stdout, "\t\n");
      }
      fprintf(stdout, "\t\n");
      return;
    }
    error_tok(node->tok, "invalid expression");
  }
  case TY_LDOUBLE: {
    gen_expr(node->lhs);
    push_x87();
    gen_expr(node->rhs);
    pop_x87();
    switch (node->kind) {
    case ND_ADD:
      fprintf(stdout, "\t\n");
      return;
    case ND_SUB:
      fprintf(stdout, "\t\n");
      return;
    case ND_MUL:
      fprintf(stdout, "\t\n");
      return;
    case ND_DIV:
      fprintf(stdout, "\t\n");
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GT:
    case ND_GE:
      if (node->kind == ND_LT || node->kind == ND_LE)
        fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      fprintf(stdout, "\t\n");
      if (node->kind == ND_EQ) {
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
      } else if (node->kind == ND_NE) {
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
        fprintf(stdout, "\t\n");
      } else if (node->kind == ND_LT || node->kind == ND_GT) {
        fprintf(stdout, "\t\n");
      } else if (node->kind == ND_LE || node->kind == ND_GE) {
        fprintf(stdout, "\t\n");
      }
      fprintf(stdout, "\t\n");
      return;
    }
    error_tok(node->tok, "invalid expression");
  }
  }
  gen_expr(node->lhs);
  push();
  gen_expr(node->rhs);
  bool is_r64 = node->lhs->ty->size == 8 || node->lhs->ty->base;
  char *ax = is_r64 ? "%rax" : "%eax";
  char *op = pop_inreg2(is_r64, (is_r64 ? tmpreg64 : tmpreg32)[0]);
  switch (node->kind) {
  case ND_ADD:
    fprintf(stdout, "", op, ax);
    return;
  case ND_SUB:
    fprintf(stdout, "", ax, op);
    fprintf(stdout, "", op, ax);
    return;
  case ND_MUL:
    fprintf(stdout, "", op, ax);
    return;
  case ND_DIV:
  case ND_MOD:
    fprintf(stdout, "", op, ax);
    if (node->ty->is_unsigned) {
      fprintf(stdout, "\t\n");
      fprintf(stdout, "", op);
    } else {
      if (node->lhs->ty->size == 8)
        fprintf(stdout, "\t\n");
      else
        fprintf(stdout, "\t\n");
      fprintf(stdout, "", op);
    }
    if (node->kind == ND_MOD)
      fprintf(stdout, "\t\n");
    return;
  case ND_BITAND:
    fprintf(stdout, "", op, ax);
    return;
  case ND_BITOR:
    fprintf(stdout, "", op, ax);
    return;
  case ND_BITXOR:
    fprintf(stdout, "", op, ax);
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
    fprintf(stdout, "", ax, op);
    gen_cmp_setcc(node->kind, node->lhs->ty->is_unsigned);
    return;
  }
  error_tok(node->tok, "invalid expression");
}
static void gen_expr(Node *node) {
  gen_expr2(node, false);
  return;
}
static void gen_stmt(Node *node) {
  if (opt_g)
    print_loc(node->tok);
  switch (node->kind) {
  case ND_NULL_STMT:
    return;
  case ND_IF: {
    int64_t c = count();
    char else_label[STRBUF_SZ];
    snprintf(else_label, STRBUF_SZ, ".L.else.%"PRIi64, c);
    gen_cond(node->cond, false, else_label);
    gen_stmt(node->then);
    if (!node->els) {
      printf("", else_label);
      return;
    }
    fprintf(stdout, "", c);
    printf("", else_label);
    gen_stmt(node->els);
    printf("", c);
    return;
  }
  case ND_FOR: {
    int64_t c = count();
    if (node->init)
      gen_stmt(node->init);
    printf("", c);
    if (node->cond)
      gen_cond(node->cond, false, node->brk_label);
    gen_stmt(node->then);
    printf("", node->cont_label);
    if (node->inc)
      gen_void_expr(node->inc);
    fprintf(stdout, "", c);
    printf("", node->brk_label);
    gen_defr(node);
    return;
  }
  case ND_DO: {
    char begin_label[STRBUF_SZ];
    snprintf(begin_label, STRBUF_SZ, ".L.begin.%"PRIi64, count());
    printf("", begin_label);
    gen_stmt(node->then);
    printf("", node->cont_label);
    gen_cond(node->cond, true, begin_label);
    printf("", node->brk_label);
    return;
  }
  case ND_SWITCH: {
    gen_expr(node->cond);
    char *ax, *cx, *dx;
    if (node->cond->ty->size == 8)
      ax = "%rax", cx = "%rcx", dx = "%rdx";
    else
      ax = "%eax", cx = "%ecx", dx = "%edx";
    for (Node *n = node->case_next; n; n = n->case_next) {
      if (n->end == n->begin) {
        imm_cmp(ax, dx, n->begin);
        fprintf(stdout, "", n->label);
        continue;
      }
      if (n->begin == 0) {
        imm_cmp(ax, dx, n->end);
        fprintf(stdout, "", n->label);
        continue;
      }
      fprintf(stdout, "", ax, cx);
      imm_sub(cx, dx, n->begin);
      imm_cmp(cx, dx, n->end - n->begin);
      fprintf(stdout, "", n->label);
    }
    if (node->default_case)
      fprintf(stdout, "", node->default_case->label);
    fprintf(stdout, "", node->brk_label);
    gen_stmt(node->then);
    printf("", node->brk_label);
    return;
  }
  case ND_CASE:
    printf("", node->label);
    if (node->lhs)
      gen_stmt(node->lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    gen_defr(node);
    return;
  case ND_GOTO:
    gen_defr(node);
    fprintf(stdout, "", node->unique_label);
    return;
  case ND_GOTO_EXPR:
    gen_expr(node->lhs);
    fprintf(stdout, "\t\n");
    return;
  case ND_LABEL:
    printf("", node->unique_label);
    if (node->lhs)
      gen_stmt(node->lhs);
    return;
  case ND_RETURN: {
    if (!node->lhs) {
      if (has_defr(node))
        gen_defr(node);
      fprintf(stdout, "", rtn_label);
      return;
    }
    gen_expr(node->lhs);
    Type *ty = node->lhs->ty;
    if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
      if (ty->size <= 16) {
        if (has_defr(node)) {
          push_copy(ty->size);
          gen_defr(node);
          pop_copy(ty->size, "%rax");
        }
        copy_struct_reg();
        fprintf(stdout, "", rtn_label);
        return;
      }
      copy_struct_mem();
      if (has_defr(node)) {
        push();
        gen_defr(node);
        pop("%rax");
      }
      fprintf(stdout, "", rtn_label);
      return;
    }
    if (has_defr(node)) {
      push_by_ty(ty);
      gen_defr(node);
      pop_by_ty(ty);
    }
    fprintf(stdout, "", rtn_label);
    return;
  }
  case ND_EXPR_STMT:
    gen_void_expr(node->lhs);
    return;
  }
  error_tok(node->tok, "invalid statement");
}
static void imm_tmpl(char *ins, char *op, int64_t val) {
  if (val == (int32_t)val) {
    fprintf(stdout, "", ins, val, op);
    return;
  }
  if (val == (uint32_t)val)
    fprintf(stdout, "", val);
  else
    fprintf(stdout, "", val);
  fprintf(stdout, "", ins, op);
  return;
}
static void memop_arith(Node *lhs, Node *rhs, char *ins) {
  char ins_sz[STRBUF_SZ];
  snprintf(ins_sz, STRBUF_SZ, "%s%s", ins, size_suffix(lhs->ty->size));
  int64_t rval;
  char ofs[STRBUF_SZ], *ptr;
  if (is_const_expr(rhs, &rval)) {
    if (is_memop(lhs, ofs, &ptr, false)) {
      char memop[STRBUF_SZ2];
      snprintf(memop, STRBUF_SZ2, "%s(%s)", ofs, ptr);
      imm_tmpl(ins_sz, memop, limit_imm(rval, lhs->ty->size));
      return;
    }
    gen_addr(lhs);
    imm_tmpl(ins_sz, "(%rax)", limit_imm(rval, lhs->ty->size));
    return;
  }
  if (is_memop(lhs, ofs, &ptr, false)) {
    gen_expr(rhs);
    fprintf(stdout, "", ins_sz, reg_ax(lhs->ty->size), ofs, ptr);
    return;
  }
  gen_addr(lhs);
  push();
  gen_expr(rhs);
  char *dptr = pop_inreg(tmpreg64[0]);
  fprintf(stdout, "", ins_sz, reg_ax(lhs->ty->size), dptr);
}
static void gen_void_arith_assign(Node *node) {
  if (node->kind == ND_POST_INCDEC) {
    node->kind = ND_ARITH_ASSIGN;
    node->arith_kind = ND_ADD;
  }
  Node *lhs = node->lhs;
  Node *rhs = node->rhs;
  add_type(rhs);
  if (opt_optimize && is_gp_ty(lhs->ty) && !is_bitfield(lhs) &&
    lhs->ty->kind != TY_BOOL && is_integer(rhs->ty)) {
    Node null = {.kind = ND_NULL_EXPR, .ty = lhs->ty, .tok = rhs->tok};
    Node expr = {.kind = node->arith_kind, .lhs = &null, .rhs = rhs, .tok = rhs->tok};
    add_type(&expr);
    switch (node->arith_kind) {
    case ND_ADD:
    case ND_SUB:
    case ND_BITAND:
    case ND_BITOR:
    case ND_BITXOR:
      memop_arith(lhs, expr.rhs, arith_ins(node->arith_kind));
      return;
    }
  }
  gen_expr2(node, true);
}
static void gen_void_assign(Node *node) {
  Node *lhs = node->lhs;
  Node *rhs = node->rhs;
  if (is_gp_ty(lhs->ty) && !is_bitfield(lhs) && !lhs->ty->is_atomic &&
    is_const_expr(rhs, NULL)) {
    memop_arith(lhs, rhs, "mov");
    return;
  }
  char sofs[STRBUF_SZ], *sptr;
  if (is_memop(rhs, sofs, &sptr, true) ||
    (is_int_to_int_cast(rhs) && lhs->ty->size <= rhs->lhs->ty->size &&
    lhs->ty->kind != TY_BOOL && is_memop(rhs->lhs, sofs, &sptr, true))) {
    char dofs[STRBUF_SZ], *dptr;
    if (is_memop(lhs, dofs, &dptr, false)) {
      gen_mem_copy2(sofs, sptr, dofs, dptr, write_size(lhs));
      return;
    }
    if (!is_bitfield(lhs) && !lhs->ty->is_atomic) {
      gen_addr(lhs);
      gen_mem_copy2(sofs, sptr, "0", "%rax", write_size(lhs));
      return;
    }
  }
  if (is_memop_ptr(rhs, sofs, &sptr) && !(opt_fpic || opt_fpie) && !strcmp(sptr, "%rip")) {
    char dofs[STRBUF_SZ], *dptr;
    if (is_memop(lhs, dofs, &dptr, false)) {
      fprintf(stdout, "", sofs, dofs, dptr);
      return;
    }
    if (!is_bitfield(lhs) && !lhs->ty->is_atomic) {
      gen_addr(lhs);
      fprintf(stdout, "", sofs);
      return;
    }
  }
  gen_expr2(node, true);
}
static void gen_void_expr(Node *node) {
  switch (node->kind) {
  case ND_NULL_EXPR:
  case ND_LABEL_VAL:
  case ND_VAR:
  case ND_NUM:
    return;
  case ND_POS:
  case ND_NEG:
  case ND_MEMBER:
  case ND_ADDR:
  case ND_DEREF:
  case ND_NOT:
  case ND_BITNOT:
  case ND_CAST:
    gen_void_expr(node->lhs);
    return;
  case ND_ADD:
  case ND_SUB:
  case ND_MUL:
  case ND_DIV:
  case ND_MOD:
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
  case ND_CHAIN:
  case ND_COMMA:
    gen_void_expr(node->lhs);
    gen_void_expr(node->rhs);
    return;
  case ND_ASSIGN:
    gen_void_assign(node);
    return;
  case ND_ARITH_ASSIGN:
  case ND_POST_INCDEC:
    gen_void_arith_assign(node);
    return;
  }
  gen_expr2(node, true);
}
static char *arith_ins(NodeKind kind) {
  char *ins;
  switch (kind) {
  case ND_ADD: ins = "add"; break;
  case ND_SUB: ins = "sub"; break;
  case ND_MUL: ins = "imul"; break;
  case ND_BITAND: ins = "and"; break;
  case ND_BITOR:ins = "or"; break;
  case ND_BITXOR: ins = "xor"; break;
  case ND_SHL: ins = "shl"; break;
  case ND_SHR: ins = "shr"; break;
  case ND_SAR: ins = "sar"; break;
  default: internal_error();
  }
  return ins;
}
static void imm_arith2(NodeKind kind, char *op, char *tmp, int64_t val) {
  if (in_imm_range(val)) {
    fprintf(stdout, "", arith_ins(kind), val, op);
    return;
  }
  fprintf(stdout, "", val, tmp);
  fprintf(stdout, "", arith_ins(kind), tmp, op);
  return;
}
static void imm_add(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0: return;
  case 1: fprintf(stdout, "", op); return;
  case -1: fprintf(stdout, "", op); return;
  }
  imm_arith2(ND_ADD, op, tmp, val);
}
static void imm_sub(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0: return;
  case 1: fprintf(stdout, "", op); return;
  case -1: fprintf(stdout, "", op); return;
  }
  imm_arith2(ND_SUB, op, tmp, val);
}
static void imm_and(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0: fprintf(stdout, "", op, op); return;
  case -1: return;
  }
  imm_arith2(ND_BITAND, op, tmp, val);
}
static void imm_cmp(char *op, char *tmp, int64_t val) {
  if (val == 0) {
    fprintf(stdout, "", op, op);
    return;
  }
  if (in_imm_range(val)) {
    fprintf(stdout, "", val, op);
    return;
  }
  fprintf(stdout, "", val, tmp);
  fprintf(stdout, "", tmp, op);
}
static void imm_arith(NodeKind kind, int sz, int64_t val) {
  char *ax = reg_ax(sz);
  char *dx = reg_dx(sz);
  switch (kind) {
  case ND_ADD: imm_add(ax, dx, val); return;
  case ND_SUB: imm_sub(ax, dx, val); return;
  case ND_BITAND: imm_and(ax, dx, val); return;
  }
  if (val == 0) {
    switch (kind) {
    case ND_BITOR:
    case ND_BITXOR:
    case ND_SHL:
    case ND_SHR:
    case ND_SAR:
      return;
    case ND_MUL:
      fprintf(stdout, "\t\n");
      return;
    }
  }
  if (val == 1)
    if (kind == ND_MUL)
      return;
  if (val == -1) {
    switch (kind) {
    case ND_MUL:
      fprintf(stdout, "", ax);
      return;
    case ND_BITOR:
      fprintf(stdout, "", ax);
      return;
    case ND_BITXOR:
      fprintf(stdout, "", ax);
      return;
    }
  }
  if (kind == ND_MUL && is_pow_of_two(val)) {
    for (int i = 1; i < sz * 8; i++) {
      if (1LL << i == val) {
        fprintf(stdout, "", i, ax);
        return;
      }
    }
  }
  imm_arith2(kind, ax, dx, val);
  return;
}
static bool divmod_opt(NodeKind kind, Type *ty, Node *expr, int64_t val) {
  char *ax = reg_ax(ty->size);
  char *dx = reg_dx(ty->size);
  if (val == 1) {
    gen_expr(expr);
    if (kind == ND_MOD)
      fprintf(stdout, "\t\n");
    return true;
  }
  if (val == -1) {
    gen_expr(expr);
    if (!ty->is_unsigned) {
      if (kind == ND_DIV)
        fprintf(stdout, "", ax);
      else
        fprintf(stdout, "\t\n");
      return true;
    }
    if (kind == ND_DIV) {
      fprintf(stdout, "", ax);
      gen_cmp_setcc(ND_EQ, false);
      return true;
    }
    fprintf(stdout, "\t\n");
    fprintf(stdout, "", ax);
    fprintf(stdout, "", dx, ax);
    return true;
  }
  if (kind == ND_DIV && is_pow_of_two(val) && ty->is_unsigned) {
    for (int i = 1; i < ty->size * 8; i++) {
      if (1LL << i == val) {
        gen_expr(expr);
        fprintf(stdout, "", i, ax);
        return true;
      }
    }
  }
  if (kind == ND_MOD && is_pow_of_two(val) && ty->is_unsigned && val != 0) {
    gen_expr(expr);
    uint64_t msk = val - 1;
    if (msk == UINT32_MAX) {
      fprintf(stdout, "\t\n");
      return true;
    }
    if (msk <= INT32_MAX) {
      fprintf(stdout, "", (int)msk);
      return true;
    }
    imm_and(ax, dx, msk);
    return true;
  }
  return false;
}
static bool gen_cmp_opt_gp2(Node *lhs, Node *rhs) {
  char ofs[STRBUF_SZ], *ptr;
  int64_t val;
  if (is_const_expr(rhs, &val)) {
    if (is_memop(lhs, ofs, &ptr, false)) {
      char memop[STRBUF_SZ2];
      snprintf(memop, STRBUF_SZ2, "%s(%s)", ofs, ptr);
      imm_tmpl(lhs->ty->size == 8 ? "cmpq" : "cmpl", memop, limit_imm(val, lhs->ty->size));
      return true;
    }
    gen_expr(lhs);
    imm_cmp(regop_ax(lhs->ty), "%rdx", limit_imm(val, lhs->ty->size));
    return true;
  }
  if (is_memop(lhs, ofs, &ptr, false)) {
    gen_expr(rhs);
    fprintf(stdout, "", regop_ax(lhs->ty), ofs, ptr);
    return true;
  }
  return false;
}
static bool gen_cmp_opt_gp(Node *node, NodeKind *kind) {
  if (gen_cmp_opt_gp2(node->lhs, node->rhs))
    return true;
  if (gen_cmp_opt_gp2(node->rhs, node->lhs)) {
    switch (*kind) {
    case ND_LT: *kind = ND_GT; break;
    case ND_LE: *kind = ND_GE; break;
    case ND_GT: *kind = ND_LT; break;
    case ND_GE: *kind = ND_LE; break;
    }
    return true;
  }
  return false;
}
static bool gen_arith_opt_gp2(NodeKind kind, int sz, Node *lhs, Node *rhs, int ctrl, bool swap) {
  int64_t val;
  char ofs[STRBUF_SZ], *ptr;
  char *ax = reg_ax(sz);
  switch (abs(ctrl)) {
  case 1:
    if (is_const_expr(rhs, &val)) {
      gen_expr(lhs);
      imm_arith(kind, sz, limit_imm(val, sz));
      return true;
    }
    break;
  case 2:
    if (is_memop(rhs, ofs, &ptr, true)) {
      gen_expr(lhs);
      fprintf(stdout, "", arith_ins(kind), ofs, ptr, ax);
      return true;
    }
    break;
  case 3:
    if (is_int_to_int_cast(rhs) && is_memop(rhs->lhs, ofs, &ptr, true) &&
      sz <= rhs->lhs->ty->size) {
      gen_expr(lhs);
      fprintf(stdout, "", arith_ins(kind), ofs, ptr, ax);
      return true;
    }
    break;
  case 4:
    if (is_int_to_int_cast(rhs) && is_memop(rhs->lhs, ofs, &ptr, true) &&
      sz > rhs->lhs->ty->size) {
      gen_expr(lhs);
      if (!rhs->lhs->ty->is_unsigned && sz == 8)
        load_extend_int64(rhs->lhs->ty, ofs, ptr, "%rdx");
      else
        load_extend_int(rhs->lhs->ty, ofs, ptr, "%edx");
      fprintf(stdout, "", arith_ins(kind), reg_dx(sz), ax);
      return true;
    }
    break;
  }
  if (swap && gen_arith_opt_gp2(kind, sz, rhs, lhs, -ctrl, false))
    return true;
  if (ctrl > 0 && ctrl < 4 && gen_arith_opt_gp2(kind, sz, lhs, rhs, ctrl + 1, swap))
    return true;
  return false;
}
static bool gen_arith_opt_gp(Node *node, int sz) {
  switch (node->kind) {
  case ND_ADD:
  case ND_MUL:
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
    if (gen_arith_opt_gp2(node->kind, sz, node->lhs, node->rhs, 1, true))
      return true;
  case ND_SUB:
    if (gen_arith_opt_gp2(node->kind, sz, node->lhs, node->rhs, 1, false))
      return true;
  }
  return false;
}
static bool gen_shift_opt_gp(Node *node) {
  char *ax = reg_ax(node->ty->size);
  int64_t val;
  char ofs[STRBUF_SZ], *ptr;
  if (is_const_expr(node->rhs, &val)) {
    gen_expr(node->lhs);
    imm_arith(node->kind, node->ty->size, val);
    return true;
  }
  if (is_memop(node->rhs, ofs, &ptr, true)) {
    gen_expr(node->lhs);
    fprintf(stdout, "", ofs, ptr);
    fprintf(stdout, "", arith_ins(node->kind), ax);
    return true;
  }
  return false;
}
static bool gen_gp_opt(Node *node) {
  NodeKind kind = node->kind;
  Node *lhs = node->lhs;
  Node *rhs = node->rhs;
  Type *ty = node->ty;
  switch (kind) {
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
    return gen_shift_opt_gp(node);
  case ND_DIV:
  case ND_MOD:
    if (rhs->kind == ND_NUM)
      return divmod_opt(kind, ty, lhs, rhs->val);
    return false;
  }
  if (gen_arith_opt_gp(node, node->ty->size))
    return true;
  if (is_cmp(node) && is_gp_ty(node->lhs->ty) && gen_cmp_opt_gp(node, &kind)) {
    gen_cmp_setcc(kind, lhs->ty->is_unsigned);
    return true;
  }
  return false;
}
static bool gen_load_opt_gp(Node *node, Reg r) {
  char ofs[STRBUF_SZ], *ptr;
  Node *lhs = node->lhs;
  Type *ty = node->ty;
  bool gen = (r != REG_NULL);
  if (is_memop_ptr(node, ofs, &ptr)) {
    if (gen) {
      if (!(opt_fpic || opt_fpie) && !strcmp(ptr, "%rip"))
        fprintf(stdout, "", ofs, regs[r][2]);
      else
        fprintf(stdout, "", ofs, ptr, regs[r][3]);
    }
    return true;
  }
  if (is_int_to_int_cast(node) && is_memop(lhs, ofs, &ptr, true)) {
    if (ty->size > lhs->ty->size) {
      if (!lhs->ty->is_unsigned && ty->size == 8) {
        if (gen)
          load_extend_int64(lhs->ty, ofs, ptr, regs[r][3]);
        return true;
      }
      if (!(ty->size == 2 && ty->is_unsigned && !lhs->ty->is_unsigned)) {
        if (gen)
          load_extend_int(lhs->ty, ofs, ptr, regs[r][2]);
        return true;
      }
    } else if (ty->kind != TY_BOOL) {
      if (gen)
        load_extend_int(ty, ofs, ptr, regs[r][ty->size == 8 ? 3 : 2]);
      return true;
    }
  }
  return false;
}
static void gen_deref_opt(Node *node, int64_t *ofs) {
  for (;; node = node->lhs) {
    if (node->kind == ND_CAST && node->ty->base)
      continue;
    if (node->kind == ND_DEREF && node->ty->kind == TY_ARRAY)
      continue;
    if (node->kind == ND_ADD && node->rhs->kind == ND_NUM) {
      *ofs += node->rhs->val;
      continue;
    }
    if (node->kind == ND_SUB && node->rhs->kind == ND_NUM) {
      *ofs -= node->rhs->val;
      continue;
    }
    break;
  }
  gen_expr(node);
}
static void gen_member_opt(Node *node, int64_t *ofs) {
  while (node->kind == ND_MEMBER) {
    *ofs += node->member->offset;
    node = node->lhs;
  }
  switch (node->kind) {
  case ND_FUNCALL:
  case ND_ASSIGN:
  case ND_COND:
  case ND_STMT_EXPR:
  case ND_VA_ARG:
    gen_expr(node);
    return;
  }
  gen_addr(node);
}
static Node *bool_expr_opt(Node *node, bool *flip) {
  Node *boolexpr = NULL;
  bool has_not = false;
  bool boolexpr_has_not;
  for (;;) {
    switch (node->kind) {
    case ND_NOT:
      has_not = !has_not;
      node = node->lhs;
      continue;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GT:
    case ND_GE:
    case ND_LOGOR:
    case ND_LOGAND:
      *flip = has_not;
      return node;
    }
    if (node->ty->kind == TY_BOOL) {
      boolexpr = node;
      boolexpr_has_not = has_not;
    }
    if (!(node->kind == ND_CAST && is_gp_ty(node->ty)))
      break;
    node = node->lhs;
  }
  if (boolexpr) {
    *flip = boolexpr_has_not;
    return boolexpr;
  }
  return NULL;
}
static bool gen_bool_opt(Node *node) {
  bool flip;
  Node *boolexpr = bool_expr_opt(node, &flip);
  if (!boolexpr || boolexpr == node)
    return false;
  node = boolexpr;
  if (is_cmp(node)) {
    Node n = *node;
    flip_cmp(&n.kind, flip);
    gen_expr(&n);
    return true;
  }
  if (node->kind == ND_LOGAND || node->kind == ND_LOGOR) {
    gen_logical(node, flip);
    return true;
  }
  if (node->kind == ND_CAST && node->ty->kind == TY_BOOL) {
    gen_cmp_zero(node->lhs, flip ? ND_EQ : ND_NE);
    return true;
  }
  gen_expr(node);
  if (flip)
    fprintf(stdout, "\t\n");
  return true;
}
static bool gen_scaled_idx_load(Type *ty, Node *lhs, Node *mul) {
  int64_t val;
  if (is_const_expr(mul, &val)) {
    int64_t ofs = 0;
    gen_deref_opt(lhs, &ofs);
    load2(ty, ofs + val, "%rax");
    return true;
  }
  int sz = 0;
  switch (mul->lhs->val) {
  case 1: sz = 1; break;
  case 2: sz = 2; break;
  case 4: sz = 4; break;
  case 8: sz = 8; break;
  default: {
    return false;
  }
  }
  char *ptr;
  char ofs[STRBUF_SZ];
  char op[STRBUF_SZ2];
  if (is_memop(lhs, ofs, &ptr, true)) {
    gen_expr(mul->rhs);
    fprintf(stdout, "", ofs, ptr);
    snprintf(op, STRBUF_SZ2, "(%%rdx, %%rax, %d)", sz);
  } else if (is_memop_ptr(lhs, ofs, &ptr)) {
    CRITICAL_BREAK_POINT;
  } else {
    gen_expr(lhs);
    push();
    gen_expr(mul->rhs);
    const char *reg = pop_inreg(tmpreg64[0]);
    snprintf(op, STRBUF_SZ2, "(%s, %%rax, %d)", reg, sz);
  }
  if (ty->size == 8)
    fprintf(stdout, "", op);
  else
    cast_extend_int32(ty, op, "%eax");
  return true;
}
static bool gen_expr_opt(Node *node) {
  NodeKind kind = node->kind;
  Type *ty = node->ty;
  Node *lhs = node->lhs;
  Node *rhs = node->rhs;
  char *var_ptr;
  char var_ofs[STRBUF_SZ];
  {
    int64_t ival;
    if (is_gp_ty(ty) && is_const_expr(node, &ival)) {
      load_val(ty, ival);
      return true;
    }
    long double fval;
    if (is_flonum(ty) && is_const_double(node, &fval)) {
      load_fval(ty, fval);
      return true;
    }
  }
  if (gen_load_opt_gp(node, REG_AX))
    return true;
  if (is_scalar(ty) && is_memop(node, var_ofs, &var_ptr, true)) {
    load3(ty, var_ofs, var_ptr);
    return true;
  }
  if (kind == ND_ASSIGN && is_memop(lhs, var_ofs, &var_ptr, false)) {
    gen_expr(rhs);
    store3(lhs->ty, var_ofs, var_ptr);
    return true;
  }
  if (gen_bool_opt(node))
    return true;
  if (is_gp_ty(ty)) {
    if (gen_gp_opt(node))
      return true;
    if (kind == ND_DEREF && lhs->kind == ND_ADD && lhs->rhs->kind == ND_MUL) {
      Node *mul = lhs->rhs;
      if (mul->lhs->kind == ND_NUM)
        if (gen_scaled_idx_load(ty, lhs->lhs, mul))
          return true;
    }
  }
  if (is_int_to_int_cast(node) && ty->size == 4 && lhs->ty->size == 8)
    if (gen_arith_opt_gp(lhs, ty->size))
      return true;
  if (kind == ND_COND && node->cond->kind == ND_NUM) {
    if (node->cond->val)
      gen_expr(node->then);
    else
      gen_expr(node->els);
    return true;
  }
  if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION ||
    is_bitfield(node)) {
    char var_ofs[STRBUF_SZ], *var_ptr;
    Node addr_node = {.kind = ND_ADDR, .lhs = node, .tok = node->tok};
    if (is_memop_ptr(&addr_node, var_ofs, &var_ptr)) {
      fprintf(stdout, "", var_ofs, var_ptr);
      if (is_bitfield(node))
        load(node, 0);
      return true;
    }
  }
  if (kind == ND_DEREF) {
    int64_t ofs = 0;
    gen_deref_opt(node->lhs, &ofs);
    if (is_scalar(ty)) {
      load2(ty, ofs, "%rax");
      return true;
    }
    imm_add("%rax", "%rdx", ofs);
    return true;
  }
  if (kind == ND_MEMBER) {
    int64_t ofs = 0;
    gen_member_opt(node, &ofs);
    if (is_scalar(ty)) {
      load(node, ofs);
      return true;
    }
    imm_add("%rax", "%rdx", ofs);
    return true;
  }
  return false;
}
static bool gen_addr_opt(Node *node) {
  return false;
}
void prepare_inline_asm(Node *node) {
}
static void store_gp2(char **reg, int sz, int dofs, char *dptr) {
}
void emit_text(Obj *fn) {
  current_fn = fn;
  gen_stmt(fn->body);
}
void prepare_funcall(Node *node, Scope *scope) {
}
int codegen(Obj *prog, FILE *out) {
  return 0;
}

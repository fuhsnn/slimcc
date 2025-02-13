#include "slimcc.h"

#define GP_MAX 6
#define FP_MAX 8

#define GP_SLOTS 6
#define FP_SLOTS 6

#define STRBUF_SZ 192
#define STRBUF_SZ2 208

typedef enum {
  REG_NULL = 0,
  REG_AX,
  REG_CX,
  REG_DX,
  REG_SI,
  REG_DI,
  REG_R8,
  REG_R9,
  REG_R10,
  REG_R11,
  REG_R12,
  REG_R13,
  REG_R14,
  REG_R15,
  REG_BX,
  REG_BP,
  REG_SP,
  REG_XMM0,
  REG_XMM1,
  REG_XMM2,
  REG_XMM3,
  REG_XMM4,
  REG_XMM5,
  REG_XMM6,
  REG_XMM7,
  REG_XMM8,
  REG_XMM9,
  REG_XMM10,
  REG_XMM11,
  REG_XMM12,
  REG_XMM13,
  REG_XMM14,
  REG_XMM15,
  REG_X87_ST0,
  REG_X87_ST1,
  REG_X87_ST2,
  REG_X87_ST3,
  REG_X87_ST4,
  REG_X87_ST5,
  REG_X87_ST6,
  REG_X87_ST7,
  REG_END
} Reg;

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

static FILE *output_file;
static char *argreg8[] = {"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
static char *argreg16[] = {"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"};
static char *argreg32[] = {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};
static char *argreg64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};

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
static char *rtn_label;

bool dont_reuse_stack;

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

typedef enum {
  SL_GP,
  SL_FP,
  SL_ST,
} SlotKind;

typedef struct {
  SlotKind kind;
  int gp_depth;
  int fp_depth;
  int st_depth;
  int st_ofs;
  char *push_reg;
  long loc;
} Slot;

static struct {
  Slot *data;
  int capacity;
  int depth;
} tmp_stack;

static void load2(Type *ty, int sofs, char *sptr);
static void store2(Type *ty, int dofs, char *dptr);
static void store_gp2(char **reg, int sz, int ofs, char *ptr);

static void gen_asm(Node *node);
static void gen_expr(Node *node);
static void gen_stmt(Node *node);
static void gen_void_expr(Node *node);
static void gen_void_assign(Node *node);
static bool gen_expr_opt(Node *node);
static bool gen_addr_opt(Node *node);
static bool gen_cmp_opt_gp(Node *node, NodeKind *kind);
static bool has_load_opt_gp(Node *node);
static bool gen_load_opt_gp(Node *node, char *reg32, char *reg64);
static Node *bool_expr_opt(Node *node, bool *flip);

static void imm_add(char *op, char *tmp, int64_t val);
static void imm_sub(char *op, char *tmp, int64_t val);
static void imm_and(char *op, char *tmp, int64_t val);
static void imm_cmp(char *op, char *tmp, int64_t val);
static char *arith_ins(NodeKind kind);

#define Prints(str) fprintf(output_file, str)
#define Printsts(str) fprintf(output_file, "\t" str)
#define Printssn(str) fprintf(output_file, str "\n")
#define Printstn(str) fprintf(output_file, "\t" str "\n")

#define Printf(str, ...) fprintf(output_file, str, __VA_ARGS__)
#define Printfts(str, ...) fprintf(output_file, "\t" str, __VA_ARGS__)
#define Printfsn(str, ...) fprintf(output_file, str "\n", __VA_ARGS__)
#define Printftn(str, ...) fprintf(output_file, "\t" str "\n", __VA_ARGS__)

static long resrvln(void) {
  long loc = ftell(output_file) + 1;
  fprintf(output_file, "\t                                                      \n");
  return loc;
}

FMTCHK(1,3)
static void insrtln(char *fmt, long loc, ...) {
  long cur_loc = ftell(output_file);
  fseek(output_file, loc, SEEK_SET);

  va_list ap;
  va_start(ap, loc);
  vfprintf(output_file, fmt, ap);
  va_end(ap);

  fseek(output_file, cur_loc, SEEK_SET);
}

static char *asm_name(Obj *var) {
  return var->asm_name ? var->asm_name : var->name;
}

static int count(void) {
  static int i = 1;
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

static Node *skip_gp_cast(Node *node) {
  while (node->kind == ND_CAST && node->ty->size == node->lhs->ty->size &&
    node->ty->kind != TY_BOOL && is_gp_ty(node->ty) && is_gp_ty(node->lhs->ty))
    node = node->lhs;

  return node;
}

static bool eval_memop(Node *node, char *ofs, char **ptr, bool let_subarray, bool let_atomic) {
  int offset;
  Obj *var = eval_var_opt(node, &offset, let_subarray, let_atomic);
  if (var) {
    if (var->is_local) {
      snprintf(ofs, STRBUF_SZ, "%d", offset + var->ofs);
      *ptr = var->ptr;
      return true;
    } else if (!var->is_tls && (!opt_fpic || var->is_static)) {
      snprintf(ofs, STRBUF_SZ, "%d+\"%s\"", offset, asm_name(var));
      *ptr = "%rip";
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

static int write_size(Node *node)  {
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

static char *size_suffix(int sz)  {
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

  long loc = resrvln();
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
    Printftn("mov %s, %s", pop_reg, dest_reg);
    return dest_reg;
  }
  insrtln("mov %s, %d(%s)", sl->loc, push_reg, sl->st_ofs, lvar_ptr);
  Printftn("mov %d(%s), %s", sl->st_ofs, lvar_ptr, dest_reg);
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
    Printftn("%s %%xmm%d, %%xmm%d", mv, pop_reg, dest_reg);
    return dest_reg;
  }
  insrtln("%s %%xmm0, %d(%s)", sl->loc, mv, sl->st_ofs, lvar_ptr);
  Printftn("%s %d(%s), %%xmm%d", mv, sl->st_ofs, lvar_ptr, dest_reg);
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
  Printftn("fldt %d(%s)", sl->st_ofs, lvar_ptr);
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
  Printftn("lea %d(%s), %s", pos, lvar_ptr, sptr);
}

// When we load a char or a short value to a register, we always
// extend them to the size of int, so we can assume the lower half of
// a register always contains a valid value.
static void load_extend_int(Type *ty, char *ofs, char *ptr, char *reg) {
  char *insn = ty->is_unsigned ? "movz" : "movs";
  switch (ty->size) {
  case 1: Printftn("%sbl %s(%s), %s", insn, ofs, ptr, reg); return;
  case 2: Printftn("%swl %s(%s), %s", insn, ofs, ptr, reg); return;
  case 4: Printftn("movl %s(%s), %s", ofs, ptr, reg); return;
  case 8: Printftn("mov %s(%s), %s", ofs, ptr, reg); return;
  }
  internal_error();
}

static void load_extend_int64(Type *ty, char *ofs, char *ptr, char *reg) {
  switch (ty->size) {
  case 4: Printftn("movslq %s(%s), %s", ofs, ptr, reg); return;
  case 2: Printftn("movswq %s(%s), %s", ofs, ptr, reg); return;
  case 1: Printftn("movsbq %s(%s), %s", ofs, ptr, reg); return;
  }
  internal_error();
}

// Round up `n` to the nearest multiple of `align`. For instance,
// align_to(5, 8) returns 8 and align_to(11, 8) returns 16.
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
      Printftn("movups %d+%s(%s), %%xmm0", i, sofs, sptr);
      Printftn("movups %%xmm0, %d+%s(%s)", i, dofs, dptr);
      i += 16;
      continue;
    }
    int p2 = (rem >= 8) ? 8 : (rem >= 4) ? 4 : (rem >= 2) ? 2 : 1;
    Printftn("mov %d+%s(%s), %s", i, sofs, sptr, reg_dx(p2));
    Printftn("mov %s, %d+%s(%s)", reg_dx(p2), i, dofs, dptr);
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
    Printstn("xorps %%xmm0, %%xmm0");
    for (int i = 0; i < sz;) {
      if (sz < i + 16)
        i = sz - 16;
      Printftn("movups %%xmm0, %d(%s)", i + dofs, dptr);
      i += 16;
    }
    return;
  }

  Printstn("xor %%eax, %%eax");
  for (int i = 0; i < sz;) {
    int rem = sz - i;
    int p2 = (rem >= 8) ? 8 : (rem >= 4) ? 4 : (rem >= 2) ? 2 : 1;
    Printftn("mov %s, %d(%s)", reg_ax(p2), i + dofs, dptr);
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
  Printftn("set%s %%al", cmp_cc(kind, is_unsigned));
  Printstn("movzbl %%al, %%eax");
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
  if (mem->ty->is_unsigned) {
    if (mem->bit_offset)
      Printftn("shr $%d, %s", mem->bit_offset, ax);
    imm_and(ax, "%rdx", (1LL << mem->bit_width) - 1);
    return;
  }
  int shft = ((mem->ty->size == 8) ? 64 : 32) - mem->bit_width;
  Printftn("shl $%d, %s", shft - mem->bit_offset, ax);
  Printftn("sar $%d, %s", shft, ax);
  return;
}

static void gen_bitfield_store(Node *node) {
  Member *mem = node->member;
  Type *alt_ty = bitwidth_to_ty(mem->bit_width, mem->ty->is_unsigned);
  if (alt_ty && (mem->bit_offset == (mem->bit_offset / 8 * 8))) {
    char *reg = pop_inreg(tmpreg64[0]);
    store2(alt_ty, mem->bit_offset / 8, reg);
    if (mem->bit_width < 32)
      imm_and("%eax", NULL, (1L << mem->bit_width) - 1);
    return;
  }

  char *ax, *dx, *cx;
  if (mem->ty->size == 8)
    ax = "%rax", cx = "%rcx", dx = "%rdx";
  else
    ax = "%eax", cx = "%ecx", dx = "%edx";

  Printftn("mov %s, %s", ax, cx);
  imm_and(cx, dx, (1LL << mem->bit_width) - 1);

  char *ptr = pop_inreg(tmpreg64[0]);
  load2(mem->ty, 0, ptr);

  imm_and(ax, dx, ~(((1ULL << mem->bit_width) - 1) << mem->bit_offset));

  if (mem->bit_offset) {
    Printftn("mov %s, %s", cx, dx);
    Printftn("shl $%d, %s", mem->bit_offset, dx);
    Printftn("or %s, %s", dx, ax);
  } else {
    Printftn("or %s, %s", cx, ax);
  }
  store2(mem->ty, 0, ptr);

  Printftn("mov %s, %s", cx, ax);
  if (!mem->ty->is_unsigned) {
    int shft = ((mem->ty->size == 8) ? 64 : 32) - mem->bit_width;
    Printftn("shl $%d, %s", shft, ax);
    Printftn("sar $%d, %s", shft, ax);
  }
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
static void gen_addr(Node *node) {
  if (opt_optimize && gen_addr_opt(node))
    return;

  switch (node->kind) {
  case ND_VAR:
    // Variable-length array, which is always local.
    if (node->var->ty->kind == TY_VLA) {
      Printftn("mov %d(%s), %%rax", node->var->ofs, node->var->ptr);
      return;
    }

    // Local variable
    if (node->var->is_local) {
      Printftn("lea %d(%s), %%rax", node->var->ofs, node->var->ptr);
      return;
    }

    if (opt_fpic) {
      // Thread-local variable
      if (node->var->is_tls) {
        clobber_all_regs();
        Printftn("data16 lea \"%s\"@tlsgd(%%rip), %%rdi", asm_name(node->var));
        Printstn(".value 0x6666");
        Printstn("rex64");
        Printstn("call __tls_get_addr@PLT");
        return;
      }

      // Function or global variable
      Printftn("mov \"%s\"@GOTPCREL(%%rip), %%rax", asm_name(node->var));
      return;
    }

    // Thread-local variable
    if (node->var->is_tls) {
      Printstn("mov %%fs:0, %%rax");
      Printftn("add $\"%s\"@tpoff, %%rax", asm_name(node->var));
      return;
    }

    // Here, we generate an absolute address of a function or a global
    // variable. Even though they exist at a certain address at runtime,
    // their addresses are not known at link-time for the following
    // two reasons.
    //
    //  - Address randomization: Executables are loaded to memory as a
    //    whole but it is not known what address they are loaded to.
    //    Therefore, at link-time, relative address in the same
    //    exectuable (i.e. the distance between two functions in the
    //    same executable) is known, but the absolute address is not
    //    known.
    //
    //  - Dynamic linking: Dynamic shared objects (DSOs) or .so files
    //    are loaded to memory alongside an executable at runtime and
    //    linked by the runtime loader in memory. We know nothing
    //    about addresses of global stuff that may be defined by DSOs
    //    until the runtime relocation is complete.
    //
    // In order to deal with the former case, we use RIP-relative
    // addressing, denoted by `(%rip)`. For the latter, we obtain an
    // address of a stuff that may be in a shared object file from the
    // Global Offset Table using `@GOTPCREL(%rip)` notation.

    // Function
    if (node->ty->kind == TY_FUNC) {
      if (node->var->is_definition)
        Printftn("lea \"%s\"(%%rip), %%rax", asm_name(node->var));
      else
        Printftn("mov \"%s\"@GOTPCREL(%%rip), %%rax", asm_name(node->var));
      return;
    }

    // Global variable
    Printftn("lea \"%s\"(%%rip), %%rax", asm_name(node->var));
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
    Printftn("movss %s(%s), %%xmm0", sofs, sptr);
    return;
  case TY_DOUBLE:
    Printftn("movsd %s(%s), %%xmm0", sofs, sptr);
    return;
  case TY_LDOUBLE:
    Printftn("fninit; fldt %s(%s)", sofs, sptr);
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
    // If it is an array, do not attempt to load a value to the
    // register because in general we can't load an entire array to a
    // register. As a result, the result of an evaluation of an array
    // becomes not the array itself but the address of the array.
    // This is where "array is automatically converted to a pointer to
    // the first element of the array in C" occurs.
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
    Printftn("movss %%xmm0, %s(%s)", dofs, dptr);
    return;
  case TY_DOUBLE:
    Printftn("movsd %%xmm0, %s(%s)", dofs, dptr);
    return;
  case TY_LDOUBLE:
    Printftn("fstpt %s(%s)", dofs, dptr);
    Printftn("fninit; fldt %s(%s)", dofs, dptr);
    return;
  }

  Printftn("mov %s, %s(%s)", reg_ax(ty->size), dofs, dptr);
}

static void store2(Type *ty, int dofs, char *dptr) {
  char ofs_buf[STRBUF_SZ];
  snprintf(ofs_buf, STRBUF_SZ, "%d", dofs);
  store3(ty, ofs_buf, dptr);
}

static void store(Node *node) {
  if (is_bitfield(node)) {
    gen_bitfield_store(node);
    return;
  }
  char *reg = pop_inreg(tmpreg64[0]);
  store2(node->ty, 0, reg);
}

static void load_val2(Type *ty, int64_t val, char *gp32, char *gp64) {
  if (val == 0) {
    Printftn("xor %s, %s", gp32, gp32);
    return;
  }
  if (val == (uint32_t)val) {
    Printftn("movl $%"PRIi64", %s", val, gp32);
    return;
  }
  if (val == (int32_t)val) {
    if (ty->size == 8)
      Printftn("movq $%"PRIi64", %s", val, gp64);
    else
      Printftn("movl $%"PRIi64", %s", val, gp32);
    return;
  }
  Printftn("movabsq $%"PRIi64", %s", val, gp64);
}

static void load_val(Type *ty, int64_t val) {
  load_val2(ty, val, "%eax", "%rax");
}

static void load_fval2(Type *ty, long double fval, int reg) {
  if (ty->kind == TY_FLOAT) {
    float pos_z = +0.0f;
    float fv = fval;
    if (!memcmp(&pos_z, &fv, sizeof(float))) {
      Printftn("xorps %%xmm%d, %%xmm%d", reg, reg);
      return;
    }
    union { float f32; uint32_t u32; } u = { fval };
    Printftn("movl $%u, %%eax", u.u32);
    Printftn("movd %%eax, %%xmm%d", reg);
    return;
  }

  double pos_z = +0.0;
  double dv = fval;
  if (!memcmp(&pos_z, &dv, sizeof(double))) {
    Printftn("xorps %%xmm%d, %%xmm%d", reg, reg);
    return;
  }
  union { double f64; uint64_t u64; } u = { fval };
  Printftn("movq $%"PRIu64", %%rax", u.u64);
  Printftn("movq %%rax, %%xmm%d", reg);
  return;
}

static void load_fval(Type *ty, long double fval) {
  if (ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE) {
    load_fval2(ty, fval, 0);
    return;
  }

  long double pos_z = +0.0L;
  if (!memcmp(&pos_z, &fval, 10)) {
    Printstn("fninit; fldz");
    return;
  }
  long double neg_z = -0.0L;
  if (!memcmp(&neg_z, &fval, 10)) {
    Printstn("fninit; fldz");
    Printstn("fchs");
    return;
  }
  if (fval == 1) {
    Printstn("fninit; fld1");
    return;
  }
  if (fval == -1) {
    Printstn("fninit; fld1");
    Printstn("fchs");
    return;
  }
  union { long double f80; uint64_t u64[2]; } u;
  memset(&u, 0, sizeof(u));
  u.f80 = fval;
  Printftn("movq $%"PRIu64", %%rax", u.u64[0]);
  Printftn("movw $%"PRIu16", %%dx", (uint16_t)u.u64[1]);
  Printstn("push %%rdx");
  Printstn("push %%rax");
  Printstn("fninit; fldt (%%rsp)");
  Printstn("add $16, %%rsp");
  return;
}

enum { I8, I16, I32, I64, U8, U16, U32, U64, F32, F64, F80 };

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

// The table for type casts
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

#define FROM_F80_1                                                        \
  "sub $24, %rsp; fnstcw 14(%rsp); movzwl 14(%rsp), %eax; or $12, %ah; " \
  "mov %ax, 12(%rsp); fldcw 12(%rsp); "

#define FROM_F80_2 " (%rsp); fldcw 14(%rsp); "
#define FROM_F80_3 "; add $24, %rsp"

static char f80i8[] = FROM_F80_1 "fistps" FROM_F80_2 "movsbl (%rsp), %eax" FROM_F80_3;
static char f80u8[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl (%rsp), %eax" FROM_F80_3;
static char f80i16[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl (%rsp), %eax" FROM_F80_3;
static char f80u16[] = FROM_F80_1 "fistpl" FROM_F80_2 "movswl (%rsp), %eax" FROM_F80_3;
static char f80i32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov (%rsp), %eax" FROM_F80_3;
static char f80u32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov (%rsp), %eax" FROM_F80_3;
static char f80i64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov (%rsp), %rax" FROM_F80_3;
static char f80u64[] =
  "sub $16, %rsp; movl $0x5f000000, 12(%rsp); flds 12(%rsp); fucomi %st(1), %st; setbe %al;"
  "fldz; fcmovbe %st(1), %st; fstp %st(1); fsubrp %st, %st(1); fnstcw 4(%rsp);"
  "movzwl 4(%rsp), %ecx; orl $3072, %ecx; movw %cx, 6(%rsp); fldcw 6(%rsp);"
  "fistpll 8(%rsp); fldcw 4(%rsp); shlq $63, %rax; xorq 8(%rsp), %rax; add $16, %rsp";

static char f80f32[] = "sub $8, %rsp; fstps (%rsp); movss (%rsp), %xmm0; add $8, %rsp";
static char f80f64[] = "sub $8, %rsp; fstpl (%rsp); movsd (%rsp), %xmm0; add $8, %rsp";

static char *cast_table[][11] = {
  // i8   i16     i32     i64     u8     u16     u32     u64     f32     f64     f80
  {NULL,  NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i8
  {i32i8, NULL,   NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i16
  {i32i8, i32i16, NULL,   i32i64, i32u8, i32u16, NULL,   i32i64, i32f32, i32f64, i32f80}, // i32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   i64f32, i64f64, i64f80}, // i64

  {i32i8, NULL,   NULL,   i32i64, NULL,  NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u8
  {i32i8, i32i16, NULL,   i32i64, i32u8, NULL,   NULL,   i32i64, i32f32, i32f64, i32f80}, // u16
  {i32i8, i32i16, NULL,   u32i64, i32u8, i32u16, NULL,   u32i64, u32f32, u32f64, u32f80}, // u32
  {i32i8, i32i16, NULL,   NULL,   i32u8, i32u16, NULL,   NULL,   u64f32, u64f64, u64f80}, // u64

  {f32i8, f32i16, f32i32, f32i64, f32u8, f32u16, f32u32, f32u64, NULL,   f32f64, f32f80}, // f32
  {f64i8, f64i16, f64i32, f64i64, f64u8, f64u16, f64u32, f64u64, f64f32, NULL,   f64f80}, // f64
  {f80i8, f80i16, f80i32, f80i64, f80u8, f80u16, f80u32, f80u64, f80f32, f80f64, NULL},   // f80
};

static void gen_cmp_zero(Node *node, NodeKind kind) {
  Node zero = {.kind = ND_NUM, .ty = node->ty, .tok = node->tok};
  Node expr = {.kind = kind, .lhs = node, .rhs = &zero, .tok = node->tok};
  add_type(&expr);
  gen_expr(&expr);
  return;
}

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
    Printftn("%s", cast_table[t1][t2]);
}

// Structs or unions equal or smaller than 16 bytes are passed
// using up to two registers.
//
// If the first 8 bytes contains only floating-point type members,
// they are passed in an XMM register. Otherwise, they are passed
// in a general-purpose register.
//
// If a struct/union is larger than 8 bytes, the same rule is
// applied to the the next 8 byte chunk.
//
// This function returns true if `ty` has only floating-point
// members in its byte range [lo, hi).
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

static void funcall_stk_args(Node *node) {
  for (Obj *var = node->args; var; var = var->param_next) {
    if (var->ptr) {
      Node var_node = {.kind = ND_VAR, .var = var, .tok = var->arg_expr->tok};
      Node assign_node = {
        .kind = ND_ASSIGN, .lhs = &var_node, .rhs = var->arg_expr, .tok = var->arg_expr->tok};
      add_type(&assign_node);
      gen_void_expr(&assign_node);
    }
  }
}

static void funcall_reg_args2(Type *ty, char *ofs, char *ptr, int *gp, int *fp) {
  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    if (has_flonum1(ty))
      Printftn("movsd %s(%s), %%xmm%d", ofs, ptr, (*fp)++);
    else
      Printftn("mov %s(%s), %s",  ofs, ptr, argreg64[(*gp)++]);

    if (ty->size > 8) {
      if (has_flonum2(ty))
        Printftn("movsd 8+%s(%s), %%xmm%d", ofs, ptr, (*fp)++);
      else
        Printftn("mov 8+%s(%s), %s",  ofs, ptr, argreg64[(*gp)++]);
    }
    return;
  case TY_FLOAT:
    Printftn("movss %s(%s), %%xmm%d", ofs, ptr, (*fp)++);
    return;
  case TY_DOUBLE:
    Printftn("movsd %s(%s), %%xmm%d", ofs, ptr, (*fp)++);
    return;
  }

  if (ty->size <= 4)
    load_extend_int(ty, ofs, ptr, argreg32[(*gp)++]);
  else
    load_extend_int(ty, ofs, ptr, argreg64[(*gp)++]);
}

static bool is_trivial_arg(Node *node, bool test, int *gp, int *fp) {
  Type *ty = node->ty;

  int64_t val;
  if (is_gp_ty(ty) && is_const_expr(node, &val)) {
    if (!test) {
      load_val2(ty, val, argreg32[*gp], argreg64[*gp]);
      (*gp)++;
    }
    return true;
  }

  long double fval;
  if (is_flonum(ty) && is_const_double(node, &fval)) {
    if (!test)
      load_fval2(ty, fval, (*fp)++);
    return true;
  }

  if (has_load_opt_gp(node)) {
    if (!test) {
      gen_load_opt_gp(node, argreg32[*gp], argreg64[*gp]);
      (*gp)++;
    }
    return true;
  }

  char ofs[STRBUF_SZ], *ptr;
  if (is_memop(node, ofs, &ptr, true)) {
    if (!test)
      funcall_reg_args2(node->ty, ofs, ptr, gp, fp);
    return true;
  }

  return false;
}

static void funcall_reg_args(Node *node) {
  int gp = 0, fp = 0;
  bool rtn_by_stk = node->ret_buffer && node->ty->size > 16;
  if (rtn_by_stk)
    Printftn("lea %d(%s), %s", node->ret_buffer->ofs, node->ret_buffer->ptr, argreg64[gp++]);

  for (Obj *var = node->args; var; var = var->param_next) {
    if (var->pass_by_stack)
      continue;
    if (is_trivial_arg(var->arg_expr, false, &gp, &fp))
      continue;
    char ofs[STRBUF_SZ];
    snprintf(ofs, STRBUF_SZ, "%d", var->ofs);
    funcall_reg_args2(var->ty, ofs, var->ptr, &gp, &fp);
  }
}

void prepare_funcall(Node *node, Scope *scope) {
  bool rtn_by_stk = node->ty->size > 16;
  calling_convention(node->args, &(int){rtn_by_stk}, &(int){0}, NULL);

  for (Obj *var = node->args; var; var = var->param_next) {
    var->is_local = true;
    if (var->pass_by_stack) {
      var->ofs = var->stack_offset;
      var->ptr = "%rsp";
      continue;
    }
    if (is_trivial_arg(var->arg_expr, true, &(int){0}, &(int){0}))
      continue;
    var->next = scope->locals;
    scope->locals = var;
  }
}

static void copy_ret_buffer(Obj *var) {
  Type *ty = var->ty;
  int gp = 0, fp = 0;

  for (int ofs = 0; ofs < ty->size; ofs += 8) {
    int chunk_sz = MIN(8, ty->size - ofs);

    if ((ofs == 0) ? has_flonum1(ty) : has_flonum2(ty)) {
      if (chunk_sz == 4)
        Printftn("movss %%xmm%d, %d(%s)", fp, ofs + var->ofs, var->ptr);
      else
        Printftn("movsd %%xmm%d, %d(%s)", fp, ofs + var->ofs, var->ptr);
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
        Printftn("movss %d(%s), %%xmm%d", ofs, sptr, fp);
      else
        Printftn("movsd %d(%s), %%xmm%d", ofs, sptr, fp);
      fp++;
      continue;
    }
    if (gp == 0) {
      Printstn("mov %%rax, %%rcx");
      sptr = "%rcx";
    }
    int regsz = (chunk_sz > 4) ? 8 : (chunk_sz > 2) ? 4 : (chunk_sz > 1) ? 2 : 1;
    if (gp == 0)
      Printftn("mov %d(%%rcx), %s", ofs, reg_ax(regsz));
    else
      Printftn("mov %d(%%rcx), %s", ofs, reg_dx(regsz));
    gp++;
  }
}

static void copy_struct_mem(void) {
  Type *ty = current_fn->ty->return_ty;

  Printftn("mov -%d(%s), %%rcx", rtn_ptr_ofs, lvar_ptr);
  gen_mem_copy(0, "%rax", 0, "%rcx", ty->size);
  Printstn("mov %%rcx, %%rax");
}

static void gen_vaarg_reg_copy(Type *ty, Obj *var) {
  int gp_inc = !has_flonum1(ty) + !has_flonum2(ty);
  if (gp_inc) {
    Printftn("cmpl $%d, (%%rax)", 48 - gp_inc * 8);
    Printstn("ja 1f");
  }
  int fp_inc = has_flonum1(ty) + has_flonum2(ty);
  Printftn("cmpl $%d, 4(%%rax)", 176 - fp_inc * 16);
  Printstn("ja 1f");

  for (int ofs = 0; ofs < ty->size; ofs += 8) {
    if ((ofs == 0) ? has_flonum1(ty) : has_flonum2(ty)) {
      Printstn("movl 4(%%rax), %%ecx");  // fp_offset
      Printstn("addq 16(%%rax), %%rcx"); // reg_save_area
      Printstn("addq $16, 4(%%rax)");
    } else {
      Printstn("movl (%%rax), %%ecx");   // gp_offset
      Printstn("addq 16(%%rax), %%rcx"); // reg_save_area
      Printstn("addq $8, (%%rax)");
    }
    gen_mem_copy(0, "%rcx",
                 ofs + var->ofs, var->ptr,
                 MIN(8, ty->size - ofs));
  }
  Printftn("lea %d(%s), %%rdx", var->ofs, var->ptr);
  return;
}

static void builtin_alloca(Node *node) {
  // Shift the temporary area by %rax.
  Printstn("sub %%rax, %%rsp");
  // Align frame pointer
  int align = node->var ? MAX(node->var->align, 16) : 16;
  Printftn("and $-%d, %%rsp", align);
  if (node->var)
    Printftn("mov %%rsp, %d(%s)", node->var->ofs, node->var->ptr);
  else
    Printstn("mov %%rsp, %%rax");
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
        Printftn("mov %d(%s), %%rsp", vla->ofs, vla->ptr);
      else
        Printftn("mov -%d(%s), %%rsp", vla_base_ofs, lvar_ptr);

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

  Printftn(".loc %d %d", tok->display_file_no, tok->display_line_no);

  file_no = tok->display_file_no;
  line_no = tok->display_line_no;
}

static void gen_expr_null_lhs(NodeKind kind, Type *ty, Node *rhs) {
  Node null = {.kind = ND_NULL_EXPR, .ty = ty, .tok = rhs->tok};
  Node expr = {.kind = kind, .lhs = &null, .rhs = rhs,.tok = rhs->tok};
  add_type(&expr);
  gen_expr(new_cast(&expr, ty));
}

static void gen_label_ph(char *label_str, bool is_main_epilogue) {
  if (!label_str) {
    if (is_main_epilogue)
      Printstn("xor %%eax, %%eax");
    return;
  }
  static char *buf;
  static size_t buf_n;
  int len = strlen(label_str);
  fseek(output_file, -(len + 7), SEEK_CUR);
  if (getline(&buf, &buf_n, output_file) != (len + 7) ||
    strncmp(buf, "  jmp ", 6) || strncmp(&buf[6], label_str, len)) {
    fseek(output_file, (len + 7), SEEK_CUR);
    if (is_main_epilogue)
      Printstn("xor %%eax, %%eax");
  }
  Printfsn("%s:", label_str);
}

static void gen_return_jmp(void) {
  if (!rtn_label)
    rtn_label = new_unique_name();
  Printftn("jmp %s", rtn_label);
}

static char *gen_cond_opt(Node *node, bool need_result, bool jump_cond) {
  if (!node)
    return NULL;

  bool flip = false;
  if (opt_optimize) {
    Node *expr = bool_expr_opt(node, &flip);
    if (expr)
      node = expr;

    int64_t val;
    if (is_const_expr(node, &val)) {
      val = !!val ^ flip;
      if (val != jump_cond)
        return NULL;
      if (need_result)
        load_val(ty_int, val);
      return "mp";
    }

    if (is_cmp(node) && is_gp_ty(node->lhs->ty)) {
      NodeKind kind = node->kind;
      flip_cmp(&kind, flip);
      if (!gen_cmp_opt_gp(node, &kind)) {
        gen_expr(node->lhs);
        push();
        gen_expr(node->rhs);

        bool is_r64 = node->lhs->ty->size == 8;
        char *op = pop_inreg2(is_r64, (is_r64 ? tmpreg64 : tmpreg32)[0]);
        Printftn("cmp %s, %s", regop_ax(node->lhs->ty), op);
      }
      if (need_result)
        gen_cmp_setcc(kind, node->lhs->ty->is_unsigned);
      flip_cmp(&kind, !jump_cond);
      return cmp_cc(kind, node->lhs->ty->is_unsigned);
    }

    if (node->kind == ND_CAST && node->ty->kind == TY_BOOL) {
      Node zero = {.kind = ND_NUM, .ty = node->lhs->ty, .tok = node->tok};
      Node expr = {.kind = flip ? ND_EQ : ND_NE, .lhs = node->lhs, .rhs = &zero, .tok = node->tok};
      add_type(&expr);
      return gen_cond_opt(&expr, need_result, jump_cond);
    }
  }
  gen_expr(node);

  if (!need_result) {
    Printstn("test %%al, %%al");
    return (jump_cond ^ flip) ? "ne" : "e";
  }
  if (flip)
    Printstn("xor $1, %%al");
  else
    Printstn("test %%al, %%al");
  return jump_cond ? "ne" : "e";
}

static char *gen_cond(Node *node, bool jump_cond) {
  return gen_cond_opt(node, false, jump_cond);
}

static char *gen_logical_cond(Node *node, bool jump_cond) {
  return gen_cond_opt(node, true, jump_cond);
}

// Generate code for a given node.
static void gen_expr(Node *node) {
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
      Printstn("mov $0x80000000, %%eax");
      Printstn("movd %%eax, %%xmm1");
      Printstn("xorps %%xmm1, %%xmm0");
      return;
    case TY_DOUBLE:
      Printstn("mov $0x8000000000000000, %%rax");
      Printstn("movq %%rax, %%xmm1");
      Printstn("xorpd %%xmm1, %%xmm0");
      return;
    case TY_LDOUBLE:
      Printstn("fchs");
      return;
    }

    Printftn("neg %s", regop_ax(node->lhs->ty));
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
    store(node->lhs);
    return;
  case ND_ARITH_ASSIGN:
    gen_addr(node->lhs);
    push();
    load(node->lhs, 0);
    gen_expr_null_lhs(node->arith_kind, node->lhs->ty, node->rhs);
    store(node->lhs);
    return;
  case ND_POST_INCDEC:
    gen_addr(node->lhs);
    Printstn("movq %%rax, %%rcx");
    load(node->lhs, 0);
    push_by_ty(node->lhs->ty);
    push_from("%rcx");
    gen_expr_null_lhs(ND_ADD, node->lhs->ty, node->rhs);
    store(node->lhs);
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
    int c = count();
    char *ins = gen_cond(node->cond, false);
    if (ins)
      Printftn("j%s .L.else.%d", ins, c);
    gen_expr(node->then);
    Printftn("jmp .L.end.%d", c);
    Printfsn(".L.else.%d:", c);
    gen_expr(node->els);
    Printfsn(".L.end.%d:", c);
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);
    Printstn("xor $1, %%al");
    return;
  case ND_BITNOT:
    gen_expr(node->lhs);
    Printstn("not %%rax");
    return;
  case ND_LOGAND: {
    int c = count();
    char *ins = gen_logical_cond(node->lhs, false);
    if (ins)
      Printftn("j%s .L.false.%d", ins, c);
    gen_expr(node->rhs);
    Printfsn(".L.false.%d:", c);
    return;
  }
  case ND_LOGOR: {
    int c = count();
    char *ins = gen_logical_cond(node->lhs, true);
    if (ins)
      Printftn("j%s .L.true.%d", ins, c);
    gen_expr(node->rhs);
    Printfsn(".L.true.%d:", c);
    return;
  }
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
    gen_expr(node->lhs);
    push();
    gen_expr(node->rhs);
    Printstn("mov %%al, %%cl");

    char *ax = regop_ax(node->ty);
    pop2((node->ty->size == 8), ax);

    switch (node->kind) {
    case ND_SHL: Printftn("shl %%cl, %s", ax); break;
    case ND_SHR: Printftn("shr %%cl, %s", ax); break;
    case ND_SAR: Printftn("sar %%cl, %s", ax); break;
    }
    return;
  case ND_FUNCALL: {
    if (node->lhs->kind == ND_VAR && !strcmp(asm_name(node->lhs->var), "alloca")) {
      gen_expr(node->args->arg_expr);
      builtin_alloca(node);
      return;
    }

    // If the return type is a large struct/union, the caller passes
    // a pointer to a buffer as if it were the first argument.
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
        Printftn("sub $%d, %%rsp", arg_stk_size);
        Printftn("and $-%d, %%rsp", arg_stk_align);
      } else {
        Printftn("sub $%d, %%rsp", align_to(arg_stk_size, 16));
      }
    }

    funcall_stk_args(node);
    funcall_reg_args(node);

    if (node->lhs->ty->is_variadic) {
      if (fp_count)
        Printftn("movb $%d, %%al", fp_count);
      else
        Printstn("xor %%al, %%al");
    }

    if (use_fn_ptr) {
      switch (gp_count) {
      case 0: break;
      case 1: clobber_gp(1); break;  // %rdi
      default: clobber_gp(4); break;
      }
      Printftn("call *%s", pop_inreg("%r10"));
    } else {
      Printftn("call \"%s\"%s", asm_name(node->lhs->var), opt_fpic ? "@PLT" : "");
    }

    clobber_all_regs();

    if (arg_stk_size) {
      if (arg_stk_align > 16)
        pop("%rsp");
      else
        Printftn("add $%d, %%rsp", align_to(arg_stk_size, 16));
    }

    // It looks like the most significant 48 or 56 bits in RAX may
    // contain garbage if a function return type is short or bool/char,
    // respectively. We clear the upper bits here.
    if (is_integer(node->ty) && node->ty->size < 4) {
      if (node->ty->kind == TY_BOOL)
        Printftn("%s", cast_table[getTypeId(ty_int)][getTypeId(ty_uchar)]);
      else
        Printftn("%s", cast_table[getTypeId(ty_int)][getTypeId(node->ty)]);
    }

    // If the return type is a small struct, a value is returned
    // using up to two registers.
    if (node->ret_buffer && node->ty->size <= 16) {
      copy_ret_buffer(node->ret_buffer);
      Printftn("lea %d(%s), %%rax", node->ret_buffer->ofs, node->ret_buffer->ptr);
    }
    return;
  }
  case ND_LABEL_VAL:
    Printftn("lea %s(%%rip), %%rax", node->unique_label);
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
    case TY_DOUBLE: Printftn("movq %%xmm0, %s", dx); break;
    case TY_FLOAT: Printftn("movd %%xmm0, %s", dx); break;
    default: Printftn("mov %s, %s", ax, dx); break;
    }

    Printftn("mov (%s), %s", old, ax);
    Printftn("lock cmpxchg %s, (%s)", dx, addr);
    Printstn("sete %%cl");
    Printstn("je 1f");
    Printftn("mov %s, (%s)", ax, old);
    Printssn("1:");
    Printstn("movzbl %%cl, %%eax");
    return;
  }
  case ND_EXCH: {
    gen_expr(node->lhs);
    push();
    gen_expr(node->rhs);
    char *reg = pop_inreg(tmpreg64[0]);

    int sz = node->lhs->ty->base->size;
    Printftn("xchg %s, (%s)", reg_ax(sz), reg);
    return;
  }
  case ND_ALLOCA:
    gen_expr(node->lhs);
    builtin_alloca(node);
    return;
  case ND_VA_START: {
    gen_expr(node->lhs);
    Printftn("movl $%d, (%%rax)", va_gp_start);
    Printftn("movl $%d, 4(%%rax)", va_fp_start);
    Printftn("lea %d(%%rbp), %%rdx", va_st_start);
    Printstn("movq %%rdx, 8(%%rax)");
    Printftn("lea -176(%s), %%rdx", lvar_ptr);
    Printstn("movq %%rdx, 16(%%rax)");
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
        // Structs with FP member are split into 8-byte chunks in the
        // reg save area, we reconstruct the layout with a local copy.
        gen_vaarg_reg_copy(ty, node->var);
      } else if (has_flonum1(ty)) {
        Printftn("cmpl $%d, 4(%%rax)", 160);
        Printstn("ja 1f");
        Printstn("movl 4(%%rax), %%edx");  // fp_offset
        Printstn("addq 16(%%rax), %%rdx"); // reg_save_area
        Printstn("addq $16, 4(%%rax)");
      } else {
        int gp_inc = ty->size > 8 ? 2 : 1;
        Printftn("cmpl $%d, (%%rax)", 48 - gp_inc * 8);
        Printstn("ja 1f");
        Printstn("movl (%%rax), %%edx");   // gp_offset
        Printstn("addq 16(%%rax), %%rdx"); // reg_save_area
        Printftn("addq $%d, (%%rax)", gp_inc * 8);
      }
      Printstn("jmp 2f");
      Printssn("1:");
    }
    Printstn("movq 8(%%rax), %%rdx"); // overflow_arg_area
    if (ty->align <= 8) {
      Printftn("addq $%d, 8(%%rax)", align_to(ty->size, 8));
    } else {
      Printftn("addq $%d, %%rdx", ty->align - 1);
      Printftn("andq $-%d, %%rdx", ty->align);
      Printftn("lea %d(%%rdx), %%rcx", align_to(ty->size, 8));
      Printstn("movq %%rcx, 8(%%rax)");
    }
    if (ty->size <= 16)
      Printssn("2:");
    Printstn("mov %%rdx, %%rax");
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
      Printftn("add%s %%xmm%d, %%xmm0", sz, reg);
      return;
    case ND_SUB:
      Printftn("sub%s %%xmm0, %%xmm%d", sz, reg);
      Printftn("movaps %%xmm%d, %%xmm0", reg);
      return;
    case ND_MUL:
      Printftn("mul%s %%xmm%d, %%xmm0", sz, reg);
      return;
    case ND_DIV:
      Printftn("div%s %%xmm0, %%xmm%d", sz, reg);
      Printftn("movaps %%xmm%d, %%xmm0", reg);
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GT:
    case ND_GE:
      if (node->kind == ND_GT || node->kind == ND_GE)
        Printftn("ucomi%s %%xmm0, %%xmm%d", sz, reg);
      else
        Printftn("ucomi%s %%xmm%d, %%xmm0", sz, reg);

      if (node->kind == ND_EQ) {
        Printstn("sete %%al");
        Printstn("setnp %%dl");
        Printstn("and %%dl, %%al");
      } else if (node->kind == ND_NE) {
        Printstn("setne %%al");
        Printstn("setp %%dl");
        Printstn("or %%dl, %%al");
      } else if (node->kind == ND_LT || node->kind == ND_GT) {
        Printstn("seta %%al");
      } else if (node->kind == ND_LE || node->kind == ND_GE) {
        Printstn("setae %%al");
      }

      Printstn("movzbl %%al, %%eax");
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
      Printstn("faddp");
      return;
    case ND_SUB:
      Printstn("fsubp");
      return;
    case ND_MUL:
      Printstn("fmulp");
      return;
    case ND_DIV:
      Printstn("fdivp");
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GT:
    case ND_GE:
      if (node->kind == ND_LT || node->kind == ND_LE)
        Printstn("fxch %%st(1)");

      Printstn("fucomip");
      Printstn("fstp %%st(0)");

      if (node->kind == ND_EQ) {
        Printstn("sete %%al");
        Printstn("setnp %%dl");
        Printstn("and %%dl, %%al");
      } else if (node->kind == ND_NE) {
        Printstn("setne %%al");
        Printstn("setp %%dl");
        Printstn("or %%dl, %%al");
      } else if (node->kind == ND_LT || node->kind == ND_GT) {
        Printstn("seta %%al");
      } else if (node->kind == ND_LE || node->kind == ND_GE) {
        Printstn("setae %%al");
      }

      Printstn("movzbl %%al, %%eax");
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
    Printftn("add %s, %s", op, ax);
    return;
  case ND_SUB:
    Printftn("sub %s, %s", ax, op);
    Printftn("mov %s, %s", op, ax);
    return;
  case ND_MUL:
    Printftn("imul %s, %s", op, ax);
    return;
  case ND_DIV:
  case ND_MOD:
    Printftn("xchg %s, %s", op, ax);
    if (node->ty->is_unsigned) {
      Printstn("xor %%edx, %%edx");
      Printftn("div %s", op);
    } else {
      if (node->lhs->ty->size == 8)
        Printstn("cqo");
      else
        Printstn("cdq");
      Printftn("idiv %s", op);
    }

    if (node->kind == ND_MOD)
      Printstn("mov %%rdx, %%rax");
    return;
  case ND_BITAND:
    Printftn("and %s, %s", op, ax);
    return;
  case ND_BITOR:
    Printftn("or %s, %s", op, ax);
    return;
  case ND_BITXOR:
    Printftn("xor %s, %s", op, ax);
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
    Printftn("cmp %s, %s", ax, op);
    gen_cmp_setcc(node->kind, node->lhs->ty->is_unsigned);
    return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  if (opt_g)
    print_loc(node->tok);

  switch (node->kind) {
  case ND_NULL_STMT:
    return;
  case ND_IF: {
    int c = count();
    char *ins = gen_cond(node->cond, false);
    if (ins)
      Printftn("j%s .L.else.%d", ins, c);
    gen_stmt(node->then);
    if (!node->els) {
      Printfsn(".L.else.%d:", c);
      return;
    }
    Printftn("jmp .L.end.%d", c);
    Printfsn(".L.else.%d:", c);
    gen_stmt(node->els);
    Printfsn(".L.end.%d:", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    Printfsn(".L.begin.%d:", c);
    char *ins = gen_cond(node->cond, false);
    if (ins)
      Printftn("j%s %s", ins, node->brk_label);
    gen_stmt(node->then);
    Printfsn("%s:", node->cont_label);
    if (node->inc)
      gen_void_expr(node->inc);
    Printftn("jmp .L.begin.%d", c);
    Printfsn("%s:", node->brk_label);
    gen_defr(node);
    return;
  }
  case ND_DO: {
    int c = count();
    Printfsn(".L.begin.%d:", c);
    gen_stmt(node->then);
    Printfsn("%s:", node->cont_label);
    char *ins = gen_cond(node->cond, true);
    if (ins)
      Printftn("j%s .L.begin.%d", ins, c);
    Printfsn("%s:", node->brk_label);
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
        Printftn("je %s", n->label);
        continue;
      }
      if (n->begin == 0) {
        imm_cmp(ax, dx, n->end);
        Printftn("jbe %s", n->label);
        continue;
      }
      Printftn("mov %s, %s", ax, cx);
      imm_sub(cx, dx, n->begin);
      imm_cmp(cx, dx, n->end - n->begin);
      Printftn("jbe %s", n->label);
    }

    if (node->default_case)
      Printftn("jmp %s", node->default_case->label);

    Printftn("jmp %s", node->brk_label);
    gen_stmt(node->then);
    gen_label_ph(node->brk_label, false);
    return;
  }
  case ND_CASE:
    Printfsn("%s:", node->label);
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
    Printftn("jmp %s", node->unique_label);
    return;
  case ND_GOTO_EXPR:
    gen_expr(node->lhs);
    Printstn("jmp *%%rax");
    return;
  case ND_LABEL:
    Printfsn("%s:", node->unique_label);
    if (node->lhs)
      gen_stmt(node->lhs);
    return;
  case ND_RETURN: {
    if (!node->lhs) {
      if (has_defr(node))
        gen_defr(node);
      gen_return_jmp();
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
        gen_return_jmp();
        return;
      }
      copy_struct_mem();
      if (has_defr(node)) {
        push();
        gen_defr(node);
        pop("%rax");
      }
      gen_return_jmp();
      return;
    }
    if (has_defr(node)) {
      push_by_ty(ty);
      gen_defr(node);
      pop_by_ty(ty);
    }
    gen_return_jmp();
    return;
  }
  case ND_EXPR_STMT:
    gen_void_expr(node->lhs);
    return;
  case ND_ASM:
    gen_asm(node);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

static void imm_tmpl(char *ins, char *op, int64_t val) {
  if (val == (int32_t)val) {
    Printftn("%s $%"PRIi64", %s", ins, val, op);
    return;
  }
  if (val == (uint32_t)val)
    Printftn("movl $%"PRIi64", %%edx", val);
  else
    Printftn("movabsq $%"PRIi64", %%rdx", val);
  Printftn("%s %%rdx, %s", ins, op);
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
    Printftn("%s %s, %s(%s)", ins_sz, reg_ax(lhs->ty->size), ofs, ptr);
    return;
  }

  gen_addr(lhs);
  push();
  gen_expr(rhs);
  char *dptr = pop_inreg(tmpreg64[0]);
  Printftn("%s %s, (%s)", ins_sz, reg_ax(lhs->ty->size), dptr);
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

  gen_expr(node);
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

  if (!opt_fpic && is_memop_ptr(rhs, sofs, &sptr) && !strcmp(sptr, "%rip")) {
    char dofs[STRBUF_SZ], *dptr;
    if (is_memop(lhs, dofs, &dptr, false)) {
      Printftn("movq $%s, %s(%s)", sofs, dofs, dptr);
      return;
    }
    if (!is_bitfield(lhs) && !lhs->ty->is_atomic) {
      gen_addr(lhs);
      Printftn("movq $%s, (%%rax)", sofs);
      return;
    }
  }

  gen_expr(node);
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
  gen_expr(node);
}

static char *arith_ins(NodeKind kind) {
  char *ins;
  switch (kind) {
  case ND_ADD: ins = "add"; break;
  case ND_SUB: ins = "sub"; break;
  case ND_MUL: ins = "imul"; break;
  case ND_BITAND: ins = "and"; break;
  case ND_BITOR:ins =  "or"; break;
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
    Printftn("%s $%"PRIi64", %s", arith_ins(kind), val, op);
    return;
  }
  Printftn("mov $%"PRIi64", %s", val, tmp);
  Printftn("%s %s, %s", arith_ins(kind), tmp, op);
  return;
}

static void imm_add(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0: return;
  case 1: Printftn("inc %s", op); return;
  case -1: Printftn("dec %s", op); return;
  }
  imm_arith2(ND_ADD, op, tmp, val);
}

static void imm_sub(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0: return;
  case 1: Printftn("dec %s", op); return;
  case -1: Printftn("inc %s", op); return;
  }
  imm_arith2(ND_SUB, op, tmp, val);
}

static void imm_and(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0: Printftn("xor %s, %s", op, op); return;
  case -1: return;
  }
  imm_arith2(ND_BITAND, op, tmp, val);
}

static void imm_cmp(char *op, char *tmp, int64_t val) {
  if (val == 0) {
    Printftn("test %s, %s", op, op);
    return;
  }
  if (in_imm_range(val)) {
    Printftn("cmp $%"PRIi64", %s", val, op);
    return;
  }
  Printftn("mov $%"PRIi64", %s", val, tmp);
  Printftn("cmp %s, %s", tmp, op);
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
      Printstn("xor %%eax, %%eax");
      return;
    }
  }

  if (val == 1)
    if (kind == ND_MUL)
      return;

  if (val == -1) {
    switch (kind) {
    case ND_MUL:
      Printftn("neg %s", ax);
      return;
    case ND_BITOR:
      Printftn("mov $-1, %s", ax);
      return;
    case ND_BITXOR:
      Printftn("not %s", ax);
      return;
    }
  }

  if (kind == ND_MUL && is_pow_of_two(val)) {
    for (int i = 1; i < sz * 8; i++) {
      if (1LL << i == val) {
        Printftn("shl $%d, %s", i, ax);
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
      Printstn("xor %%eax, %%eax");
    return true;
  }

  if (val == -1) {
    gen_expr(expr);

    if (!ty->is_unsigned) {
      if (kind == ND_DIV)
        Printftn("neg %s", ax);
      else
        Printstn("xor %%eax, %%eax");
      return true;
    }

    if (kind == ND_DIV) {
      Printftn("cmp $-1, %s", ax);
      gen_cmp_setcc(ND_EQ, false);
      return true;
    }

    Printstn("xor %%edx, %%edx");
    Printftn("cmp $-1, %s", ax);
    Printftn("cmove %s, %s", dx, ax);
    return true;
  }

  if (kind == ND_DIV && is_pow_of_two(val) && ty->is_unsigned) {
    for (int i = 1; i < ty->size * 8; i++) {
      if (1LL << i == val) {
        gen_expr(expr);
        Printftn("shr $%d, %s", i, ax);
        return true;
      }
    }
  }

  if (kind == ND_MOD && is_pow_of_two(val) && ty->is_unsigned && val != 0) {
    gen_expr(expr);

    uint64_t msk = val - 1;
    if (msk == UINT32_MAX) {
      Printstn("movl %%eax, %%eax");
      return true;
    }
    if (msk <= INT32_MAX) {
      Printftn("and $%d, %%eax", (int)msk);
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
    Printftn("cmp %s, %s(%s)", regop_ax(lhs->ty), ofs, ptr);
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
      Printftn("%s %s(%s), %s", arith_ins(kind), ofs, ptr, ax);
      return true;
    }
    break;
  case 3:
    if (is_int_to_int_cast(rhs) && is_memop(rhs->lhs, ofs, &ptr, true) &&
      sz <= rhs->lhs->ty->size) {
      gen_expr(lhs);
      Printftn("%s %s(%s), %s", arith_ins(kind), ofs, ptr, ax);
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

      Printftn("%s %s, %s", arith_ins(kind), reg_dx(sz), ax);
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

    Printftn("movb %s(%s), %%cl", ofs, ptr);
    Printftn("%s %%cl, %s", arith_ins(node->kind), ax);
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

static bool gen_load_opt_gp(Node *node, char *reg32, char *reg64) {
  char ofs[STRBUF_SZ], *ptr;
  Node *lhs = node->lhs;
  Type *ty = node->ty;

  if (is_memop_ptr(node, ofs, &ptr)) {
    if (!strcmp(ptr, "%rip") && !opt_fpic)
      Printftn("movl $%s, %s", ofs, reg32);
    else
      Printftn("lea %s(%s), %s", ofs, ptr, reg64);
    return true;
  }

  if (is_int_to_int_cast(node) && is_memop(lhs, ofs, &ptr, true)) {
    if (ty->size > lhs->ty->size) {
      if (!lhs->ty->is_unsigned && ty->size == 8) {
        load_extend_int64(lhs->ty, ofs, ptr, reg64);
        return true;
      }
      if (!(ty->size == 2 && ty->is_unsigned && !lhs->ty->is_unsigned)) {
        load_extend_int(lhs->ty, ofs, ptr, reg32);
        return true;
      }
    } else if (ty->kind != TY_BOOL) {
      load_extend_int(ty, ofs, ptr, ty->size == 8 ? reg64 : reg32);
      return true;
    }
  }
  return false;
}

static bool has_load_opt_gp(Node *node) {
  char ofs[STRBUF_SZ], *ptr;
  Node *lhs = node->lhs;
  Type *ty = node->ty;

  if (is_memop_ptr(node, ofs, &ptr))
    return true;

  if (is_int_to_int_cast(node) && is_memop(lhs, ofs, &ptr, true)) {
    if (ty->size > lhs->ty->size) {
      if (!lhs->ty->is_unsigned && ty->size == 8)
        return true;
      if (!(ty->size == 2 && ty->is_unsigned && !lhs->ty->is_unsigned))
        return true;
    } else if (ty->kind != TY_BOOL) {
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
  Node *expr = bool_expr_opt(node, &flip);
  if (expr) {
    if (expr->kind == ND_CAST && expr->ty->kind == TY_BOOL) {
      gen_cmp_zero(expr->lhs, flip ? ND_EQ : ND_NE);
      return true;
    }
    if (expr != node) {
      if (is_cmp(expr)) {
        Node n = *expr;
        flip_cmp(&n.kind, flip);
        gen_expr(&n);
        return true;
      }
      gen_expr(expr);
      if (flip)
        Printstn("xor $1, %%al");
      return true;
    }
  }
  return false;
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

  if (gen_load_opt_gp(node, "%eax", "%rax"))
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

  if (is_gp_ty(ty) && gen_gp_opt(node))
    return true;

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
      Printftn("lea %s(%s), %%rax", var_ofs, var_ptr);
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
  NodeKind kind = node->kind;

  {
    char ofs[STRBUF_SZ], *ptr;
    Node addr_node = {.kind = ND_ADDR, .lhs = node, .tok = node->tok};
    if (is_memop_ptr(&addr_node, ofs, &ptr)) {
      Printftn("lea %s(%s), %%rax", ofs, ptr);
      return true;
    }
  }

  if (kind == ND_DEREF) {
    int64_t ofs = 0;
    gen_deref_opt(node->lhs, &ofs);
    imm_add("%rax", "%rdx", ofs);
    return true;
  }

  if (kind == ND_MEMBER) {
    int64_t ofs = 0;
    gen_member_opt(node, &ofs);
    imm_add("%rax", "%rdx", ofs);
    return true;
  }

  return false;
}

static int asm_ops_push(AsmParam *ap) {
  int idx = asm_ops.cnt++;
  if (idx >= asm_ops.capacity) {
    asm_ops.capacity = idx + 4;
    asm_ops.data = realloc(asm_ops.data, sizeof(AsmParam *) * asm_ops.capacity);
    asm_ops.data[idx] = NULL;
  }
  asm_ops.data[idx] = ap;
  return idx;
}

static void asm_fill_ops(Node *node) {
  asm_ops.cnt = 0;
  for (int i = 0; i < asm_ops.capacity; i++)
    asm_ops.data[i] = NULL;

  for (AsmParam *ap = node->asm_outputs; ap; ap = ap->next)
    asm_ops_push(ap);

  for (AsmParam *ap = node->asm_inputs; ap; ap = ap->next)
    asm_ops_push(ap);
}

static void asm_fill_ops2(Node *node) {
  for (AsmParam *ap = node->asm_outputs; ap; ap = ap->next)
    if (*ap->constraint->str == '+')
      asm_ops_push(ap);

  for (AsmParam *ap = node->asm_labels; ap; ap = ap->next)
    ap->label_id = asm_ops_push(ap);
}

static bool is_gp_reg(Reg reg) {
  return REG_AX <= reg && reg <= REG_SP;
}

static bool is_xmm_reg(Reg reg) {
  return REG_XMM0 <= reg && reg <= REG_XMM15;
}

static bool is_x87_reg(Reg reg) {
  return REG_X87_ST0 <= reg && reg <= REG_X87_ST7;
}

static Reg ident_gp_reg(char *loc) {
  static HashMap map;
  if (map.capacity == 0) {
    for (Reg r = REG_NULL; r < REG_END; r++)
      if (is_gp_reg(r))
        for (int j = 0; j < 4; j++)
          hashmap_put(&map, &regs[r][j][1], (void *)(intptr_t)r);

    hashmap_put(&map, "ah", (void *)(intptr_t)REG_AX);
    hashmap_put(&map, "bh", (void *)(intptr_t)REG_BX);
    hashmap_put(&map, "ch", (void *)(intptr_t)REG_CX);
    hashmap_put(&map, "dh", (void *)(intptr_t)REG_DX);
  }
  return (intptr_t)hashmap_get(&map, loc);
}

static Reg ident_reg(Token *tok) {
  char *loc = tok->str;
  while (*loc == '%')
    loc++;

  Reg gpreg = ident_gp_reg(loc);
  if (gpreg)
    return gpreg;

  if (!strncmp(loc, "xmm", 3) || !strncmp(loc, "ymm", 3) || !strncmp(loc, "zmm", 3))
    return REG_XMM0 + strtoul(&loc[3], NULL, 10);

  if (!strncmp(loc, "st", 2)) {
    if (loc[2] == '(') {
      unsigned long num = strtoul(&loc[3], NULL, 10);
      return REG_X87_ST0 + MIN(num, 7);
    }
    return REG_X87_ST0;
  }
  error_tok(tok, "unknown register");
}

static void asm_clobbers(Token *tok, int *x87_clobber) {
  if (!tok)
    return;

  Token *start = tok;
  for (; !equal(tok, ":") && !equal(tok, ")"); tok = tok->next) {
    if (tok != start)
      tok = skip(tok, ",");

    if (equal(tok, "\"cc\""))
      continue;
    if (equal(tok, "\"memory\""))
      continue;

    Reg r = ident_reg(tok);
    if (r == REG_SP)
      continue;
    if (is_x87_reg(r)) {
      *x87_clobber = MAX(*x87_clobber, (r - REG_X87_ST0 + 1));
      continue;
    }
    asm_use.in[r] = asm_use.out[r] = true;
    continue;
  }
}

static bool has_matching(AsmParam *ap) {
  return ap->match && !ap->match->kind;
}

static bool asm_get_use(AsmParam *ap, Reg r) {
  if (ap->constraint->str[0] != '=' && ap->constraint->str[0] != '+')
    return asm_use.in[r];

  if (ap->constraint->str[0] == '=' && ap->kind == ASMOP_REG &&
    !ap->is_early_clobber && !has_matching(ap))
    return asm_use.out[r];

  return asm_use.in[r] || asm_use.out[r];
}

static void asm_set_use(AsmParam *ap) {
  if (ap->constraint->str[0] != '=' && ap->constraint->str[0] != '+') {
    asm_use.in[ap->reg] = true;
    return;
  }
  if (ap->constraint->str[0] == '=' && ap->kind == ASMOP_REG &&
    !ap->is_early_clobber && !has_matching(ap)) {
    asm_use.out[ap->reg] = true;
    return;
  }
  asm_use.out[ap->reg] = asm_use.in[ap->reg] = true;

  if (has_matching(ap)) {
    ap->match->kind = ASMOP_REG;
    ap->match->reg = ap->reg;
  }
}

static void fixed_reg(Reg *reg, Reg spec, Token *tok) {
  if (*reg && *reg != spec)
    error_tok(tok, "conflicting register constraints");
  *reg = spec;
}

static void asm_constraint(AsmParam *ap, int x87_clobber) {
  for (; ap; ap = ap->next) {
    Reg reg = REG_NULL;
    char *p = ap->constraint->str;
    Token *tok = ap->arg->tok;
    int match_idx = -1;
    bool is_num = false;
    bool is_symbolic = false;

    if (!strncmp(p, "=@cc", 4)) {
      ap->kind = ASMOP_FLAG;
      ap->flag = strdup(&p[4]);
      continue;
    }
    for (; *p; p++) {
      switch (*p) {
      case '=':
      case '+': continue;
      case '&': ap->is_early_clobber = true; continue;
      case 'm':
      case 'o': ap->is_mem = true; continue;
      case 'r':
      case 'q': ap->is_gp = true; continue;
      case 'Q': ap->is_gp_highbyte = true; continue;
      case 'R': ap->is_gp_legacy = true; continue;
      case 'U': ap->is_gp_free = true; continue;
      case 'n':
      case 'I':
      case 'J':
      case 'K':
      case 'L':
      case 'M':
      case 'N':
      case 'O':
      case 'P': is_num = true; continue;
      case 'i': is_num = is_symbolic = true; continue;
      case 'x':
      case 'v': ap->is_fp = true; continue;
      case 'f': ap->is_x87 = true; continue;
      case 'X': ap->is_fp = true;
      case 'g': ap->is_mem = ap->is_gp = is_num = true; continue;
      case 'a': fixed_reg(&reg, REG_AX, tok); continue;
      case 'b': fixed_reg(&reg, REG_BX, tok); continue;
      case 'c': fixed_reg(&reg, REG_CX, tok); continue;
      case 'd': fixed_reg(&reg, REG_DX, tok); continue;
      case 'S': fixed_reg(&reg, REG_SI, tok); continue;
      case 'D': fixed_reg(&reg, REG_DI, tok); continue;
      case 't': fixed_reg(&reg, REG_X87_ST0, tok); continue;
      case 'u': fixed_reg(&reg, REG_X87_ST1, tok); continue;
      }
      if (Isdigit(*p)) {
        char *pos;
        match_idx = strtoul(p, &pos, 10);
        p = pos - 1;
        continue;
      }
      error_tok(ap->constraint, "unknown constraint \"%c\"", *p);
    }
    if (is_num && is_integer(ap->arg->ty) && is_const_expr(ap->arg, &ap->val)) {
      ap->kind = ASMOP_NUM;
      continue;
    }
    if (is_symbolic) {
      ap->kind = ASMOP_SYMBOLIC;
      continue;
    }
    if (ap->is_mem && has_memop(ap->arg)) {
      ap->kind = ASMOP_MEM;
      continue;
    }
    if (ap->arg->kind == ND_VAR && ap->arg->var->asm_str) {
      Reg r = ident_reg(ap->arg->var->asm_str);
      if ((is_gp_reg(r) && !ap->is_gp) || (is_xmm_reg(r) && !ap->is_fp) ||
        (is_x87_reg(r) && !ap->is_x87))
        error_tok(ap->arg->tok, "constraint mismatch with variable register");
      fixed_reg(&reg, r, tok);
    }
    if (reg) {
      if (reg == REG_X87_ST0 && x87_clobber >= 1)
        ap->is_clobbered_x87 = true;
      else if (reg == REG_X87_ST1 && x87_clobber >= 2)
        ap->is_clobbered_x87 = true;

      ap->reg = reg;
      ap->kind = ASMOP_REG;
      asm_set_use(ap);
      continue;
    }
    if (match_idx >= 0) {
      if (match_idx >= asm_ops.cnt)
        error_tok(ap->arg->tok, "matching constraint exceeds operand count");
      asm_ops.data[match_idx]->match = ap;
    }
  }
}

static Reg find_free_reg(AsmParam *ap, Reg start, Reg end) {
  for (Reg i = start; i <= end; i++) {
    if (!asm_get_use(ap, i)) {
      ap->reg = i;
      return true;
    }
  }
  return false;
}

static void asm_assign_oprands2(Reg *reg_list, size_t bofs) {
  for (int i = 0; i < asm_ops.cnt; i++) {
    AsmParam *ap = asm_ops.data[i];
    if (ap->kind || !*((bool *)ap + bofs))
      continue;
    for (Reg *r = reg_list; *r != REG_NULL; r++) {
      if (!asm_get_use(ap, *r)) {
        ap->reg = *r;
        ap->kind = ASMOP_REG;
        asm_set_use(ap);
        break;
      }
    }
  }
}

static void asm_assign_oprands(void) {
  static Reg highbyte[] = {REG_AX, REG_CX, REG_DX, REG_BX, REG_NULL};
  static Reg legacy[] = {REG_AX, REG_CX, REG_DX, REG_SI, REG_DI, REG_BX, REG_BP, REG_NULL};
  static Reg free[] = {
    REG_AX, REG_CX, REG_DX, REG_SI, REG_DI, REG_R8, REG_R9, REG_R10, REG_R11, REG_NULL};

  asm_assign_oprands2(highbyte, offsetof(AsmParam, is_gp_highbyte));
  asm_assign_oprands2(legacy, offsetof(AsmParam, is_gp_legacy));
  asm_assign_oprands2(free, offsetof(AsmParam, is_gp_free));

  for (int i = 0; i < asm_ops.cnt; i++) {
    AsmParam *ap = asm_ops.data[i];
    if (ap->kind)
      continue;
    if ((ap->is_fp && find_free_reg(ap, REG_XMM0, REG_XMM15)) ||
      (ap->is_x87 && find_free_reg(ap, REG_X87_ST0, REG_X87_ST7)) ||
      (ap->is_gp && find_free_reg(ap, REG_AX, REG_R15))) {
      ap->kind = ASMOP_REG;
      asm_set_use(ap);
      continue;
    }
    if (ap->is_mem && find_free_reg(ap, REG_AX, REG_R15)) {
      ap->kind = ASMOP_MEM;
      asm_set_use(ap);
      continue;
    }
    error_tok(ap->arg->tok, "cannot assign register");
  }
}

static Reg acquire_gp(bool *use1, bool *use2, Token *tok) {
  for (Reg r = REG_AX; r <= REG_R15; r++) {
    if (use1[r] || (use2 && use2[r]))
      continue;
    use1[r] = true;
    if (use2)
      use2[r] = true;
    return r;
  }
  error_tok(tok, "out of registers");
}

static void ptr_transfrom(Node **node) {
  Node *arg = *node;
  add_type(arg);
  if (is_array(arg->ty))
    *node = new_cast(arg, pointer_to(arg->ty->base));
  else if (arg->ty->kind == TY_FUNC)
    *node = new_cast(arg, pointer_to(arg->ty));
}

void prepare_inline_asm(Node *node) {
  for (int i = 0; i < REG_END; i++)
    asm_use.in[i] = asm_use.out[i] = false;

  asm_fill_ops(node);

  int x87_clobber = 0;
  asm_clobbers(node->asm_clobbers, &x87_clobber);

  asm_constraint(node->asm_inputs, x87_clobber);
  asm_constraint(node->asm_outputs, 0);

  asm_assign_oprands();

  if (asm_use.in[REG_BP] || asm_use.out[REG_BP])
    node->alt_frame_ptr = acquire_gp(asm_use.in, asm_use.out, node->tok);
  if (asm_use.in[REG_BX] || asm_use.out[REG_BX])
    node->alt_frame_ptr2 = acquire_gp(asm_use.in, asm_use.out, node->tok);

  if (node->asm_outputs) {
    if (node->alt_frame_ptr && !asm_use.out[REG_BP])
      node->output_tmp_gp = REG_BP;
    else if (node->alt_frame_ptr2 && !asm_use.out[REG_BX])
      node->output_tmp_gp = REG_BX;
    else
      node->output_tmp_gp = acquire_gp(asm_use.out, NULL, node->tok);
  }

  for (Reg r = REG_R12; r <= REG_BP; r++)
    if (asm_use.in[r] || asm_use.out[r])
      node->clobber_mask |= 1U << r;

  for (AsmParam *ap = node->asm_outputs; ap; ap = ap->next) {
    if (ap->kind == ASMOP_REG)
      ptr_transfrom(&ap->arg);
    if (has_memop(ap->arg))
      continue;
    ap->ptr = new_lvar(NULL, pointer_to(ap->arg->ty));
  }
  for (AsmParam *ap = node->asm_inputs; ap; ap = ap->next) {
    if (ap->kind == ASMOP_REG)
      ptr_transfrom(&ap->arg);
    if (has_memop(ap->arg))
      continue;
    if (ap->kind == ASMOP_REG)
      ap->var = new_lvar(NULL, ap->arg->ty);
    else if (ap->kind == ASMOP_MEM)
      ap->ptr = new_lvar(NULL, pointer_to(ap->arg->ty));
  }
}

static char *reg_high_byte(Reg reg) {
  switch (reg) {
  case REG_AX: return "%ah";
  case REG_BX: return "%bh";
  case REG_CX: return "%ch";
  case REG_DX: return "%dh";
  }
  return regs[reg][0];
}

static char *reg_sz(Reg reg, int sz) {
  if (is_gp_reg(reg)) {
    switch (sz) {
    case 1: return regs[reg][0];
    case 2: return regs[reg][1];
    case 4: return regs[reg][2];
    case 8: return regs[reg][3];
    }
  }
  internal_error();
}

static void asm_gen_operands(void) {
  for (int i = 0; i < asm_ops.cnt; i++) {
    AsmParam *ap = asm_ops.data[i];
    if (ap->ptr) {
      Node var_node = {.kind = ND_VAR, .var = ap->ptr, .tok = ap->arg->tok};
      Node addr_node = {.kind = ND_ADDR, .lhs = ap->arg, .tok = ap->arg->tok};
      Node assign_node = {
        .kind = ND_ASSIGN, .lhs = &var_node, .rhs = &addr_node, .tok = ap->arg->tok};
      add_type(&assign_node);
      gen_void_expr(&assign_node);
      continue;
    }
    if (ap->var) {
      Node var_node = {.kind = ND_VAR, .var = ap->var, .tok = ap->arg->tok};
      Node assign_node = {
        .kind = ND_ASSIGN, .lhs = &var_node, .rhs = ap->arg, .tok = ap->arg->tok};
      add_type(&assign_node);
      gen_void_expr(&assign_node);
      continue;
    }
  }
}

static char *alt_ptr(char *reg) {
  if (asm_alt_ptr.rbp && !strcmp("%rbp", reg))
    return asm_alt_ptr.rbp;
  if (asm_alt_ptr.rbx && !strcmp("%rbx", reg))
    return asm_alt_ptr.rbx;
  return reg;
}

static void asm_gen_ptr(AsmParam *ap, char *ofs, char **ptr, Reg tmpreg) {
  if (is_memop(ap->arg, ofs, ptr, true)) {
    *ptr = alt_ptr(*ptr);
    return;
  }
  if (ap->ptr) {
    snprintf(ofs, STRBUF_SZ, "0");
    *ptr = regs[tmpreg][3];
    Printftn("movq %d(%s), %s", ap->ptr->ofs, alt_ptr(ap->ptr->ptr), *ptr);
    return;
  }
  if (ap->var) {
    snprintf(ofs, STRBUF_SZ, "%d", ap->var->ofs);
    *ptr = alt_ptr(ap->var->ptr);
    return;
  }
  internal_error();
}

static void asm_reg_input(AsmParam *ap, Reg tmpreg) {
  char ofs[STRBUF_SZ], *ptr = NULL;
  asm_gen_ptr(ap, ofs, &ptr, tmpreg);

  if (is_gp_reg(ap->reg)) {
    switch (ap->arg->ty->size) {
    case 1: Printftn("movb %s(%s), %s", ofs, ptr, regs[ap->reg][0]); return;
    case 2: Printftn("movw %s(%s), %s", ofs, ptr, regs[ap->reg][1]); return;
    case 4: Printftn("movl %s(%s), %s", ofs, ptr, regs[ap->reg][2]); return;
    case 8: Printftn("movq %s(%s), %s", ofs, ptr, regs[ap->reg][3]); return;
    }
  }
  if (is_xmm_reg(ap->reg)) {
    switch (ap->arg->ty->size) {
    case 4: Printftn("movss %s(%s), %%xmm%d", ofs, ptr, ap->reg - REG_XMM0); return;
    case 8: Printftn("movsd %s(%s), %%xmm%d", ofs, ptr, ap->reg - REG_XMM0); return;
    }
  }
  if (is_x87_reg(ap->reg)) {
    switch (ap->arg->ty->size) {
    case 4: Printftn("flds %s(%s)", ofs, ptr); return;
    case 8: Printftn("fldl %s(%s)", ofs, ptr); return;
    case 16: Printftn("fldt %s(%s)", ofs, ptr); return;
    }
  }
  error_tok(ap->arg->tok, "unsupported operand size");
}

static void asm_reg_output(AsmParam *ap, Reg tmpreg) {
  char ofs[STRBUF_SZ], *ptr = NULL;
  asm_gen_ptr(ap, ofs, &ptr, tmpreg);

  if (is_gp_reg(ap->reg)) {
    switch (ap->arg->ty->size) {
    case 1: Printftn("movb %s, %s(%s)", regs[ap->reg][0], ofs, ptr); return;
    case 2: Printftn("movw %s, %s(%s)", regs[ap->reg][1], ofs, ptr); return;
    case 4: Printftn("movl %s, %s(%s)", regs[ap->reg][2], ofs, ptr); return;
    case 8: Printftn("movq %s, %s(%s)", regs[ap->reg][3], ofs, ptr); return;
    }
  }
  if (is_xmm_reg(ap->reg)) {
    switch (ap->arg->ty->size) {
    case 4: Printftn("movss %%xmm%d, %s(%s)", ap->reg - REG_XMM0, ofs, ptr); return;
    case 8: Printftn("movsd %%xmm%d, %s(%s)", ap->reg - REG_XMM0, ofs, ptr); return;
    }
  }
  if (is_x87_reg(ap->reg)) {
    switch (ap->arg->ty->size) {
    case 4: Printftn("fstps %s(%s)", ofs, ptr); return;
    case 8: Printftn("fstpl %s(%s)", ofs, ptr); return;
    case 16: Printftn("fstpt %s(%s)", ofs, ptr); return;
    }
  }
  error_tok(ap->arg->tok, "unsupported operand size");
}

static bool asm_bitfield_out(AsmParam *ap, Reg tmpreg) {
  if (!is_bitfield(ap->arg))
    return false;

  Member *mem = ap->arg->member;
  char *val = regs[ap->reg][3];
  char *tmp = regs[tmpreg][3];

  Printftn("shl $%d, %s", (64 - mem->bit_width), val);
  Printftn("shr $%d, %s", (64 - mem->bit_width - mem->bit_offset), val);

  if (mem->bit_width + mem->bit_offset != 64) {
    Printftn("movq %d(%s), %s", ap->ptr->ofs, alt_ptr(ap->ptr->ptr), tmp);
    Printftn("mov (%s), %s", tmp, reg_sz(tmpreg, mem->ty->size));
    Printftn("shr $%d, %s", (mem->bit_width + mem->bit_offset), tmp);
    Printftn("shl $%d, %s", (mem->bit_width + mem->bit_offset), tmp);
    Printftn("or %s, %s", tmp, val);
  }
  if (mem->bit_offset) {
    Printftn("movq %d(%s), %s", ap->ptr->ofs, alt_ptr(ap->ptr->ptr), tmp);
    Printftn("mov (%s), %s", tmp, reg_sz(tmpreg, mem->ty->size));
    Printftn("shl $%d, %s", (64 - mem->bit_offset), tmp);
    Printftn("shr $%d, %s", (64 - mem->bit_offset), tmp);
    Printftn("or %s, %s", tmp, val);
  }
  Printftn("movq %d(%s), %s", ap->ptr->ofs, alt_ptr(ap->ptr->ptr), tmp);
  Printftn("mov %s, (%s)", reg_sz(ap->reg, mem->ty->size), tmp);
  return true;
}

static Reg get_input_reg(AsmParam *ap) {
  if (ap->kind == ASMOP_REG && *ap->constraint->str != '=')
    return ap->reg;
  return REG_NULL;
}

static void asm_xmm_inputs(void) {
  for (int i = 0; i < asm_ops.cnt; i++) {
    AsmParam *ap = asm_ops.data[i];
    if (is_xmm_reg(get_input_reg(ap)))
      asm_reg_input(ap, REG_DX);
  }
}

static void asm_gp_inputs(void) {
  for (int i = 0; i < asm_ops.cnt; i++) {
    AsmParam *ap = asm_ops.data[i];
    if (ap->kind == ASMOP_MEM && ap->reg)
      Printftn("movq %d(%s), %s", ap->ptr->ofs, alt_ptr(ap->ptr->ptr), regs[ap->reg][3]);
    if (is_gp_reg(get_input_reg(ap)))
      asm_reg_input(ap, ap->reg);
  }
}

static int asm_x87_inputs(void) {
  AsmParam *sort_buf[8] = {0};

  for (int i = 0; i < asm_ops.cnt; i++) {
    AsmParam *ap = asm_ops.data[i];
    Reg reg = get_input_reg(ap);
    if (is_x87_reg(reg))
      sort_buf[reg - REG_X87_ST0] = ap;
  }

  int cnt = 0;
  for (int i = 0; i < 8; i++) {
    AsmParam *ap = sort_buf[7 - i];
    if (ap) {
      if (!ap->is_clobbered_x87)
        cnt++;
      asm_reg_input(ap, REG_DX);
    }
  }
  return cnt;
}

static void asm_x87_outputs(Node *node, int in_cnt) {
  AsmParam *sort_buf[8] = {0};
  int out_cnt = 0;
  for (AsmParam *ap = node->asm_outputs; ap; ap = ap->next) {
    if (is_x87_reg(ap->reg)) {
      sort_buf[ap->reg - REG_X87_ST0] = ap;
      out_cnt++;
    }
  }
  out_cnt = MAX(out_cnt, in_cnt);
  for (int i = 0; i < out_cnt; i++) {
    AsmParam *ap = sort_buf[i];
    if (ap)
      asm_reg_output(ap, node->output_tmp_gp);
    else
      Printstn("fstp %%st(0)");
  }
}

static void asm_outputs(Node *node) {
  Reg freereg = node->output_tmp_gp;

  for (AsmParam *ap = node->asm_outputs; ap; ap = ap->next) {
    if (ap->kind == ASMOP_REG) {
      if (is_x87_reg(ap->reg))
        continue;
      if (asm_bitfield_out(ap, freereg))
        continue;
      asm_reg_output(ap, freereg);
      continue;
    }
    if (ap->kind == ASMOP_FLAG) {
      char ofs[STRBUF_SZ], *ptr;
      asm_gen_ptr(ap, ofs, &ptr, freereg);
      if (ap->arg->ty->size != 1)
        Printftn("mov%s $0, %s(%s)", size_suffix(ap->arg->ty->size), ofs, ptr);
      Printftn("set%s %s(%s)", ap->flag, ofs, ptr);
      continue;
    }
  }
}

static bool named_op(char *p, int len, char *str, char **rest) {
  if (len && !strncmp(&p[1], str, len) && p[1 + len] == ']') {
    *rest = &p[2 + len];
    return true;
  }
  return false;
}

static AsmParam *find_op(char *p, char **rest, Token *tok, bool is_label) {
  if (*p == '[') {
    for (int i = 0; i < asm_ops.cnt; i++) {
      AsmParam *op = asm_ops.data[i];
      if (is_label && op->arg->label)
        if (named_op(p, strlen(op->arg->label), op->arg->label, rest))
          return op;

      if (!is_label && op->name)
        if (named_op(p, op->name->len, op->name->loc, rest))
          return op;
    }
  } else if (Isdigit(*p)) {
    unsigned long idx = strtoul(p, rest, 10);
    if (idx < asm_ops.cnt)
      return asm_ops.data[idx];
  }
  error_tok(tok, "operand not found");
}

static void asm_body(Node *node) {
  for (char *p = node->asm_str->str; *p != '\0';) {
    if (*p != '%') {
      fputc(*p, output_file);
      p++;
      continue;
    }
    p++;

    if (*p == '%') {
      fputc('%', output_file);
      p++;
      continue;
    }
    if (*p == 'l' && (p[1] == '[' || Isdigit(p[1]))) {
      AsmParam *ap = find_op(p + 1, &p, node->asm_str, true);
      if (!ap->arg->unique_label)
        error_tok(ap->arg->tok, "not a label");

      if (node->asm_outputs)
        fprintf(output_file, "%df", ap->label_id);
      else
        fprintf(output_file, "%s", ap->arg->unique_label);
      continue;
    }

    char mod = 0;
    switch (*p) {
    case 'h':
    case 'b':
    case 'w':
    case 'k':
    case 'q':
    case 'c':
      mod = *p;
      p++;
    }
    if (*p == '[' || Isdigit(*p)) {
      AsmParam *ap = find_op(p, &p, node->asm_str, false);
      char *punct = (mod == 'c') ? "" : "$";

      switch (ap->kind) {
      case ASMOP_NUM:
        fprintf(output_file, "%s%"PRIi64, punct, ap->val);
        continue;
      case ASMOP_SYMBOLIC:{
        if (ap->arg->kind == ND_VAR && !ap->arg->var->is_local && !opt_fpic) {
          fprintf(output_file, "%s\"%s\"", punct, asm_name(ap->arg->var));
          continue;
        }
        char ofs[STRBUF_SZ], *ptr;
        if (ap->arg->kind == ND_ADDR &&
          is_memop(ap->arg->lhs, ofs, &ptr, true) && !strcmp(ptr, "%rip")) {
          fprintf(output_file, "%s%s", punct, ofs);
          continue;
        }
        break;
      }
      case ASMOP_MEM: {
        char ofs[STRBUF_SZ], *ptr;
        if (is_memop(ap->arg, ofs, &ptr, true)) {
          fprintf(output_file, "%s(%s)", ofs, alt_ptr(ptr));
          continue;
        }
        if (ap->reg) {
          fprintf(output_file, "(%s)", regs[ap->reg][3]);
          continue;
        }
        break;
      }
      case ASMOP_REG:
        if (is_xmm_reg(ap->reg)) {
          fprintf(output_file, "%%xmm%d", ap->reg - REG_XMM0);
          continue;
        }
        if (is_x87_reg(ap->reg)) {
          fprintf(output_file, "%%st(%d)", ap->reg - REG_X87_ST0);
          continue;
        }
        if (is_gp_reg(ap->reg)) {
          char *regname = NULL;
          switch (mod) {
          case 'h': regname = reg_high_byte(ap->reg); break;
          case 'b': regname = regs[ap->reg][0]; break;
          case 'w': regname = regs[ap->reg][1]; break;
          case 'k': regname = regs[ap->reg][2]; break;
          case 'q': regname = regs[ap->reg][3]; break;
          default: regname = reg_sz(ap->reg, ap->arg->ty->size); break;
          }
          fprintf(output_file, "%s", regname);
          continue;
        }
      }
      error_tok(ap->arg->tok, "invalid operand");
    }
    error_tok(node->asm_str, "unknown modifier \"%c\"", *p);
  }
  fprintf(output_file, "\n");
}

static void asm_push_clobbers(Node *node) {
  for (Reg r = REG_R12; r <= REG_R15; r++)
    if (node->clobber_mask & (1U << r))
      push_from(regs[r][3]);
}

static void asm_pop_clobbers(Node *node) {
  for (Reg r = REG_R15; r >= REG_R12; r--)
    if (node->clobber_mask & (1U << r))
      pop(regs[r][3]);
}

static char *asm_push_frame_ptr(Node *node) {
  char *prev = lvar_ptr;

  if (node->alt_frame_ptr) {
    asm_alt_ptr.rbp = regs[node->alt_frame_ptr][3];
    if (!strcmp(lvar_ptr, "%rbp"))
      lvar_ptr = asm_alt_ptr.rbp;
    Printftn("movq %%rbp, %s", asm_alt_ptr.rbp);
  }
  if (node->alt_frame_ptr2) {
    asm_alt_ptr.rbx = regs[node->alt_frame_ptr2][3];
    if (!strcmp(lvar_ptr, "%rbx"))
      lvar_ptr = asm_alt_ptr.rbx;
    Printftn("movq %%rbx, %s", asm_alt_ptr.rbx);
  }

  if (node->alt_frame_ptr)
    push_from("%rbp");
  if (node->alt_frame_ptr2)
    push_from("%rbx");
  return prev;
}

static void asm_pop_frame_ptr(Node *node, char *prev) {
  if (node->alt_frame_ptr2)
    pop("%rbx");
  if (node->alt_frame_ptr)
    pop("%rbp");
  lvar_ptr = prev;
}

static void gen_asm(Node *node) {
  if (!node->asm_outputs && !node->asm_inputs &&
    !node->asm_clobbers && !node->asm_labels) {
    Printftn("%s", node->asm_str->str);
    return;
  }
  asm_alt_ptr.rbp = asm_alt_ptr.rbx = NULL;
  asm_fill_ops(node);
  asm_gen_operands();

  int x87_depth = asm_x87_inputs();
  asm_xmm_inputs();

  asm_push_clobbers(node);
  char *prev_lvar_ptr = asm_push_frame_ptr(node);

  asm_gp_inputs();

  asm_fill_ops2(node);
  asm_body(node);

  int fallthrough_label = asm_ops.cnt;
  int meet_label = asm_ops.cnt + 1;
  bool is_goto_with_output = node->asm_outputs && node->asm_labels;
  if (is_goto_with_output) {
    char *tmp_gp = regs[node->output_tmp_gp][3];
    Printftn("lea %df(%%rip), %s; jmp %df", fallthrough_label, tmp_gp, meet_label);
    for (AsmParam *ap = node->asm_labels; ap; ap = ap->next) {
      Printftn("%d:", ap->label_id);
      Printftn("lea %s(%%rip), %s", ap->arg->unique_label, tmp_gp);
      Printftn("jmp %df", meet_label);
    }
    Printftn("%d:", meet_label);
    Printftn("push %s", tmp_gp);
  }

  asm_outputs(node);
  asm_x87_outputs(node, x87_depth);

  clobber_all_regs();
  asm_pop_frame_ptr(node, prev_lvar_ptr);
  asm_pop_clobbers(node);

  if (is_goto_with_output) {
    Printstn("pop %%rax");
    Printstn("jmp *%%rax");
    Printftn("%d:", fallthrough_label);
  }
}

static int get_lvar_align(Scope *scp, int align) {
  for (Obj *var = scp->locals; var; var = var->next) {
    if (var->pass_by_stack)
      continue;
    align = MAX(align, var->align);
  }
  for (Scope *sub = scp->children; sub; sub = sub->sibling_next)
    align = MAX(align, get_lvar_align(sub, align));
  return align;
}

static int assign_lvar_offsets(Scope *sc, int bottom) {
  for (Obj *var = sc->locals; var; var = var->next) {
    if (var->pass_by_stack) {
      var->ofs = var->stack_offset + 16;
      var->ptr = "%rbp";
      continue;
    }

    // AMD64 System V ABI has a special alignment rule for an array of
    // length at least 16 bytes. We need to align such array to at least
    // 16-byte boundaries. See p.14 of
    // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
    int align;
    if (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      align = MAX(16, var->align);
    else if (var->ty->kind == TY_VLA)
      align = 8;
    else
      align = var->align;

    bottom += var->ty->size;
    bottom = align_to(bottom, align);
    var->ofs = -bottom;
    var->ptr = lvar_ptr;
  }

  int max_depth = bottom;
  for (Scope *sub = sc->children; sub; sub = sub->sibling_next) {
    int sub_depth= assign_lvar_offsets(sub, bottom);
    if (dont_reuse_stack)
      bottom = max_depth = sub_depth;
    else
      max_depth = MAX(max_depth, sub_depth);
  }
  return max_depth;
}

static void emit_symbol(Obj *var) {
  if (var->is_static)
    Printftn(".local \"%s\"", asm_name(var));
  else if (var->is_weak)
    Printftn(".weak \"%s\"", asm_name(var));
  else
    Printftn(".globl \"%s\"", asm_name(var));

  if (var->alias_name)
    Printftn(".set \"%s\", \"%s\"", var->name, var->alias_name);

  char *vis_mode = var->visibility ? var->visibility : opt_visibility;
  if (vis_mode && (!strcmp(vis_mode, "hidden") ||
    !strcmp(vis_mode, "internal") || !strcmp(vis_mode, "protected")))
    Printftn(".%s \"%s\"", vis_mode, asm_name(var));
}

static void emit_data(Obj *var) {
  emit_symbol(var);

  int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
    ? MAX(16, var->align) : var->align;

  if (var->is_tentative && !var->is_tls && !var->section_name) {
    if (var->ty->kind == TY_ARRAY && var->ty->size < 0)
      var->ty->size = var->ty->base->size;

    // Common symbol
    if (!var->is_weak && opt_fcommon) {
      Printftn(".comm \"%s\", %"PRIi64", %d", asm_name(var), var->ty->size, align);
      return;
    }
  }
  bool use_rodata = !opt_fpic && is_const_var(var);

  if (var->section_name)
    Printfts(".section \"%s\"", var->section_name);
  else if (var->is_tls)
    Printfts(".section .%s", var->init_data ? "tdata" : "tbss");
  else if (use_rodata)
    Printsts(".section .rodata");
  else
    Printfts(".section .%s", var->init_data ? "data" : "bss");

  if (opt_data_sections)
    Printf(".\"%s\"", asm_name(var));

  if (var->is_tls)
    Prints(",\"awT\"");
  else if (use_rodata)
    Prints(",\"a\"");
  else
    Prints(",\"aw\"");

  if (!var->init_data && !use_rodata)
    Printssn(",@nobits");
  else
    Printssn(",@progbits");

  Printftn(".size \"%s\", %"PRIi64, asm_name(var), var->ty->size);
  Printftn(".align %d", align);
  Printfsn("\"%s\":", asm_name(var));

  if (!var->init_data) {
    Printftn(".zero %"PRIi64, var->ty->size);
    return;
  }
  Relocation *rel = var->rel;
  int pos = 0;
  while (pos < var->ty->size) {
    if (rel && rel->offset == pos) {
      Printftn(".quad \"%s\"%+ld", *rel->label, rel->addend);
      rel = rel->next;
      pos += 8;
    } else {
      Printftn(".byte %d", var->init_data[pos++]);
    }
  }
}

static void store_fp(int r, int sz, int ofs, char *ptr) {
  switch (sz) {
  case 4:
    Printftn("movss %%xmm%d, %d(%s)", r, ofs, ptr);
    return;
  case 8:
    Printftn("movsd %%xmm%d, %d(%s)", r, ofs, ptr);
    return;
  }
  internal_error();
}

static void store_gp2(char **reg, int sz, int dofs, char *dptr) {
  for (int ofs = 0;;) {
    int rem = sz - ofs;
    int p2 = (rem >= 8) ? 8 : (rem >= 4) ? 4 : (rem >= 2) ? 2 : 1;
    switch (p2) {
    case 1: Printftn("mov %s, %d(%s)", reg[0], ofs + dofs, dptr); break;
    case 2: Printftn("mov %s, %d(%s)", reg[1], ofs + dofs, dptr); break;
    case 4: Printftn("mov %s, %d(%s)", reg[2], ofs + dofs, dptr); break;
    case 8: Printftn("mov %s, %d(%s)", reg[3], ofs + dofs, dptr); break;
    }
    ofs += p2;
    if (ofs >= sz)
      return;
    Printftn("shr $%d, %s", p2 * 8, reg[3]);
  }
}

static void store_gp(int r, int sz, int ofs, char *ptr) {
  store_gp2((char *[]){argreg8[r], argreg16[r], argreg32[r], argreg64[r]}, sz, ofs, ptr);
}

void emit_text(Obj *fn) {
  for (Obj *var = fn->static_lvars; var; var = var->next)
    emit_data(var);

  emit_symbol(fn);

  if (opt_func_sections)
    Printftn(".section .text.\"%s\",\"ax\",@progbits", asm_name(fn));
  else
    Printstn(".text");
  Printftn(".type \"%s\", @function", asm_name(fn));
  Printfsn("\"%s\":", asm_name(fn));

  bool rtn_by_stk = fn->ty->return_ty->size > 16;
  int gp_count = rtn_by_stk;
  int fp_count = 0;
  int arg_stk_size = calling_convention(fn->ty->param_list, &gp_count, &fp_count, NULL);

  int lvar_align = get_lvar_align(fn->ty->scopes, 16);
  lvar_ptr = (lvar_align > 16) ? "%rbx" : "%rbp";
  current_fn = fn;
  rtn_label = NULL;

  // Prologue
  Printstn("push %%rbp");
  Printstn("mov %%rsp, %%rbp");
  if (lvar_align > 16) {
    Printstn("push %%rbx");
    Printftn("and $-%d, %%rsp", lvar_align);
    Printstn("mov %%rsp, %%rbx");
  }

  long stack_alloc_loc = resrvln();

  lvar_stk_sz = 0;

  // Save arg registers if function is variadic
  if (fn->ty->is_variadic) {
    va_gp_start = gp_count * 8;
    va_fp_start = fp_count * 16 + 48;
    va_st_start = arg_stk_size + 16;
    lvar_stk_sz += 176;

    switch (gp_count) {
    case 0: Printftn("movq %%rdi, -176(%s)", lvar_ptr);
    case 1: Printftn("movq %%rsi, -168(%s)", lvar_ptr);
    case 2: Printftn("movq %%rdx, -160(%s)", lvar_ptr);
    case 3: Printftn("movq %%rcx, -152(%s)", lvar_ptr);
    case 4: Printftn("movq %%r8, -144(%s)", lvar_ptr);
    case 5: Printftn("movq %%r9, -136(%s)", lvar_ptr);
    }
    if (fp_count < 8) {
      Printstn("test %%al, %%al");
      Printstn("je 1f");
      switch (fp_count) {
      case 0: Printftn("movaps %%xmm0, -128(%s)", lvar_ptr);
      case 1: Printftn("movaps %%xmm1, -112(%s)", lvar_ptr);
      case 2: Printftn("movaps %%xmm2, -96(%s)", lvar_ptr);
      case 3: Printftn("movaps %%xmm3, -80(%s)", lvar_ptr);
      case 4: Printftn("movaps %%xmm4, -64(%s)", lvar_ptr);
      case 5: Printftn("movaps %%xmm5, -48(%s)", lvar_ptr);
      case 6: Printftn("movaps %%xmm6, -32(%s)", lvar_ptr);
      case 7: Printftn("movaps %%xmm7, -16(%s)", lvar_ptr);
      }
      Printssn("1:");
    }
  }

  if (fn->dealloc_vla) {
    vla_base_ofs = lvar_stk_sz += 8;
    Printftn("mov %%rsp, -%d(%s)", vla_base_ofs, lvar_ptr);
  }

  if (rtn_by_stk) {
    rtn_ptr_ofs = lvar_stk_sz += 8;
    Printftn("mov %s, -%d(%s)", argreg64[0], rtn_ptr_ofs, lvar_ptr);
  }

  lvar_stk_sz = assign_lvar_offsets(fn->ty->scopes, lvar_stk_sz);
  peak_stk_usage = lvar_stk_sz = align_to(lvar_stk_sz, 8);

  // Save passed-by-register arguments to the stack
  int gp = rtn_by_stk, fp = 0;
  for (Obj *var = fn->ty->param_list; var; var = var->param_next) {
    if (var->pass_by_stack)
      continue;

    Type *ty = var->ty;

    switch (ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      assert(ty->size <= 16);
      if (has_flonum1(ty))
        store_fp(fp++, MIN(8, ty->size), var->ofs, var->ptr);
      else
        store_gp(gp++, MIN(8, ty->size), var->ofs, var->ptr);

      if (ty->size > 8) {
        if (has_flonum2(ty))
          store_fp(fp++, ty->size - 8, var->ofs + 8, var->ptr);
        else
          store_gp(gp++, ty->size - 8, var->ofs + 8, var->ptr);
      }
      break;
    case TY_FLOAT:
    case TY_DOUBLE:
      store_fp(fp++, ty->size, var->ofs, var->ptr);
      break;
    default:
      store_gp(gp++, ty->size, var->ofs, var->ptr);
    }
  }

  // Emit code
  gen_stmt(fn->body);
  assert(tmp_stack.depth == 0);

  if (peak_stk_usage)
    insrtln("sub $%d, %%rsp", stack_alloc_loc, align_to(peak_stk_usage, 16));

  // Epilogue
  gen_label_ph(rtn_label, !strcmp(asm_name(fn), "main"));
  if (lvar_align > 16)
    Printstn("mov -8(%%rbp), %%rbx");
  Printstn("leave");
  Printstn("ret");
  Printftn(".size \"%s\", .-\"%s\"", asm_name(fn), asm_name(fn));
}

typedef struct {
  char *buf;
  size_t buflen;
} FuncObj;

void *prepare_funcgen(void) {
  FuncObj *obj = malloc(sizeof(FuncObj));
  output_file = open_memstream(&obj->buf, &obj->buflen);
  return obj;
}

void end_funcgen(void) {
  fclose(output_file);
  output_file = NULL;
}

int codegen(Obj *prog, FILE *out) {
  output_file = out;

  if (opt_g) {
    File **files = get_input_files();
    for (int i = 0; files[i]; i++)
      Printftn(".file %d \"%s\"", files[i]->file_no, files[i]->name);
  }

  for (Obj *var = prog; var; var = var->next) {
    if (var->asm_str) {
      Printfsn("%s", var->asm_str->str);
      continue;
    }
    if (!var->is_definition) {
      if (var->is_weak || var->alias_name)
        emit_symbol(var);
      continue;
    }
    if (var->ty->kind == TY_FUNC) {
      if (var->is_live)
        fwrite(((FuncObj *)var->output)->buf, ((FuncObj *)var->output)->buflen, 1, out);
      continue;
    }
    emit_data(var);
    continue;
  }
  Printstn(".section  .note.GNU-stack,\"\",@progbits");
  return 0;
}

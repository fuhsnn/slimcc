#include "slimcc.h"

#define GP_MAX 6
#define FP_MAX 8

#define GP_SLOTS 6
#define FP_SLOTS 6

#define STRBUF_SZ 192
#define STRBUF_SZ2 208

typedef enum {
  REGSZ_8 = 0,
  REGSZ_16,
  REGSZ_32,
  REGSZ_64,
} RegSz;

typedef enum {
  REG_X64_NULL = 0,
  REG_X64_AX,
  REG_X64_CX,
  REG_X64_DX,
  REG_X64_SI,
  REG_X64_DI,
  REG_X64_R8,
  REG_X64_R9,
  REG_X64_R10,
  REG_X64_R11,
  REG_X64_R12,
  REG_X64_R13,
  REG_X64_R14,
  REG_X64_R15,
  REG_X64_BX,
  REG_X64_BP,
  REG_X64_SP,
  REG_X64_XMM0,
  REG_X64_XMM1,
  REG_X64_XMM2,
  REG_X64_XMM3,
  REG_X64_XMM4,
  REG_X64_XMM5,
  REG_X64_XMM6,
  REG_X64_XMM7,
  REG_X64_XMM8,
  REG_X64_XMM9,
  REG_X64_XMM10,
  REG_X64_XMM11,
  REG_X64_XMM12,
  REG_X64_XMM13,
  REG_X64_XMM14,
  REG_X64_XMM15,
  REG_X64_X87_ST0,
  REG_X64_X87_ST1,
  REG_X64_X87_ST2,
  REG_X64_X87_ST3,
  REG_X64_X87_ST4,
  REG_X64_X87_ST5,
  REG_X64_X87_ST6,
  REG_X64_X87_ST7,
  REG_X64_END
} Reg;

static char *regs[REG_X64_XMM0][4] = {
  [REG_X64_NULL] = {"null", "null", "null", "null"},
  [REG_X64_SP] = {"%spl", "%sp", "%esp", "%rsp"},
  [REG_X64_BP] = {"%bpl", "%bp", "%ebp", "%rbp"},
  [REG_X64_AX] = {"%al", "%ax", "%eax", "%rax"},
  [REG_X64_BX] = {"%bl", "%bx", "%ebx", "%rbx"},
  [REG_X64_CX] = {"%cl", "%cx", "%ecx", "%rcx"},
  [REG_X64_DX] = {"%dl", "%dx", "%edx", "%rdx"},
  [REG_X64_SI] = {"%sil", "%si", "%esi", "%rsi"},
  [REG_X64_DI] = {"%dil", "%di", "%edi", "%rdi"},
  [REG_X64_R8] = {"%r8b", "%r8w", "%r8d", "%r8"},
  [REG_X64_R9] = {"%r9b", "%r9w", "%r9d", "%r9"},
  [REG_X64_R10] = {"%r10b", "%r10w", "%r10d", "%r10"},
  [REG_X64_R11] = {"%r11b", "%r11w", "%r11d", "%r11"},
  [REG_X64_R12] = {"%r12b", "%r12w", "%r12d", "%r12"},
  [REG_X64_R13] = {"%r13b", "%r13w", "%r13d", "%r13"},
  [REG_X64_R14] = {"%r14b", "%r14w", "%r14d", "%r14"},
  [REG_X64_R15] = {"%r15b", "%r15w", "%r15d", "%r15"},
};

static FILE *output_file;
static FILE *extref_file;

static char *argreg32[] = {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};
static char *argreg64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};

static Reg argreg[] = {REG_X64_DI, REG_X64_SI, REG_X64_DX, REG_X64_CX, REG_X64_R8, REG_X64_R9};

static char *tmpreg32[] = {"%edi", "%esi", "%r8d", "%r9d", "%r10d", "%r11d"};
static char *tmpreg64[] = {"%rdi", "%rsi", "%r8", "%r9", "%r10", "%r11"};

static char *rip = "%rip";
static char *rbp = "%rbp";
static char *rbx = "%rbx";

static Obj *current_fn;
static char *lvar_ptr;
static int va_gp_start;
static int va_fp_start;
static int va_st_start;
static int vla_base_ofs;
static int rtn_ptr_ofs;
static int lvar_stk_sz;
static int peak_stk_usage;
static int tmpbuf_sz;
static int64_t rtn_label;

static struct {
  bool in[REG_X64_END];
  bool out[REG_X64_END];
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

struct AsmContext {
  Reg output_tmp1;
  Reg output_tmp2;
  Reg frame_ptr1;
  Reg frame_ptr2;
  uint32_t clobber_mask;
};

typedef struct {
  char *buf;
  size_t buflen;
  char *extref_buf;
  size_t extref_buflen;
} FuncObj;

typedef struct {
  HashMap map;
  char *buf;
  size_t buflen;
} ExtRefs;

static ExtRefs *ext_refs;

static void load2(Type *ty, int sofs, char *sptr);
static void store2(Type *ty, int dofs, char *dptr);
static void store_gp2(char **r, int sz, int dofs, char *dptr);

static void gen_asm(Node *node);
static void gen_expr(Node *node);
static void gen_stmt(Node *node);
static bool gen_block_stmt(Node *node, bool is_reach);
static bool gen_if_stmt(Node *node, bool is_reach);
static void gen_void_expr(Node *node);
static void gen_void_assign(Node *node);
static bool gen_reachable_stmt(Node *node);
static bool gen_unreachable_stmt(Node *node);
static bool gen_expr_opt(Node *node);
static bool gen_addr_opt(Node *node);
static bool gen_cmp_opt_gp(Node *node, NodeKind *kind);
static bool gen_load_opt_gp(Node *node, Reg r);
static Node *bool_expr_opt(Node *node, bool *flip);
static void gen_mem_copy2(char *sofs, char *sptr, char *dofs, char *dptr, int sz);
static void load_val2(Type *ty, int64_t val, char *gp32, char *gp64);

static void imm_add(char *op, char *tmp, int64_t val);
static void imm_sub(char *op, char *tmp, int64_t val);
static void imm_and(char *op, char *tmp, int64_t val);
static void imm_cmp(char *op, char *tmp, int64_t val);
static char *arith_ins(NodeKind kind);
static void imm_tmpl(char *ins, char *op, int64_t val);

static bool is_asm_symbolic_arg(Node *node, char *punct);

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

static bool is_valid_vis(char *vis) {
  return vis &&
    (!strcmp(vis, "hidden") || !strcmp(vis, "internal") || !strcmp(vis, "protected"));
}

static bool check_extref(char *name) {
  if (hashmap_get(&ext_refs->map, name))
    return false;

  hashmap_put(&ext_refs->map, name, (void *)1);
  return true;
}

static char *asm_name(Obj *var) {
  char *asm_name = var->asm_name ? var->asm_name : var->name;

  if (!var->is_static && !var->is_definition &&
    (var->is_weak || is_valid_vis(var->visibility)) &&
    ext_refs && check_extref(var->name)) {
    if (!extref_file)
      extref_file = open_memstream(&ext_refs->buf, &ext_refs->buflen);

    if (var->is_weak)
      fprintf(extref_file, "\t.weak \"%s\"\n", asm_name);
    if (is_valid_vis(var->visibility))
      fprintf(extref_file, "\t.%s \"%s\"\n", var->visibility, asm_name);
  }
  return asm_name;
}

static int get_align(Obj *var) {
  if (var->ty->kind == TY_VLA)
    return 8;
  if (var->alt_align)
    return var->alt_align;
  if (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
    return MAX(16, var->ty->align);
  return var->ty->align;
}

static int64_t count(void) {
  static int64_t i = 1;
  return i++;
}

static char *goto_label(Node *node) {
  if (node->unique_label)
    return node->unique_label;
  if (node->default_label)
    return node->default_label->unique_label;
  internal_error();
}

static void name_labels(Node *node) {
  for (Node *n = node->goto_next; n; n = n->goto_next)
    n->unique_label = new_unique_name();
}

static void name_brk_cont(Node *node, char *bg, char *brk, char *cont, int64_t c) {
  snprintf(brk, STRBUF_SZ, ".L.brk.%"PRIi64, c);
  if (bg)
    snprintf(bg, STRBUF_SZ, ".L.begin.%"PRIi64, c);
  if (cont)
    snprintf(cont, STRBUF_SZ, ".L.cont.%"PRIi64, c);

  for (Node *n = node->goto_next; n; n = n->goto_next) {
    if (n->kind == ND_BREAK)
      n->unique_label = brk;
    else
      n->unique_label = cont;
  }
}

static char *tmpbuf(int sz) {
  tmpbuf_sz = MAX(tmpbuf_sz, sz);
  return "__BUF";
}

static bool is_gp_ty(Type *ty) {
  return is_integer(ty) || ty->kind == TY_PTR || ty->kind == TY_NULLPTR;
}

static bool is_scalar(Type *ty) {
  return is_gp_ty(ty) || is_flonum(ty) || ty->kind == TY_BITINT;
}

static int trailing_zero(uint64_t val) {
  int tz = 0;
  for (; tz < 64; tz++)
    if ((1LL << tz) == val)
      break;
  return tz;
}

static bool in_imm_range (int64_t val) {
  return val == (int32_t)val;
}

static bool use_rip(Obj *var) {
  if (var->is_static || !(opt_fpie || opt_fpic))
    return true;

  if (opt_fpie)
    return var->is_definition || (var->ty->kind != TY_FUNC && !var->is_weak);

  char *vis = var->visibility ? var->visibility : opt_visibility;
  if (vis && (!strcmp(vis, "hidden")))
    return var->is_definition || (var->ty->kind != TY_FUNC && !var->is_weak && var->visibility);

  return false;
}

static Node *skip_gp_cast(Node *node) {
  while (node->kind == ND_CAST && node->ty->size == node->lhs->ty->size &&
    node->ty->kind != TY_BOOL && is_gp_ty(node->ty) && is_gp_ty(node->lhs->ty))
    node = node->lhs;

  return node;
}

static bool eval_memop(Node *node, char *ofs, char **ptr, bool let_array, bool let_atomic) {
  int offset;
  Obj *var = eval_var_opt(node, &offset, let_array, let_atomic);
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

static bool is_plain_asm(Node *node) {
  return !node->asm_outputs && !node->asm_inputs &&
    !node->asm_clobbers && !node->asm_labels;
}

static bool is_direct_fncall(Node *lhs) {
  return lhs->kind == ND_VAR && lhs->var->ty->kind == TY_FUNC;
}

static RegSz bitwidth_to_regsz(int bits) {
  switch(bits) {
  case 64: return REGSZ_64;
  case 32: return REGSZ_32;
  case 16: return REGSZ_16;
  case 8: return REGSZ_8;
  }
  internal_error();
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

static int32_t ovf_headroom(Type *ty, NodeKind kind) {
  int32_t bits;
  switch (ty->kind) {
  case TY_BOOL: bits = 1; break;
  case TY_BITINT: bits = ty->bit_cnt; break;
  default: bits = ty->size * 8; break;
  }
  if (kind == ND_MUL)
    return bits * 2 + ty->is_unsigned;
  return bits + 1 + ty->is_unsigned;
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
    if (!opt_reuse_stack || current_fn->dont_reuse_stk) {
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
  insrtln("fldt %s(%s); fstpt %d(%s)", sl->loc, tmpbuf(10), lvar_ptr, sl->st_ofs, lvar_ptr);
  Printftn("fldt %d(%s); fstpt %s(%s)", sl->st_ofs, lvar_ptr, tmpbuf(10), lvar_ptr);
  return true;
}

static void push_bitint(int sz) {
  for (int ofs = align_to(sz, 8) - 8; ofs >= 0; ofs -= 8)
    push_tmpstack(SL_ST);
}

static int pop_bitint(int sz) {
  int pos = 0;
  for (int ofs = align_to(sz, 8) - 8; ofs >= 0; ofs -= 8) {
    Slot *sl = pop_tmpstack(1);
    pos = sl->st_ofs;
    insrtln("mov %d+%s(%s), %%rdx; mov %%rdx, %d(%s)", sl->loc,
      ofs, tmpbuf(sz), lvar_ptr, sl->st_ofs, lvar_ptr);
  }
  return pos;
}

static void push_by_ty(Type *ty) {
  switch (ty->kind) {
  case TY_LDOUBLE: push_x87(); return;
  case TY_DOUBLE:
  case TY_FLOAT: pushf(); return;
  case TY_BITINT: push_bitint(ty->size); return;
  default: push(); return;
  }
}

static void pop_by_ty(Type *ty) {
  switch (ty->kind) {
  case TY_LDOUBLE: pop_x87(); return;
  case TY_DOUBLE:
  case TY_FLOAT: popf(0); return;
  case TY_BITINT: {
    int pos = pop_bitint(ty->size);
    char sofs_buf[STRBUF_SZ];
    snprintf(sofs_buf, STRBUF_SZ, "%d", pos);
    gen_mem_copy2(sofs_buf, lvar_ptr, tmpbuf(ty->size), lvar_ptr, ty->size);
    return;
  }
  default: pop("%rax"); return;
  }
}

static void cast_extend_int32(Type *ty, char *from, char *to) {
  char *insn = ty->is_unsigned ? "movz" : "movs";
  switch (ty->size) {
  case 1: Printftn("%sbl %s, %s", insn, from, to); return;
  case 2: Printftn("%swl %s, %s", insn, from, to); return;
  case 4: Printftn("movl %s, %s", from, to); return;
  }
  internal_error();
}

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

static void gen_bitint_builtin_call2(char *fn) {
  strarray_push(&current_fn->refs, fn);
  Printftn("call %s", fn);
  clobber_all_regs();
}

static void gen_bitint_builtin_call(NodeKind kind) {
  static char *fn[] = {
    [ND_NEG] = "__slimcc_bitint_neg",
    [ND_BITNOT] = "__slimcc_bitint_bitnot",
    [ND_BITAND] = "__slimcc_bitint_bitand",
    [ND_BITOR] = "__slimcc_bitint_bitor",
    [ND_BITXOR] = "__slimcc_bitint_bitxor",
    [ND_SHL] = "__slimcc_bitint_shl",
    [ND_SHR] = "__slimcc_bitint_shr",
    [ND_SAR] = "__slimcc_bitint_shr",
    [ND_ADD] = "__slimcc_bitint_add",
    [ND_SUB] = "__slimcc_bitint_sub",
    [ND_MUL] = "__slimcc_bitint_mul",
    [ND_DIV] = "__slimcc_bitint_div",
    [ND_MOD] = "__slimcc_bitint_div",
  };
  gen_bitint_builtin_call2(fn[kind]);
}

static void gen_bitfield_load(Node *node, int ofs) {
  Member *mem = node->member;

  if (mem->ty->kind == TY_BITINT) {
    Printftn("mov %%rax, %s", argreg64[1]);
    if (ofs)
      Printftn("add $%d, %s", ofs, argreg64[1]);
    Printftn("movl $%"PRIi32", %s", (int32_t)mem->ty->bit_cnt, argreg32[0]);
    Printftn("lea %s(%s), %s", tmpbuf(mem->ty->size), lvar_ptr, argreg64[2]);
    load_val2(ty_int, mem->bit_width, argreg32[3], NULL);
    load_val2(ty_int, mem->bit_offset, argreg32[4], NULL);
    load_val2(ty_int, mem->ty->is_unsigned, argreg32[5], NULL);
    gen_bitint_builtin_call2("__slimcc_bitint_bitfield_load");
    Printstn("mov %%rax, %%rcx");
    return;
  }

  if (mem->is_aligned_bitfiled) {
    int p2bits = next_pow_of_two(mem->bit_offset + mem->bit_width);
    load2(bitwidth_to_ty(p2bits, mem->ty->is_unsigned), ofs, "%rax");

    if (mem->bit_width == mem->ty->size * 8)
      return;

    int lshft = 64 - mem->bit_width - mem->bit_offset;
    int rshft = 64 - mem->bit_width;
    if (lshft)
      Printftn("shl $%d, %%rax", lshft);
    Printftn("%s $%d, %%rax", mem->ty->is_unsigned ? "shr" : "sar", rshft);
    return;
  }
  Printstn("movq %%rax, %%rdx");

  int pofs = bitfield_footprint(mem) - 1;
  for (int start = pofs; pofs >= !!mem->bit_offset; pofs--) {
    if (pofs != start)
      Printstn("shlq $8, %%rax");
    Printftn("movb %d(%%rdx), %%al", pofs + ofs);
  }
  if (mem->bit_offset) {
    Printftn("shlq $%d, %%rax", 8 - mem->bit_offset);
    Printftn("movb %d(%%rdx), %%dl", ofs);
    Printftn("shrb $%d, %%dl", mem->bit_offset);
    Printstn("orb %%dl, %%al");
  }
  if (mem->bit_width < 64) {
    int shft = 64 - mem->bit_width;
    Printftn("shl $%d, %%rax", shft);
    Printftn("%s $%d, %%rax", mem->ty->is_unsigned ? "shr" : "sar", shft);
  }
}

static void gen_bitfield_store2(char *ptr, Reg r_val, Reg r_tmp, Member *mem) {
  if (mem->is_aligned_bitfiled) {
    int p2bits = next_pow_of_two(mem->bit_offset + mem->bit_width);
    RegSz rsz = bitwidth_to_regsz(p2bits);
    char *tmp = regs[r_tmp][rsz];
    char *val = regs[r_val][rsz];

    if (mem->bit_width == p2bits) {
      Printftn("mov %s, (%s)", val, ptr);
      return;
    }
    uint64_t msk = (((uint64_t)1 << mem->bit_width) - 1) << mem->bit_offset;
    msk = (int64_t)(~msk << (64 - p2bits)) >> (64 - p2bits);

    load_val2(bitwidth_to_ty(p2bits, false), msk, regs[r_tmp][REGSZ_32], regs[r_tmp][REGSZ_64]);

    Printftn("and %s, (%s)", tmp, ptr);
    Printftn("mov %s, %s", val, tmp);

    int lshft = p2bits - mem->bit_width;
    int rshft = p2bits - mem->bit_width - mem->bit_offset;
    if (lshft)
      Printftn("shl $%d, %s", lshft, tmp);
    if (rshft)
      Printftn("shr $%d, %s", rshft, tmp);
    Printftn("or %s, (%s)", tmp, ptr);
    return;
  }
  char *valb = regs[r_val][REGSZ_8];
  char *valq = regs[r_val][REGSZ_64];
  char *tmpb = regs[r_tmp][REGSZ_8];
  char *tmpq = regs[r_tmp][REGSZ_64];

  int rem = mem->bit_offset + mem->bit_width;
  int pofs = 0;
  if (mem->bit_offset) {
    Printftn("shlb $%d, (%s)", 8 - mem->bit_offset, ptr);
    Printftn("shrb $%d, (%s)", 8 - mem->bit_offset, ptr);
    Printftn("movb %s, %s", valb, tmpb);
    Printftn("shlb $%d, %s", mem->bit_offset, tmpb);
    Printftn("orb %s, (%s)", tmpb, ptr);
    Printftn("movq %s, %s", valq, tmpq);
    Printftn("shrq $%d, %s", 8 - mem->bit_offset, tmpq);
    pofs++;
    rem -= 8;
  } else {
    Printftn("mov %s, %s", valq, tmpq);
  }

  for (int start = rem; rem > 0; rem -= 8, pofs++) {
    if (rem != start)
      Printftn("shrq $8, %s", tmpq);

    if (rem >= 8) {
      Printftn("movb %s, %d(%s)", tmpb, pofs, ptr);
      continue;
    }
    Printftn("shrb $%d, %d(%s)", rem, pofs, ptr);
    Printftn("shlb $%d, %d(%s)", rem, pofs, ptr);
    Printftn("andb $%d, %s", (1 << rem) - 1, tmpb);
    Printftn("orb %s, %d(%s)", tmpb, pofs, ptr);
    return;
  }
}

static void gen_bitfield_store(Node *node, bool is_void) {
  Member *mem = node->member;

  if (mem->ty->kind == TY_BITINT) {
    pop(argreg64[2]);
    Printftn("movl $%"PRIi32", %s", (int32_t)mem->ty->bit_cnt, argreg32[0]);
    Printftn("lea %s(%s), %s", tmpbuf(mem->ty->size), lvar_ptr, argreg64[1]);
    load_val2(ty_int, mem->bit_width, argreg32[3], NULL);
    load_val2(ty_int, mem->bit_offset, argreg32[4], NULL);
    gen_bitint_builtin_call2("__slimcc_bitint_bitfield_save");
    if (!is_void) {
      Printftn("lea %s(%s), %s", tmpbuf(mem->ty->size), lvar_ptr, argreg64[1]);
      Printftn("movl $%"PRIi32", %s", (int32_t)mem->bit_width, argreg32[0]);
      Printftn("movl $%"PRIi32", %s", (int32_t)mem->ty->bit_cnt, argreg32[2]);
      load_val2(ty_int, mem->ty->is_unsigned, argreg32[3], NULL);
      gen_bitint_builtin_call2("__slimcc_bitint_sign_ext");
    }
    return;
  }

  char *ptr = pop_inreg(tmpreg64[0]);
  gen_bitfield_store2(ptr, REG_X64_AX, REG_X64_DX, mem);

  if (!is_void && mem->bit_width < 64) {
    int shft = 64 - mem->bit_width;
    Printftn("shl $%d, %%rax", shft);
    Printftn("%s $%d, %%rax", mem->ty->is_unsigned ? "shr" : "sar", shft);
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

    // Thread-local variable
    if (node->var->is_tls) {
      if (opt_femulated_tls) {
        clobber_all_regs();
        Printftn("movq \"__emutls_v.%s\"@GOTPCREL(%%rip), %%rdi", asm_name(node->var));
        if (opt_use_plt)
          Printstn("call __emutls_get_address@PLT");
        else
          Printstn("call *__emutls_get_address@GOTPCREL(%%rip)");
        return;
      }
      if (opt_fpic) {
        clobber_all_regs();
        Printftn(".byte 0x66; lea \"%s\"@tlsgd(%%rip), %%rdi", asm_name(node->var));
        if (opt_use_plt)
          Printstn(".value 0x6666; rex64 call __tls_get_addr@PLT");
        else
          Printstn(".byte 0x66; rex64 call *__tls_get_addr@GOTPCREL(%%rip)");
        return;
      }

      Printstn("mov %%fs:0, %%rax");
      if (node->var->is_definition)
        Printftn("add $\"%s\"@tpoff, %%rax", asm_name(node->var));
      else
        Printftn("add \"%s\"@gottpoff(%%rip), %%rax", asm_name(node->var));
      return;
    }

    // Function or global variable
    if (!(opt_fpic || opt_fpie))
      Printftn("movl $\"%s\", %%eax", asm_name(node->var));
    else if (use_rip(node->var))
      Printftn("leaq \"%s\"(%%rip), %%rax", asm_name(node->var));
    else
      Printftn("movq \"%s\"@GOTPCREL(%%rip), %%rax", asm_name(node->var));
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
    gen_addr(node->lhs);
    imm_add("%rax", "%rdx", node->member->offset);
    return;
  case ND_ASSIGN:
  case ND_COND:
  case ND_FUNCALL:
  case ND_STMT_EXPR:
  case ND_VA_ARG:
    if (node->ty->kind == TY_STRUCT || node->ty->kind == TY_UNION) {
      gen_expr(node);
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
    gen_mem_copy2(sofs, sptr, tmpbuf(10), lvar_ptr, 10);
    return;
  case TY_BITINT:
    gen_mem_copy2(sofs, sptr, tmpbuf(ty->size), lvar_ptr, ty->size);
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
    gen_mem_copy2(tmpbuf(10), lvar_ptr, dofs, dptr, 10);
    return;
  case TY_BITINT:
    gen_mem_copy2(tmpbuf(ty->size), lvar_ptr, dofs, dptr, ty->size);
    return;
  }

  Printftn("mov %s, %s(%s)", reg_ax(ty->size), dofs, dptr);
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

static void load_f32_f64(Type *ty, long double fval, int reg) {
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
    load_f32_f64(ty, fval, 0);
    return;
  }

  union { long double f80; int32_t i32[3]; } u;
  memset(&u, 0, sizeof(u));
  u.f80 = fval;
  Printftn("movl $%"PRIi32", %s(%s)", u.i32[0], tmpbuf(10), lvar_ptr);
  Printftn("movl $%"PRIi32", 4+%s(%s)", u.i32[1], tmpbuf(10), lvar_ptr);
  Printftn("movw $%"PRIu16", 8+%s(%s)", (uint16_t)u.i32[2], tmpbuf(10), lvar_ptr);
  return;
}

static void load_bitint_val(Type *ty, uint64_t *buf) {
  char memop[STRBUF_SZ];
  int32_t cnt = (ty->bit_cnt + 63) / 64;
  for (int32_t i = 0; i < cnt; i++) {
    snprintf(memop, STRBUF_SZ, "%"PRIi32"+%s(%s)", i * 8, tmpbuf(ty->size), lvar_ptr);
    imm_tmpl("movq", memop, buf[i]);
  }
}

static void gen_cmp_zero(Node *node, NodeKind kind) {
  if (node->ty->kind == TY_BITINT) {
    gen_expr(node);

    Printftn("movl $%"PRIi32", %s", (int32_t)node->ty->bit_cnt, argreg32[0]);
    Printftn("lea %s(%s), %s", tmpbuf(node->ty->size), lvar_ptr, argreg64[1]);
    gen_bitint_builtin_call2("__slimcc_bitint_to_bool");
    if (kind == ND_EQ)
      Printstn("xor $1, %%al");
    return;
  }
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

static void gen_cast_to_bitint(int32_t width, Node *node) {
  Node expr = *node;
  gen_expr(new_cast(&expr, new_bitint(width, node->tok)));
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
static char f80u32[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov (%rsp), %eax" FROM_F80_3;
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

static void gen_cast(Node *node) {
  if (node->ty->kind == TY_BOOL) {
    gen_cmp_zero(node->lhs, ND_NE);
    return;
  }
  gen_expr(node->lhs);

  if (node->ty->kind == TY_VOID)
    return;

  if (node->ty->kind == TY_BITINT) {
    int64_t from_sz;
    if (node->lhs->ty->kind == TY_BITINT) {
      if (node->lhs->ty->bit_cnt >= node->ty->bit_cnt)
        return;
      from_sz = node->lhs->ty->bit_cnt;
    } else if (is_gp_ty(node->lhs->ty)) {
      Printftn("mov %%rax, %s(%s)", tmpbuf(node->ty->size), lvar_ptr);
      if (node->lhs->ty->size * 8 >= node->ty->bit_cnt)
        return;
      from_sz = node->lhs->ty->size * 8;
    } else {
      error_tok(node->tok, "Unimplemented cast to _BitInt");
    }
    Printftn("lea %s(%s), %s", tmpbuf(node->ty->size), lvar_ptr, argreg64[1]);
    Printftn("movl $%"PRIi32", %s", (int32_t)from_sz, argreg32[0]);
    Printftn("movl $%"PRIi32", %s", (int32_t)node->ty->bit_cnt, argreg32[2]);
    load_val2(ty_int, node->lhs->ty->is_unsigned, argreg32[3], NULL);
    gen_bitint_builtin_call2("__slimcc_bitint_sign_ext");
    return;
  }

  if (node->lhs->ty->kind == TY_BITINT) {
    if (!is_gp_ty(node->ty))
      error_tok(node->tok, "Unimplemented cast from _BitInt");
    Printftn("mov %s(%s), %%rax", tmpbuf(node->ty->size), lvar_ptr);
    int64_t shft = 64 - node->lhs->ty->bit_cnt;
    if (shft > 0) {
      Printftn("shl $%d, %%rax", (int)shft);
      if (node->lhs->ty->is_unsigned)
        Printftn("shr $%d, %%rax", (int)shft);
      else
        Printftn("sar $%d, %%rax", (int)shft);
    }
    return;
  }

  int t1 = getTypeId(node->lhs->ty);
  int t2 = getTypeId(node->ty);
  if (cast_table[t1][t2]) {
    if (t1 == F80)
      Printftn("fldt %s(%s)", tmpbuf(10), lvar_ptr);

    Printftn("%s", cast_table[t1][t2]);

    if (t2 == F80)
      Printftn("fstpt %s(%s)", tmpbuf(10), lvar_ptr);
  }
}

typedef enum {
  CLASS_NO,
  CLASS_INT,
  CLASS_SSE,
  CLASS_X87,
  CLASS_MEM,
} ArgClass;

static ArgClass calc_class(ArgClass a, ArgClass b) {
  if (a == b)
    return a;
  if (a == CLASS_NO || b == CLASS_NO)
    return a == CLASS_NO ? b : a;
  if (a == CLASS_MEM || b == CLASS_MEM)
    return CLASS_MEM;
  if (a == CLASS_X87 || b == CLASS_X87)
    return CLASS_MEM;
  if (a == CLASS_INT || b == CLASS_INT)
    return CLASS_INT;
  return CLASS_SSE;
}

static ArgClass get_class(Type *ty, int lo, int hi, int offset, ArgClass ac) {
  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    for (Member *mem = ty->members; mem_iter(&mem); mem = mem->next) {
      int ofs = offset + mem->offset;
      if ((ofs + mem->ty->size) <= lo)
        continue;
      if (ofs >= hi)
        break;
      ac = get_class(mem->ty, lo, hi, ofs, ac);
    }
    return ac;
  }

  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++) {
      int ofs = offset + ty->base->size * i;
      if ((ofs + ty->base->size) <= lo)
        continue;
      if (ofs >= hi)
        break;
      ac = get_class(ty->base, lo, hi, ofs, ac);
    }
    return ac;
  }

  switch (ty->kind) {
  case TY_FLOAT:
  case TY_DOUBLE:
    return calc_class(ac, CLASS_SSE);
  case TY_LDOUBLE:
    return calc_class(ac, CLASS_X87);
  }
  return calc_class(ac, CLASS_INT);
}

static bool is_fp_class_lo(Type *ty) {
  return get_class(ty, 0, 8, 0, CLASS_NO) == CLASS_SSE;
}

static bool is_fp_class_hi(Type *ty) {
  return get_class(ty, 8, 16, 0, CLASS_NO) == CLASS_SSE;
}

static bool is_x87_class(Type *ty) {
  if (ty->size != 16)
    return false;
  return get_class(ty, 0, 16, 0, CLASS_NO) == CLASS_X87;
}

static bool is_mem_class(Type *ty) {
  if (ty->size > 16)
    return true;
  return get_class(ty, 0, 16, 0, CLASS_NO) == CLASS_MEM;
}

static bool is_by_reg_agg(Type *ty) {
  if (ty->size > 16)
    return false;
  switch (get_class(ty, 0, 16, 0, CLASS_NO)) {
  case CLASS_X87:
  case CLASS_MEM:
    return false;
  }
  return true;
}

bool va_arg_need_copy(Type *ty) {
  if (ty->size > 8 && ty->size <= 16)
    return is_fp_class_lo(ty) || is_fp_class_hi(ty);
  return false;
}

bool bitint_rtn_need_copy(size_t width) {
  return width > 128;
}

static int calling_convention(Obj *var, int *gp_count, int *fp_count, int *stack_align) {
  int stack = 0;
  int max_align = 16;
  int gp = *gp_count, fp = *fp_count;
  for (; var; var = var->param_next) {
    Type *ty = var->ty;
    assert(ty->size != 0);

    switch (ty->kind) {
    case TY_BITINT:
    case TY_STRUCT:
    case TY_UNION:
      if (is_by_reg_agg(ty)) {
        int fp_inc = is_fp_class_lo(ty) + (ty->size > 8 && is_fp_class_hi(ty));
        int gp_inc = !is_fp_class_lo(ty) + (ty->size > 8 && !is_fp_class_hi(ty));

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

    if ((ofs == 0) ? is_fp_class_lo(ty) : is_fp_class_hi(ty)) {
      if (chunk_sz == 4)
        Printftn("movss %%xmm%d, %d(%s)", fp, ofs + var->ofs, var->ptr);
      else
        Printftn("movsd %%xmm%d, %d(%s)", fp, ofs + var->ofs, var->ptr);
      fp++;
      continue;
    }
    store_gp2(regs[gp == 0 ? REG_X64_AX : REG_X64_DX], chunk_sz, ofs + var->ofs, var->ptr);
    gp++;
  }
}

static void copy_struct_reg(void) {
  Type *ty = current_fn->ty->return_ty;
  int gp = 0, fp = 0;
  char *sptr = "%rax";

  if (is_x87_class(ty)) {
    Printftn("fldt (%s)", sptr);
    return;
  }
  for (int ofs = 0; ofs < ty->size; ofs += 8) {
    int chunk_sz = MIN(8, ty->size - ofs);

    if ((ofs == 0) ? is_fp_class_lo(ty) : is_fp_class_hi(ty)) {
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
  int gp_inc = !is_fp_class_lo(ty) + !is_fp_class_hi(ty);
  if (gp_inc) {
    Printftn("cmpl $%d, (%%rax)", 48 - gp_inc * 8);
    Printstn("ja 1f");
  }
  int fp_inc = is_fp_class_lo(ty) + is_fp_class_hi(ty);
  Printftn("cmpl $%d, 4(%%rax)", 176 - fp_inc * 16);
  Printstn("ja 1f");

  for (int ofs = 0; ofs < ty->size; ofs += 8) {
    if ((ofs == 0) ? is_fp_class_lo(ty) : is_fp_class_hi(ty)) {
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
  Printstn("mov %%rsp, %%rcx");
  Printstn("sub %%rax, %%rsp");

  int align = node->var ? MAX(node->var->alt_align, 16) : 16;
  Printftn("and $-%d, %%rsp", align);
  Printstn("sub %%rsp, %%rcx");

  if (node->kind == ND_ALLOCA_ZINIT) {
    Printstn("xorps %%xmm0, %%xmm0");
    Printstn("2:");
    Printstn("sub $16, %%rcx");
    Printstn("js 1f");
    Printstn("movaps %%xmm0, (%%rsp, %%rcx)");
  } else {
    Printstn("2:");
    Printstn("sub $4096, %%rcx");
    Printstn("js 1f");
    Printstn("orb $0, (%%rsp, %%rcx)");
  }
  Printstn("jmp 2b");
  Printstn("1:");

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

static void place_reg_arg(Type *ty, char *ofs, char *ptr, int *gp, int *fp) {
  switch (ty->kind) {
  case TY_BITINT:
  case TY_STRUCT:
  case TY_UNION:
    if (is_fp_class_lo(ty))
      Printftn("movsd %s(%s), %%xmm%d", ofs, ptr, (*fp)++);
    else
      Printftn("mov %s(%s), %s",  ofs, ptr, argreg64[(*gp)++]);

    if (ty->size > 8) {
      if (is_fp_class_hi(ty))
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

// Logic should be in sync with prepare_funcall()
static void gen_funcall_args(Node *node) {
  // Pass-by-stack or non-trival args that need spilling
  for (Obj *var = node->args; var; var = var->param_next)
    if (var->ptr)
      gen_var_assign(var, var->arg_expr);

  bool rtn_by_stk = is_mem_class(node->ty);
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
      if (gen_load_opt_gp(arg_expr, (gp < 6 ? argreg[gp] : REG_X64_NULL))) {
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
          Printftn("mov %%rax, %s", argreg64[gp++]);
        else if (is_flonum(arg_expr->ty))
          fp++;
        else if (arg_expr->ty->kind == TY_BITINT)
          place_reg_arg(arg_expr->ty, tmpbuf(arg_expr->ty->size), lvar_ptr, &gp, &fp);
        else
          place_reg_arg(arg_expr->ty, "0", "%rax", &gp, &fp);
        continue;
      }
    }
    snprintf(ofs, STRBUF_SZ, "%d", var->ofs);
    place_reg_arg(var->ty, ofs, var->ptr, &gp, &fp);
  }

  if (rtn_by_stk)
    Printftn("lea %d(%s), %%rdi", node->ret_buffer->ofs, node->ret_buffer->ptr);
}

static void gen_funcall(Node *node) {
  bool rtn_by_stk = is_mem_class(node->ty);
  int gp_count = rtn_by_stk;
  int fp_count = 0;
  int arg_stk_align;
  int arg_stk_size = calling_convention(node->args, &gp_count, &fp_count, &arg_stk_align);
  if (arg_stk_size)
    if (arg_stk_align > 16)
      push_from("%rsp");

  bool use_fn_ptr = !is_direct_fncall(node->lhs);
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

  gen_funcall_args(node);

  if (get_func_ty(node->lhs->ty)->is_variadic) {
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
    if (use_rip(node->lhs->var))
      Printftn("call \"%s\"", asm_name(node->lhs->var));
    else if (opt_use_plt)
      Printftn("call \"%s\"@PLT", asm_name(node->lhs->var));
    else
      Printftn("call *\"%s\"@GOTPCREL(%%rip)", asm_name(node->lhs->var));
  }

  clobber_all_regs();

  if (arg_stk_size) {
    if (arg_stk_align > 16)
      pop("%rsp");
    else
      Printftn("add $%d, %%rsp", align_to(arg_stk_size, 16));
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
        Printftn("jmp %s", jump_label);
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
        Printftn("cmp %s, %s", regop_ax(node->lhs->ty), op);
      }
      flip_cmp(&kind, !jump_cond);
      char *ins = cmp_cc(kind, node->lhs->ty->is_unsigned);
      Printftn("j%s %s", ins, jump_label);
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
      Printftn("jmp %s", jump_label);
      Printfsn("%s:", short_label);
      return;
    }

    if (node->kind == ND_CAST && node->ty->kind == TY_BOOL &&
      node->lhs->ty->kind != TY_BITINT) {
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
  Printstn("test %%al, %%al");
  Printftn("j%s %s", (jump_cond ? "ne" : "e"), jump_label);
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
  Printftn("%s", short_cond ? set_zero : set_one);
  Printftn("jmp .L.fall.%"PRIi64, c);
  Printfsn("%s:", short_label);
  Printftn("%s", short_cond ? set_one : set_zero);
  Printfsn(".L.fall.%"PRIi64":", c);
}

static void gen_xmm_arith(Node *node) {
  if (node->kind == ND_NEG) {
    gen_expr(node->lhs);

    if (node->ty->kind == TY_FLOAT) {
      Printstn("mov $0x80000000, %%eax");
      Printstn("movd %%eax, %%xmm1");
      Printstn("xorps %%xmm1, %%xmm0");
    } else {
      Printstn("mov $0x8000000000000000, %%rax");
      Printstn("movq %%rax, %%xmm1");
      Printstn("xorpd %%xmm1, %%xmm0");
    }
    return;
  }
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

static void gen_x87_arith(Node *node) {
  if (node->kind == ND_NEG) {
    gen_expr(node->lhs);
    Printftn("fldt %s(%s)", tmpbuf(10), lvar_ptr);
    Printstn("fchs");
    Printftn("fstpt %s(%s)", tmpbuf(10), lvar_ptr);
    return;
  }
  gen_expr(node->lhs);
  push_x87();
  gen_expr(node->rhs);
  Printftn("fldt %s(%s)", tmpbuf(10), lvar_ptr);
  pop_x87();
  Printftn("fldt %s(%s)", tmpbuf(10), lvar_ptr);

  switch (node->kind) {
  case ND_ADD:
    Printstn("faddp");
    Printftn("fstpt %s(%s)", tmpbuf(10), lvar_ptr);
    return;
  case ND_SUB:
    Printstn("fsubp");
    Printftn("fstpt %s(%s)", tmpbuf(10), lvar_ptr);
    return;
  case ND_MUL:
    Printstn("fmulp");
    Printftn("fstpt %s(%s)", tmpbuf(10), lvar_ptr);
    return;
  case ND_DIV:
    Printstn("fdivp");
    Printftn("fstpt %s(%s)", tmpbuf(10), lvar_ptr);
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

static void gen_bitint_arith(Node *node) {
  Type *ty = node->lhs->ty;

  switch (node->kind) {
  case ND_NEG:
  case ND_BITNOT:
    gen_expr(node->lhs);

    Printftn("movl $%"PRIi32", %s", (int32_t)ty->bit_cnt, argreg32[0]);
    Printftn("lea %s(%s), %s", tmpbuf(ty->size), lvar_ptr, argreg64[1]);
    gen_bitint_builtin_call(node->kind);
    return;
  }

  gen_expr(node->lhs);
  push_bitint(ty->size);
  gen_expr(node->rhs);

  Printftn("movl $%"PRIi32", %s", (int32_t)ty->bit_cnt, argreg32[0]);
  Printftn("lea %d(%s), %s", pop_bitint(ty->size), lvar_ptr, argreg64[1]);
  Printftn("lea %s(%s), %s", tmpbuf(ty->size), lvar_ptr, argreg64[2]);

  switch (node->kind) {
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
  case ND_ADD:
  case ND_SUB:
  case ND_MUL:
    gen_bitint_builtin_call(node->kind);
    return;
  case ND_DIV:
  case ND_MOD:
    load_val2(ty_int, ty->is_unsigned, argreg32[3], NULL);
    load_val2(ty_int, node->kind == ND_DIV, argreg32[4], NULL);
    gen_bitint_builtin_call(node->kind);
    return;
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
    Printftn("movl %%eax, %s", argreg32[3]);
    if (node->kind != ND_SHL)
      load_val2(ty_int, ty->is_unsigned, argreg32[4], NULL);
    gen_bitint_builtin_call(node->kind);
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE: {
    load_val2(ty_int, ty->is_unsigned, argreg32[3], NULL);
    gen_bitint_builtin_call2("__slimcc_bitint_cmp");

    NodeKind kind;
    int val;
    switch (node->kind) {
    case ND_EQ: val = 0; kind = ND_EQ; break;
    case ND_NE: val = 0; kind = ND_NE; break;
    case ND_LT: val = 1; kind = ND_EQ; break;
    case ND_LE: val = 2; kind = ND_NE; break;
    case ND_GT: val = 2; kind = ND_EQ; break;
    case ND_GE: val = 1; kind = ND_NE; break;
    }
    imm_cmp("%eax", NULL, val);
    gen_cmp_setcc(kind, false);
    return;
  }
  }
  error_tok(node->tok, "invalid expression");
}

static void gen_gp_arith(Node *node) {
  switch (node->kind) {
  case ND_NEG:
    gen_expr(node->lhs);
    Printftn("neg %s", regop_ax(node->lhs->ty));
    return;
  case ND_BITNOT:
    gen_expr(node->lhs);
    Printstn("not %%rax");
    return;
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

// Generate code for a given node.
static void gen_expr2(Node *node, bool is_void) {
  if (opt_g)
    print_loc(node->tok);

  if (opt_optimize && gen_expr_opt(node))
    return;

  switch (node->kind) {
  case ND_NULL_EXPR:
  case ND_UNREACHABLE:
    return;
  case ND_NUM: {
    if (node->ty->kind == TY_BITINT)
      load_bitint_val(node->ty, node->bitint_data);
    else if (is_flonum(node->ty))
      load_fval(node->ty, node->fval);
    load_val(node->ty, node->val);
    return;
  }
  case ND_POS:
    gen_expr(node->lhs);
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
    Printstn("movq %%rax, %%rcx");
    load(node->lhs, 0);
    push_by_ty(node->lhs->ty);
    push_from("%rcx");
    gen_expr_null_lhs(ND_ADD, node->lhs->ty, node->rhs);
    store(node->lhs, true);
    pop_by_ty(node->lhs->ty);
    return;
  case ND_CKD_ARITH: {
    Type *res_ty = node->inc->ty->base;
    int32_t chk_bits = res_ty->is_unsigned +
      (res_ty->kind == TY_BITINT ? res_ty->bit_cnt : res_ty->size * 8);

    int32_t bits = MAX(chk_bits, res_ty->size * 8);
    bits = MAX(bits, ovf_headroom(node->lhs->ty, node->arith_kind));
    bits = MAX(bits, ovf_headroom(node->rhs->ty, node->arith_kind));
    bits = align_to(bits, 64);
    int32_t sz = bits / 8;

    gen_expr(node->inc);
    push();
    gen_cast_to_bitint(bits, node->lhs);
    push_bitint(sz);
    gen_cast_to_bitint(bits, node->rhs);

    load_val2(ty_int, bits, argreg32[0], NULL);
    Printftn("lea %d(%s), %s", pop_bitint(sz), lvar_ptr, argreg64[1]);
    Printftn("lea %s(%s), %s", tmpbuf(sz), lvar_ptr, argreg64[2]);
    gen_bitint_builtin_call(node->arith_kind);

    pop("%rax");
    gen_mem_copy2(tmpbuf(sz), lvar_ptr, "0", "%rax", res_ty->size);

    load_val2(ty_int, bits, argreg32[0], NULL);
    Printftn("lea %s(%s), %s", tmpbuf(sz), lvar_ptr, argreg64[1]);
    load_val2(ty_int, chk_bits, argreg32[2], NULL);
    load_val2(ty_int, res_ty->is_unsigned, argreg32[3], NULL);
    gen_bitint_builtin_call2("__slimcc_bitint_overflow");
    return;
  }
  case ND_STMT_EXPR:
    name_labels(node);
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
  case ND_INIT_SEQ:
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
    Printftn("jmp .L.end.%"PRIi64, c);
    Printfsn("%s:", else_label);
    gen_expr(node->els);
    Printfsn(".L.end.%"PRIi64":", c);
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);
    Printstn("xor $1, %%al");
    return;
  case ND_LOGAND:
  case ND_LOGOR:
    gen_logical(node, false);
    return;
  case ND_FUNCALL:
    gen_funcall(node);

    if (is_x87_class(node->ty)) {
      if (is_void) {
        Printstn("fstp %%st(0)");
      } else if (node->ret_buffer) {
        Printftn("lea %d(%s), %%rax", node->ret_buffer->ofs, node->ret_buffer->ptr);
        Printstn("fstpt (%%rax)");
      } else {
        Printftn("fstpt %s(%s)", tmpbuf(10), lvar_ptr);
      }
      return;
    }
    if (node->ty->kind == TY_BITINT) {
      if (bitint_rtn_need_copy(node->ty->bit_cnt)) {
        char sofs[STRBUF_SZ];
        snprintf(sofs, STRBUF_SZ, "%d", node->ret_buffer->ofs);
        gen_mem_copy2(sofs, node->ret_buffer->ptr,
          tmpbuf(node->ty->size), lvar_ptr, node->ty->size);
      } else {
        Printftn("mov %%rax, %s(%s)", tmpbuf(node->ty->size), lvar_ptr);
        if (node->ty->bit_cnt > 64)
          Printftn("mov %%rdx, 8+%s(%s)", tmpbuf(node->ty->size), lvar_ptr);
      }
      return;
    }
    if (!is_void) {
      if (is_integer(node->ty) && node->ty->size < 4) {
        cast_extend_int32(node->ty, reg_ax(node->ty->size), "%eax");
        return;
      }
      if (node->ret_buffer && !is_mem_class(node->ty)) {
        copy_ret_buffer(node->ret_buffer);
        Printftn("lea %d(%s), %%rax", node->ret_buffer->ofs, node->ret_buffer->ptr);
      }
    }
    return;
  case ND_LABEL_VAL:
    Printftn("lea %s(%%rip), %%rax", goto_label(node));
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

    switch (ty->kind) {
    case TY_DOUBLE: Printftn("movq %%xmm0, %s", dx); break;
    case TY_FLOAT: Printftn("movd %%xmm0, %s", dx); break;
    default:
      if (!is_gp_ty(ty))
        error_tok(node->tok, "unsupported type for atomic CAS");
      Printftn("mov %s, %s", ax, dx);
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
  case ND_THREAD_FENCE:
    Printstn("mfence");
    return;
  case ND_ALLOCA:
  case ND_ALLOCA_ZINIT:
    gen_expr(node->lhs);
    builtin_alloca(node);
    return;
  case ND_FRAME_ADDR:
    if (!node->val) {
      Printstn("movq %%rbp, %%rax");
      return;
    }
    Printstn("movq (%%rbp), %%rax");
    for (int64_t i = 1; i < node->val; i++)
      Printstn("movq (%%rax), %%rax");
    return;
  case ND_RTN_ADDR:
    if (!node->val) {
      Printstn("movq 8(%%rbp), %%rax");
      return;
    }
    Printstn("movq (%%rbp), %%rax");
    for (int64_t i = 1; i < node->val; i++)
      Printstn("movq (%%rax), %%rax");
    Printstn("movq 8(%%rax), %%rax");
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
    bool use_reg_save = is_by_reg_agg(ty);
    if (use_reg_save) {
      if (va_arg_need_copy(ty)) {
        // Structs with FP member are split into 8-byte chunks in the
        // reg save area, we reconstruct the layout with a local copy.
        gen_vaarg_reg_copy(ty, node->var);
      } else if (is_fp_class_lo(ty)) {
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
    if (use_reg_save)
      Printssn("2:");
    Printstn("mov %%rdx, %%rax");
    return;
  }
  }

  switch (node->lhs->ty->kind) {
  case TY_FLOAT:
  case TY_DOUBLE:
    gen_xmm_arith(node);
    return;
  case TY_LDOUBLE:
    gen_x87_arith(node);
    return;
  case TY_BITINT:
    gen_bitint_arith(node);
    return;
  }
  if (is_gp_ty(node->ty)) {
    gen_gp_arith(node);
    return;
  }
  error_tok(node->tok, "invalid expression");
}

static void gen_expr(Node *node) {
  gen_expr2(node, false);
  return;
}

static void gen_stmt_naked(Node *node) {
  switch (node->kind) {
  case ND_NULL_STMT:
    return;
  case ND_BLOCK:
    if (has_defr(node))
      break;
    for (Node *n = node->body; n; n = n->next)
      gen_stmt_naked(n);
    return;
  case ND_ASM:
    if (!is_plain_asm(node))
      break;
    gen_asm(node);
    return;
  }
  error_tok(node->tok, "unsupported statement in naked function");
}

static void gen_stmt(Node *node) {
  if (opt_g)
    print_loc(node->tok);

  switch (node->kind) {
  case ND_NULL_STMT:
    return;
  case ND_IF:
    gen_if_stmt(node, true);
    return;
  case ND_FOR: {
    char bg[STRBUF_SZ], cont[STRBUF_SZ], brk[STRBUF_SZ];
    name_brk_cont(node, bg, brk, cont, count());

    if (node->init)
      gen_stmt(node->init);
    Printfsn("%s:", bg);
    if (node->cond)
      gen_cond(node->cond, false, brk);
    gen_stmt(node->then);
    Printfsn("%s:", cont);
    if (node->inc)
      gen_void_expr(node->inc);
    gen_defr(node);
    Printftn("jmp %s", bg);
    Printfsn("%s:", brk);
    return;
  }
  case ND_DO: {
    char bg[STRBUF_SZ], cont[STRBUF_SZ], brk[STRBUF_SZ];
    name_brk_cont(node, bg, brk, cont, count());

    Printfsn("%s:", bg);
    gen_stmt(node->then);
    Printfsn("%s:", cont);
    gen_cond(node->cond, true, bg);
    Printfsn("%s:", brk);
    return;
  }
  case ND_SWITCH: {
    gen_expr(node->cond);

    int64_t c = count(), cnt = 0;
    char *ax, *cx, *dx;
    if (node->cond->ty->size == 8)
      ax = "%rax", cx = "%rcx", dx = "%rdx";
    else
      ax = "%eax", cx = "%ecx", dx = "%edx";

    Node *label = NULL;
    for (CaseRange *cr = node->cases; cr; cr = cr->next) {
      if (cr->label == node->default_label)
        continue;

      if (label != cr->label) {
        label = cr->label;
        label->unique_label = format(".L.case%"PRIi64".%"PRIi64, cnt++, c);
      }

      if (cr->hi == cr->lo) {
        imm_cmp(ax, dx, cr->lo);
        Printftn("je %s", label->unique_label);
        continue;
      }
      if (cr->lo == 0) {
        imm_cmp(ax, dx, cr->hi);
        Printftn("jbe %s", label->unique_label);
        continue;
      }
      Printftn("mov %s, %s", ax, cx);
      imm_sub(cx, dx, cr->lo);
      imm_cmp(cx, dx, cr->hi - cr->lo);
      Printftn("jbe %s", label->unique_label);
    }

    char brk[STRBUF_SZ];
    name_brk_cont(node, NULL, brk, NULL, c);

    if (node->default_label) {
      node->default_label->unique_label = format(".L.default.%"PRIi64, c);
      Printftn("jmp %s", node->default_label->unique_label);
    } else {
      Printftn("jmp %s", brk);
    }
    gen_stmt(node->then);
    Printfsn("%s:", brk);
    return;
  }
  case ND_BLOCK:
    gen_block_stmt(node, true);
    return;
  case ND_BREAK:
  case ND_CONT:
  case ND_GOTO:
    gen_defr(node);
    Printftn("jmp %s", goto_label(node));
    return;
  case ND_GOTO_EXPR:
    gen_expr(node->lhs);
    Printstn("jmp *%%rax");
    return;
  case ND_LABEL:
    Printfsn("%s:", node->unique_label);
    return;
  case ND_RETURN: {
    if (!node->lhs) {
      if (has_defr(node))
        gen_defr(node);
      Printftn("jmp .L.rtn.%"PRIi64, rtn_label);
      return;
    }
    gen_expr(node->lhs);
    Type *ty = node->lhs->ty;

    if (ty->kind == TY_STRUCT || ty->kind == TY_UNION || ty->kind == TY_BITINT) {
      if (ty->kind == TY_BITINT)
        Printftn("lea %s(%s), %%rax", tmpbuf(ty->size), lvar_ptr);

      if (!is_mem_class(ty)) {
        if (has_defr(node)) {
          if (ty->kind != TY_BITINT)
            gen_mem_copy2("0", "%rax", tmpbuf(ty->size), lvar_ptr, ty->size);
          push_bitint(ty->size);
          gen_defr(node);
          Printftn("lea %d(%s), %%rax", pop_bitint(ty->size), lvar_ptr);
        }
        copy_struct_reg();
        Printftn("jmp .L.rtn.%"PRIi64, rtn_label);
        return;
      }
      copy_struct_mem();
      if (has_defr(node)) {
        push();
        gen_defr(node);
        pop("%rax");
      }
      Printftn("jmp .L.rtn.%"PRIi64, rtn_label);
      return;
    }
    if (has_defr(node)) {
      push_by_ty(ty);
      gen_defr(node);
      pop_by_ty(ty);
    }
    if (ty->kind == TY_LDOUBLE)
      Printftn("fldt %s(%s)", tmpbuf(10), lvar_ptr);
    Printftn("jmp .L.rtn.%"PRIi64, rtn_label);
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

  if (is_memop_ptr(rhs, sofs, &sptr) && sptr == rip && !(opt_fpic || opt_fpie)) {
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

  gen_expr2(node, true);
}

static void gen_void_expr(Node *node) {
  if (node->ty->is_atomic || node->ty->is_volatile) {
    gen_expr2(node, true);
    return;
  }

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

  if (is_pow_of_two(val)) {
    if (kind == ND_DIV) {
      if (ty->is_unsigned) {
        gen_expr(expr);
        Printftn("shr $%d, %s", trailing_zero(val), ax);
        return true;
      }
      if (val > 1 && val <= (1L << 30)) {
        gen_expr(expr);
        Printftn("lea %"PRIi32"(%s), %s", (int32_t)(val - 1), ax, dx);
        Printftn("test %s, %s", ax, ax);
        Printftn("cmovs %s, %s", dx, ax);
        Printftn("sar $%d, %s", trailing_zero(val), ax);
        return true;
      }
    }

    if (kind == ND_MOD && val != 0) {
      if (ty->is_unsigned) {
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
      if (val > 1 && val <= (1L << 30)) {
        gen_expr(expr);
        Printftn("%s", ty->size == 8 ? "cqto" : "cltd");
        Printftn("shr $%d, %s", (int)(ty->size * 8 - trailing_zero(val)), dx);
        Printftn("add %s, %s", dx, ax);
        imm_and(ax, NULL, (int32_t)(val - 1));
        Printftn("sub %s, %s", dx, ax);
        return true;
      }
    }
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

static bool gen_arith_opt_gp2(NodeKind kind, int sz, Node *lhs, Node *rhs, int pass) {
  int64_t val;
  char ofs[STRBUF_SZ], *ptr;
  char *ax = reg_ax(sz);

  switch (pass) {
  case 0:
    if (is_const_expr(rhs, &val)) {
      gen_expr(lhs);
      imm_arith(kind, sz, limit_imm(val, sz));
      return true;
    }
    break;
  case 1:
    if (is_memop(rhs, ofs, &ptr, true)) {
      gen_expr(lhs);
      Printftn("%s %s(%s), %s", arith_ins(kind), ofs, ptr, ax);
      return true;
    }
    break;
  case 2:
    if (is_int_to_int_cast(rhs) && is_memop(rhs->lhs, ofs, &ptr, true) &&
      sz <= rhs->lhs->ty->size) {
      gen_expr(lhs);
      Printftn("%s %s(%s), %s", arith_ins(kind), ofs, ptr, ax);
      return true;
    }
    break;
  case 3:
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
  return false;
}

static bool gen_arith_opt_gp(Node *node, int sz) {
  switch (node->kind) {
  case ND_ADD:
  case ND_MUL:
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
    for (int pass = 0; pass <= 3; pass++)
      if (gen_arith_opt_gp2(node->kind, sz, node->lhs, node->rhs, pass) ||
        gen_arith_opt_gp2(node->kind, sz, node->rhs, node->lhs, pass))
        return true;
    break;
  case ND_SUB:
    for (int pass = 0; pass <= 3; pass++)
      if (gen_arith_opt_gp2(node->kind, sz, node->lhs, node->rhs, pass))
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
  case ND_MOD: {
    int64_t val;
    if (is_const_expr(rhs, &val))
      return divmod_opt(kind, ty, lhs, val);
    return false;
  }
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
  bool gen = (r != REG_X64_NULL);

  if (is_memop_ptr(node, ofs, &ptr)) {
    if (gen) {
      if (ptr == rip && !(opt_fpic || opt_fpie))
        Printftn("movl $%s, %s", ofs, regs[r][2]);
      else
        Printftn("lea %s(%s), %s", ofs, ptr, regs[r][3]);
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

    int64_t val;
    if (node->kind == ND_ADD && is_const_expr(node->rhs, &val)) {
      *ofs += val;
      continue;
    }
    if (node->kind == ND_SUB && is_const_expr(node->rhs, &val)) {
      *ofs -= val;
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
    Printstn("xor $1, %%al");
  return true;
}

static bool gen_block_stmt(Node *node, bool is_reach) {
  name_labels(node);
  for (Node *n = node->body; n; n = n->next) {
    if (is_reach)
      is_reach = gen_reachable_stmt(n);
    else
      is_reach = gen_unreachable_stmt(n);
  }
  if (is_reach)
    gen_defr(node);
  return is_reach;
}

static bool gen_skip_unreachable_stmt(Node *node, bool is_reach, char *lbl) {
  if (!node)
    return is_reach;
  if (!is_reach)
    return gen_unreachable_stmt(node);

  Printftn("jmp %s", lbl);
  gen_unreachable_stmt(node);
  Printfsn("%s:", lbl);
  return true;
}

static bool gen_if_stmt(Node *node, bool is_reach) {
  int64_t c = count();
  char else_label[STRBUF_SZ];
  snprintf(else_label, STRBUF_SZ, ".L.else.%"PRIi64, c);

  if (!is_reach) {
    is_reach = gen_unreachable_stmt(node->then);
    return gen_skip_unreachable_stmt(node->els, is_reach, else_label);
  }
  int64_t val;
  if (is_const_expr(node->cond, &val)) {
    if (val) {
      is_reach = gen_reachable_stmt(node->then);
      return gen_skip_unreachable_stmt(node->els, is_reach, else_label);
    }
    is_reach = node->els ? gen_reachable_stmt(node->els) : true;
    return gen_skip_unreachable_stmt(node->then, is_reach, else_label);
  }

  gen_cond(node->cond, false, else_label);
  is_reach = gen_reachable_stmt(node->then);
  if (!node->els) {
    Printfsn("%s:", else_label);
    return true;
  }

  if (is_reach)
    Printftn("jmp .L.end.%"PRIi64, c);
  Printfsn("%s:", else_label);
  bool is_reach2 = gen_reachable_stmt(node->els);
  if (is_reach)
    Printfsn(".L.end.%"PRIi64":", c);
  return is_reach | is_reach2;
}

static bool gen_reachable_stmt(Node *node) {
  if (opt_g)
    print_loc(node->tok);

  switch (node->kind) {
  case ND_IF:
    return gen_if_stmt(node, true);
  case ND_BLOCK:
    return gen_block_stmt(node, true);
  case ND_GOTO:
  case ND_GOTO_EXPR:
  case ND_BREAK:
  case ND_CONT:
  case ND_RETURN:
    gen_stmt(node);
    return false;
  case ND_EXPR_STMT:
    if (node->lhs->kind == ND_UNREACHABLE)
      return false;
    if (node->lhs->kind == ND_FUNCALL && is_direct_fncall(node->lhs->lhs) &&
      node->lhs->lhs->var->is_noreturn) {
      gen_stmt(node);
      return false;
    }
  }
  gen_stmt(node);
  return true;
}

static bool gen_unreachable_stmt(Node *node) {
  if (opt_g)
    print_loc(node->tok);

  switch (node->kind) {
  case ND_IF:
    return gen_if_stmt(node, false);
  case ND_BLOCK:
    return gen_block_stmt(node, false);
  case ND_LABEL:
    gen_stmt(node);
    return true;
  case ND_FOR:
  case ND_DO:
  case ND_SWITCH:
    if (node->then->no_label)
      return false;
    gen_stmt(node);
    return true;
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

  if (gen_load_opt_gp(node, REG_X64_AX))
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

  {
    int64_t cond_val;
    if (kind == ND_COND && is_const_expr(node->cond, &cond_val)) {
      if (cond_val)
        gen_expr(node->then);
      else
        gen_expr(node->els);
      return true;
    }
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
  memset(asm_ops.data, 0, sizeof(*asm_ops.data) * asm_ops.capacity);

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
  return REG_X64_AX <= reg && reg <= REG_X64_SP;
}

static bool is_xmm_reg(Reg reg) {
  return REG_X64_XMM0 <= reg && reg <= REG_X64_XMM15;
}

static bool is_x87_reg(Reg reg) {
  return REG_X64_X87_ST0 <= reg && reg <= REG_X64_X87_ST7;
}

static char *gcc_reg_id(char *loc, Token *tok) {
  // FIXED_REGISTERS in gcc/config/i386/i386.h
  // LLVM call these GCCRegNames
  static char *names[] = {"ax", "dx", "cx", "bx", "si", "di"};

  unsigned long idx = strtoul(loc, NULL, 10);
  if (idx >= 6)
    error_tok(tok, "unknown gcc register id");
  return names[idx];
}

static Reg ident_gp_reg(char *loc) {
  static HashMap map;
  if (map.capacity == 0) {
    for (Reg r = REG_X64_NULL; r < REG_X64_END; r++)
      if (is_gp_reg(r))
        for (int j = 0; j < 4; j++)
          hashmap_put(&map, &regs[r][j][1], (void *)(intptr_t)r);

    hashmap_put(&map, "ah", (void *)(intptr_t)REG_X64_AX);
    hashmap_put(&map, "bh", (void *)(intptr_t)REG_X64_BX);
    hashmap_put(&map, "ch", (void *)(intptr_t)REG_X64_CX);
    hashmap_put(&map, "dh", (void *)(intptr_t)REG_X64_DX);
  }
  return (intptr_t)hashmap_get(&map, loc);
}

static Reg ident_reg(char *str, Token *tok) {
  if (Isdigit(*str)) {
    str = gcc_reg_id(str, tok);
  } else {
    while (*str == '%')
      str++;
  }
  Reg gpreg = ident_gp_reg(str);
  if (gpreg)
    return gpreg;

  if (!strncmp(str, "xmm", 3) || !strncmp(str, "ymm", 3) || !strncmp(str, "zmm", 3))
    return REG_X64_XMM0 + strtoul(&str[3], NULL, 10);

  if (!strncmp(str, "st", 2)) {
    if (str[2] == '(') {
      unsigned long num = strtoul(&str[3], NULL, 10);
      return REG_X64_X87_ST0 + MIN(num, 7);
    }
    return REG_X64_X87_ST0;
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

    Reg r = ident_reg(tok->str, tok);
    if (r == REG_X64_SP)
      continue;
    if (is_x87_reg(r)) {
      *x87_clobber = MAX(*x87_clobber, (r - REG_X64_X87_ST0 + 1));
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
    Reg reg = REG_X64_NULL;
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
      case '+':
      case '%': continue;
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
      case 'a': fixed_reg(&reg, REG_X64_AX, tok); continue;
      case 'b': fixed_reg(&reg, REG_X64_BX, tok); continue;
      case 'c': fixed_reg(&reg, REG_X64_CX, tok); continue;
      case 'd': fixed_reg(&reg, REG_X64_DX, tok); continue;
      case 'S': fixed_reg(&reg, REG_X64_SI, tok); continue;
      case 'D': fixed_reg(&reg, REG_X64_DI, tok); continue;
      case 't': fixed_reg(&reg, REG_X64_X87_ST0, tok); continue;
      case 'u': fixed_reg(&reg, REG_X64_X87_ST1, tok); continue;
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
    if (is_symbolic && is_asm_symbolic_arg(ap->arg, NULL)) {
      ap->kind = ASMOP_SYMBOLIC;
      continue;
    }
    if (ap->is_mem && has_memop(ap->arg)) {
      ap->kind = ASMOP_MEM;
      continue;
    }
    if (ap->arg->kind == ND_VAR && ap->arg->var->asm_name) {
      Reg r = ident_reg(ap->arg->var->asm_name, ap->arg->tok);
      if ((is_gp_reg(r) && !ap->is_gp) || (is_xmm_reg(r) && !ap->is_fp) ||
        (is_x87_reg(r) && !ap->is_x87))
        error_tok(ap->arg->tok, "constraint mismatch with variable register");
      fixed_reg(&reg, r, tok);
    }
    if (reg) {
      if (reg == REG_X64_X87_ST0 && x87_clobber >= 1)
        ap->is_clobbered_x87 = true;
      else if (reg == REG_X64_X87_ST1 && x87_clobber >= 2)
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
    for (Reg *r = reg_list; *r != REG_X64_NULL; r++) {
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
  static Reg highbyte[] = {REG_X64_AX, REG_X64_CX, REG_X64_DX, REG_X64_BX, REG_X64_NULL};
  static Reg legacy[] = {REG_X64_AX, REG_X64_CX, REG_X64_DX, REG_X64_SI, REG_X64_DI,
    REG_X64_BX, REG_X64_BP, REG_X64_NULL};
  static Reg free[] = {
    REG_X64_AX, REG_X64_CX, REG_X64_DX, REG_X64_SI, REG_X64_DI,
    REG_X64_R8, REG_X64_R9, REG_X64_R10, REG_X64_R11, REG_X64_NULL};

  asm_assign_oprands2(highbyte, offsetof(AsmParam, is_gp_highbyte));
  asm_assign_oprands2(legacy, offsetof(AsmParam, is_gp_legacy));
  asm_assign_oprands2(free, offsetof(AsmParam, is_gp_free));

  for (int i = 0; i < asm_ops.cnt; i++) {
    AsmParam *ap = asm_ops.data[i];
    if (ap->kind)
      continue;
    if ((ap->is_fp && find_free_reg(ap, REG_X64_XMM0, REG_X64_XMM15)) ||
      (ap->is_x87 && find_free_reg(ap, REG_X64_X87_ST0, REG_X64_X87_ST7)) ||
      (ap->is_gp && find_free_reg(ap, REG_X64_AX, REG_X64_R15))) {
      ap->kind = ASMOP_REG;
      asm_set_use(ap);
      continue;
    }
    if (ap->is_mem && find_free_reg(ap, REG_X64_AX, REG_X64_R15)) {
      ap->kind = ASMOP_MEM;
      asm_set_use(ap);
      continue;
    }
    error_tok(ap->arg->tok, "cannot assign register");
  }
}

static void asm_prepare_args(Node *node, int *out_tmp) {
  bool has_ptr_out = false;
  bool has_bitfield_out = false;
  for (AsmParam *ap = node->asm_outputs; ap; ap = ap->next) {
    if (ap->kind == ASMOP_REG)
      ptr_convert(&ap->arg);
    if (has_memop(ap->arg))
      continue;
    has_bitfield_out = is_bitfield(ap->arg);
    has_ptr_out = true;

    ap->ptr = new_lvar(NULL, pointer_to(ap->arg->ty));
  }
  *out_tmp = has_ptr_out + has_bitfield_out;

  for (AsmParam *ap = node->asm_inputs; ap; ap = ap->next) {
    if (ap->kind == ASMOP_REG)
      ptr_convert(&ap->arg);
    if (has_memop(ap->arg))
      continue;
    if (ap->kind == ASMOP_REG)
      ap->var = new_lvar(NULL, ap->arg->ty);
    else if (ap->kind == ASMOP_MEM)
      ap->ptr = new_lvar(NULL, pointer_to(ap->arg->ty));
  }
}

static Reg acquire_gp(bool *use1, bool *use2, Token *tok) {
  for (Reg r = REG_X64_AX; r <= REG_X64_R15; r++) {
    if (use1[r] || (use2 && use2[r]))
      continue;
    use1[r] = true;
    if (use2)
      use2[r] = true;
    return r;
  }
  error_tok(tok, "out of registers");
}

static Reg acquire_out_tmp(Node *node) {
  if (node->asm_ctx->frame_ptr1 && !asm_use.out[REG_X64_BP])
    return asm_use.out[REG_X64_BP] = true, REG_X64_BP;

  if (node->asm_ctx->frame_ptr2 && !asm_use.out[REG_X64_BX])
    return asm_use.out[REG_X64_BX] = true, REG_X64_BX;

  return acquire_gp(asm_use.out, NULL, node->tok);
}

static void asm_prepare_regs(Node *node, int tmp_cnt) {
  node->asm_ctx = ast_arena_calloc(sizeof(AsmContext));

  if (asm_use.in[REG_X64_BP] || asm_use.out[REG_X64_BP])
    node->asm_ctx->frame_ptr1 = acquire_gp(asm_use.in, asm_use.out, node->tok);

  if (asm_use.in[REG_X64_BX] || asm_use.out[REG_X64_BX])
    node->asm_ctx->frame_ptr2 = acquire_gp(asm_use.in, asm_use.out, node->tok);

  switch (tmp_cnt) {
  case 2: node->asm_ctx->output_tmp2 = acquire_out_tmp(node);
  case 1: node->asm_ctx->output_tmp1 = acquire_out_tmp(node);
  }

  for (Reg r = REG_X64_R12; r <= REG_X64_BP; r++)
    if (asm_use.in[r] || asm_use.out[r])
      node->asm_ctx->clobber_mask |= 1U << r;
}

void prepare_inline_asm(Node *node) {
  memset(&asm_use, 0, sizeof(asm_use));

  asm_fill_ops(node);

  int x87_clobber = 0;
  asm_clobbers(node->asm_clobbers, &x87_clobber);

  asm_constraint(node->asm_inputs, x87_clobber);
  asm_constraint(node->asm_outputs, 0);

  asm_assign_oprands();

  int out_tmp;
  asm_prepare_args(node, &out_tmp);

  asm_prepare_regs(node, out_tmp);
}

static char *reg_high_byte(Reg reg) {
  switch (reg) {
  case REG_X64_AX: return "%ah";
  case REG_X64_BX: return "%bh";
  case REG_X64_CX: return "%ch";
  case REG_X64_DX: return "%dh";
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
      Node addr_node = {.kind = ND_ADDR, .lhs = ap->arg, .tok = ap->arg->tok};
      gen_var_assign(ap->ptr, &addr_node);
      continue;
    }
    if (ap->var) {
      gen_var_assign(ap->var, ap->arg);
      continue;
    }
  }
}

static char *alt_ptr(char *reg) {
  if (asm_alt_ptr.rbp && reg == rbp)
    return asm_alt_ptr.rbp;
  if (asm_alt_ptr.rbx && reg == rbx)
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
    case 4: Printftn("movss %s(%s), %%xmm%d", ofs, ptr, ap->reg - REG_X64_XMM0); return;
    case 8: Printftn("movsd %s(%s), %%xmm%d", ofs, ptr, ap->reg - REG_X64_XMM0); return;
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
    case 4: Printftn("movss %%xmm%d, %s(%s)", ap->reg - REG_X64_XMM0, ofs, ptr); return;
    case 8: Printftn("movsd %%xmm%d, %s(%s)", ap->reg - REG_X64_XMM0, ofs, ptr); return;
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

static Reg get_input_reg(AsmParam *ap) {
  if (ap->kind == ASMOP_REG && *ap->constraint->str != '=')
    return ap->reg;
  return REG_X64_NULL;
}

static void asm_xmm_inputs(void) {
  for (int i = 0; i < asm_ops.cnt; i++) {
    AsmParam *ap = asm_ops.data[i];
    if (is_xmm_reg(get_input_reg(ap)))
      asm_reg_input(ap, REG_X64_DX);
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
      sort_buf[reg - REG_X64_X87_ST0] = ap;
  }

  int cnt = 0;
  for (int i = 0; i < 8; i++) {
    AsmParam *ap = sort_buf[7 - i];
    if (ap) {
      if (!ap->is_clobbered_x87)
        cnt++;
      asm_reg_input(ap, REG_X64_DX);
    }
  }
  return cnt;
}

static void asm_save_out_flags(Node *node) {
  Reg freereg = node->asm_ctx->output_tmp1;

  for (AsmParam *ap = node->asm_outputs; ap; ap = ap->next) {
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

static void asm_save_out_x87(Node *node, int in_cnt) {
  AsmParam *sort_buf[8] = {0};
  int out_cnt = 0;
  for (AsmParam *ap = node->asm_outputs; ap; ap = ap->next) {
    if (ap->kind == ASMOP_REG) {
      if (is_x87_reg(ap->reg)) {
        sort_buf[ap->reg - REG_X64_X87_ST0] = ap;
        out_cnt++;
      }
    }
  }
  out_cnt = MAX(out_cnt, in_cnt);
  for (int i = 0; i < out_cnt; i++) {
    AsmParam *ap = sort_buf[i];
    if (ap)
      asm_reg_output(ap, node->asm_ctx->output_tmp1);
    else
      Printstn("fstp %%st(0)");
  }
}

static void asm_save_out_bitfield(AsmParam *ap, Reg r1, Reg r2) {
  Member *mem = ap->arg->member;
  char *ptr = regs[r2][3];

  Printftn("movq %d(%s), %s", ap->ptr->ofs, alt_ptr(ap->ptr->ptr), ptr);

  gen_bitfield_store2(ptr, ap->reg, r1, mem);
  return;
}

static void asm_save_out(Node *node) {
  Reg r1 = node->asm_ctx->output_tmp1;
  Reg r2 = node->asm_ctx->output_tmp2;

  for (AsmParam *ap = node->asm_outputs; ap; ap = ap->next) {
    if (ap->kind == ASMOP_REG) {
      if (is_x87_reg(ap->reg))
        continue;
      if (is_bitfield(ap->arg)) {
        asm_save_out_bitfield(ap, r1, r2);
        continue;
      }
      asm_reg_output(ap, r1);
      continue;
    }
  }
}

static bool named_op(char **rest, char *p, Token *tok) {
  if (tok && !strncmp(p, tok->loc, tok->len) && p[tok->len] == ']') {
    *rest = &p[tok->len + 1];
    return true;
  }
  return false;
}

static AsmParam *find_op(char *p, char **rest, Token *tok, bool is_label) {
  if (*p == '[') {
    for (int i = 0; i < asm_ops.cnt; i++) {
      AsmParam *op = asm_ops.data[i];
      if (is_label) {
        if (named_op(rest, p + 1, op->arg->tok))
          return op;
      } else {
        if (named_op(rest, p + 1, op->name))
          return op;
      }
    }
  } else if (Isdigit(*p)) {
    unsigned long idx = strtoul(p, rest, 10);
    if (idx < asm_ops.cnt)
      return asm_ops.data[idx];
  }
  error_tok(tok, "operand not found");
}

static bool is_asm_symbolic_arg(Node *node, char *punct) {
  if (node->kind == ND_VAR && node->var->ty->kind == TY_FUNC && use_rip(node->var)) {
    if (punct)
      fprintf(output_file, "%s\"%s\"", punct, asm_name(node->var));
    return true;
  }
  char ofs[STRBUF_SZ], *ptr;
  if (node->kind == ND_ADDR &&
    is_memop(node->lhs, ofs, &ptr, true) && ptr == rip) {
    if (punct)
      Printf("%s%s", punct, ofs);
    return true;
  }
  return false;
}

static void asm_body(Node *node) {
  char *p = node->asm_str->str;
  for (;;) {
    size_t spn = strcspn(p, "%");
    if (spn) {
      fwrite(p, 1, spn, output_file);
      p += spn;
    }
    if (*p == '\0')
      break;

    if (p[1] == '%') {
      fputc('%', output_file);
      p += 2;
      continue;
    }
    p++;

    if (*p == 'l' && (p[1] == '[' || Isdigit(p[1]))) {
      AsmParam *ap = find_op(p + 1, &p, node->asm_str, true);
      if (ap->arg->kind != ND_GOTO)
        error_tok(ap->arg->tok, "not a label");

      if (node->asm_outputs)
        Printf("%df", ap->label_id);
      else
        Printf("%s", goto_label(ap->arg));
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
        Printf("%s%"PRIi64, punct, ap->val);
        continue;
      case ASMOP_SYMBOLIC:
        if (is_asm_symbolic_arg(ap->arg, punct))
          continue;
        break;
      case ASMOP_MEM: {
        char ofs[STRBUF_SZ], *ptr;
        if (is_memop(ap->arg, ofs, &ptr, true)) {
          Printf("%s(%s)", ofs, alt_ptr(ptr));
          continue;
        }
        if (ap->reg) {
          Printf("(%s)", regs[ap->reg][3]);
          continue;
        }
        break;
      }
      case ASMOP_REG:
        if (is_xmm_reg(ap->reg)) {
          Printf("%%xmm%d", ap->reg - REG_X64_XMM0);
          continue;
        }
        if (is_x87_reg(ap->reg)) {
          Printf("%%st(%d)", ap->reg - REG_X64_X87_ST0);
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
          Printf("%s", regname);
          continue;
        }
      }
      error_tok(ap->arg->tok, "invalid operand");
    }
    error_tok(node->asm_str, "unknown modifier \"%c\"", *p);
  }
  fputc('\n', output_file);
}

static void asm_push_clobbers(Node *node) {
  for (Reg r = REG_X64_R12; r <= REG_X64_R15; r++)
    if (node->asm_ctx->clobber_mask & (1U << r))
      push_from(regs[r][3]);
}

static void asm_pop_clobbers(Node *node) {
  for (Reg r = REG_X64_R15; r >= REG_X64_R12; r--)
    if (node->asm_ctx->clobber_mask & (1U << r))
      pop(regs[r][3]);
}

static char *asm_push_frame_ptr(Node *node) {
  char *prev = lvar_ptr;

  if (node->asm_ctx->frame_ptr1) {
    asm_alt_ptr.rbp = regs[node->asm_ctx->frame_ptr1][3];
    if (lvar_ptr == rbp)
      lvar_ptr = asm_alt_ptr.rbp;
    Printftn("movq %%rbp, %s", asm_alt_ptr.rbp);
  }
  if (node->asm_ctx->frame_ptr2) {
    asm_alt_ptr.rbx = regs[node->asm_ctx->frame_ptr2][3];
    if (lvar_ptr == rbx)
      lvar_ptr = asm_alt_ptr.rbx;
    Printftn("movq %%rbx, %s", asm_alt_ptr.rbx);
  }

  if (node->asm_ctx->frame_ptr1)
    push_from(rbp);
  if (node->asm_ctx->frame_ptr2)
    push_from(rbx);
  return prev;
}

static void asm_pop_frame_ptr(Node *node, char *prev) {
  if (node->asm_ctx->frame_ptr2)
    pop(rbx);
  if (node->asm_ctx->frame_ptr1)
    pop(rbp);
  lvar_ptr = prev;
}

static void gen_asm(Node *node) {
  if (is_plain_asm(node)) {
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
    char *tmp_gp = regs[node->asm_ctx->output_tmp1][3];
    Printftn("lea %df(%%rip), %s; jmp %df", fallthrough_label, tmp_gp, meet_label);
    for (AsmParam *ap = node->asm_labels; ap; ap = ap->next) {
      Printftn("%d:", ap->label_id);
      Printftn("lea %s(%%rip), %s", goto_label(ap->arg), tmp_gp);
      Printftn("jmp %df", meet_label);
    }
    Printftn("%d:", meet_label);
    Printftn("push %s", tmp_gp);
  }

  asm_save_out_flags(node);
  asm_save_out_x87(node, x87_depth);
  asm_save_out(node);

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
    align = MAX(align, get_align(var));
  }
  for (Scope *sub = scp->children; sub; sub = sub->sibling_next)
    align = MAX(align, get_lvar_align(sub, align));
  return align;
}

static int assign_lvar_offsets(Scope *sc, int bottom) {
  for (Obj *var = sc->locals; var; var = var->next) {
    if (var->pass_by_stack) {
      var->ofs = var->stack_offset + 16;
      var->ptr = rbp;
      continue;
    }
    bottom += var->ty->size;
    bottom = align_to(bottom, get_align(var));
    var->ofs = -bottom;
    var->ptr = lvar_ptr;
  }

  int max_depth = bottom;
  for (Scope *sub = sc->children; sub; sub = sub->sibling_next) {
    int sub_depth= assign_lvar_offsets(sub, bottom);
    if (!opt_reuse_stack || current_fn->dont_reuse_stk)
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

  char *vis = var->visibility ? var->visibility : opt_visibility;
  if (is_valid_vis(vis))
    Printftn(".%s \"%s\"", vis, asm_name(var));
}

static void emit_data(Obj *var) {
  int64_t sz = var->ty->size;

  if (var->ty->kind == TY_ARRAY)
    if (sz < 0 && var->is_tentative)
      sz = var->ty->base->size;

  if (sz < 0)
    error("object \'%s\' has incomplete type", asm_name(var));

  if (opt_femulated_tls && var->is_tls) {
    char name_t[STRBUF_SZ], name_v[STRBUF_SZ];
    snprintf(name_t, STRBUF_SZ, "__emutls_t.%s", asm_name(var));
    snprintf(name_v, STRBUF_SZ, "__emutls_v.%s", asm_name(var));

    Obj tmp_var = *var;
    tmp_var.is_tls = false;
    tmp_var.asm_name = tmp_var.alias_name = NULL;

    tmp_var.name = name_v;
    emit_symbol(&tmp_var);
    Printstn(".data");
    Printftn(".size \"%s\", 32", name_v);
    Printstn(".align 8");
    Printfsn("\"%s\":", name_v);
    Printftn(".quad %" PRIi64, sz);
    Printftn(".quad %d", get_align(var));
    Printstn(".quad 0");
    Printftn(".quad \"%s\"", name_t);

    tmp_var.name = name_t;
    emit_data(&tmp_var);
    return;
  }

  emit_symbol(var);

  if (var->is_tentative && !(var->is_tls || var->is_weak || var->section_name) &&
    (var->is_common || (!var->is_nocommon && opt_fcommon))) {
    Printftn(".comm \"%s\", %"PRIi64", %d", asm_name(var), sz, get_align(var));
    return;
  }

  bool use_rodata = is_const_var(var) && !((opt_fpic || opt_fpie) && (var->rel || var->section_name));
  bool use_bss = !var->init_data && !var->rel;

  if (var->section_name)
    Printfts(".section \"%s\"", var->section_name);
  else if (var->is_tls)
    Printfts(".section .%s", use_bss ? "tbss" : "tdata");
  else if (use_rodata)
    Printsts(".section .rodata");
  else if ((opt_fpic || opt_fpie) && var->rel)
    Printfts(".section .data.rel%s%s", (is_const_var(var) ? ".ro" : ""), (opt_fpie ? ".local" : ""));
  else
    Printfts(".section .%s", use_bss ? "bss" : "data");

  if (opt_data_sections && !var->section_name)
    Printf(".\"%s\"", asm_name(var));

  if (var->is_tls)
    Printssn(",\"awT\"");
  else if (use_rodata)
    Printssn(",\"a\"");
  else
    Printssn(",\"aw\"");

  Printftn(".type \"%s\", @object", asm_name(var));
  Printftn(".size \"%s\", %"PRIi64, asm_name(var), sz);
  Printftn(".align %d", get_align(var));
  Printfsn("\"%s\":", asm_name(var));

  if (sz == 0)
    return;

  if (!var->init_data) {
    Printftn(".zero %"PRIi64, sz);
    return;
  }

  Relocation *rel = var->rel;
  for (int pos = 0;;) {
    int rem = (rel ? rel->offset : sz) - pos;

    while (rem > 0) {
      if (rem >= 8)
        Printftn(".quad %"PRIi64, BUFF_CAST(int64_t, &var->init_data[pos])), pos += 8, rem -= 8;
      else if (rem >= 4)
        Printftn(".long %"PRIi32, BUFF_CAST(int32_t, &var->init_data[pos])), pos += 4, rem -= 4;
      else if (rem >= 2)
        Printftn(".word %"PRIi16, BUFF_CAST(int16_t, &var->init_data[pos])), pos += 2, rem -= 2;
      else
        Printftn(".byte %"PRIi8, BUFF_CAST(int8_t, &var->init_data[pos])), pos += 1, rem -= 1;
    }

    if (!rel)
      break;

    Printftn(".quad \"%s\"%+ld", *rel->label, rel->addend), pos += 8;
    rel = rel->next;
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

static void store_gp2(char **r, int sz, int dofs, char *dptr) {
  for (int ofs = 0;;) {
    int rem = sz - ofs;
    int p2 = (rem >= 8) ? 8 : (rem >= 4) ? 4 : (rem >= 2) ? 2 : 1;
    switch (p2) {
    case 1: Printftn("mov %s, %d(%s)", r[REGSZ_8], ofs + dofs, dptr); break;
    case 2: Printftn("mov %s, %d(%s)", r[REGSZ_16], ofs + dofs, dptr); break;
    case 4: Printftn("mov %s, %d(%s)", r[REGSZ_32], ofs + dofs, dptr); break;
    case 8: Printftn("mov %s, %d(%s)", r[REGSZ_64], ofs + dofs, dptr); break;
    }
    ofs += p2;
    if (ofs >= sz)
      return;
    Printftn("shr $%d, %s", p2 * 8, r[REGSZ_64]);
  }
}

static void store_gp(int idx, int sz, int ofs, char *ptr) {
  store_gp2(regs[argreg[idx]], sz, ofs, ptr);
}

static void emit_cdtor(char *sec, uint16_t priority, Obj *fn) {
  Printfts(".section .%s", sec);
  if (priority)
    Printf(".%"PRIu16, (uint16_t)(priority - 1));
  Printssn(",\"aw\"");
  Printstn(".align 8");
  Printftn(".quad \"%s\"", asm_name(fn));
}

static void emit_text_end(Obj *fn) {
  Printftn(".size \"%s\", .-\"%s\"", asm_name(fn), asm_name(fn));

  fclose(output_file);
  output_file = NULL;
}

void emit_text(Obj *fn) {
  FuncObj *fnobj = fn->output = calloc(1, sizeof(FuncObj));
  output_file = open_memstream(&fnobj->buf, &fnobj->buflen);

  for (Obj *var = fn->static_lvars; var; var = var->next)
    emit_data(var);

  if (fn->is_ctor || fn->is_dtor)
    fn->is_referenced = true;
  if (fn->is_ctor)
    emit_cdtor("init_array", fn->ctor_prior, fn);
  if (fn->is_dtor)
    emit_cdtor("fini_array", fn->dtor_prior, fn);

  emit_symbol(fn);

  if (fn->section_name)
    Printftn(".section \"%s\",\"ax\",@progbits", fn->section_name);
  else if (opt_func_sections)
    Printftn(".section .text.\"%s\",\"ax\",@progbits", asm_name(fn));
  else
    Printstn(".text");
  Printftn(".type \"%s\", @function", asm_name(fn));
  Printfsn("\"%s\":", asm_name(fn));

  if (fn->is_naked) {
    gen_stmt_naked(fn->body);

    emit_text_end(fn);
    return;
  }

  bool rtn_by_stk = is_mem_class(fn->ty->return_ty);
  int gp_count = rtn_by_stk;
  int fp_count = 0;
  int arg_stk_size = calling_convention(fn->ty->param_list, &gp_count, &fp_count, NULL);

  int lvar_align = get_lvar_align(fn->ty->scopes, 16);
  lvar_ptr = (lvar_align > 16) ? rbx : rbp;
  current_fn = fn;
  ext_refs = &(ExtRefs){0};
  rtn_label = count();

  // Prologue
  Printstn("push %%rbp");
  Printstn("mov %%rsp, %%rbp");
  if (lvar_align > 16) {
    Printstn("push %%rbx");
    Printftn("and $-%d, %%rsp", lvar_align);
    Printstn("mov %%rsp, %%rbx");
  }

  long stack_alloc_loc = resrvln();

  lvar_stk_sz = tmpbuf_sz = 0;

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
    case TY_BITINT:
    case TY_STRUCT:
    case TY_UNION:
      if (is_fp_class_lo(ty))
        store_fp(fp++, MIN(8, ty->size), var->ofs, var->ptr);
      else
        store_gp(gp++, MIN(8, ty->size), var->ofs, var->ptr);

      if (ty->size > 8) {
        if (is_fp_class_hi(ty))
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
  bool is_reach = gen_reachable_stmt(fn->body);

  if (is_reach && !strcmp(fn->name, "main"))
    Printstn("xor %%eax, %%eax");

  // Epilogue
  Printfsn(".L.rtn.%"PRIi64":", rtn_label);
  if (lvar_align > 16)
    Printstn("mov -8(%%rbp), %%rbx");
  Printstn("leave");
  Printstn("ret");

  assert(tmp_stack.depth == 0);
  peak_stk_usage += tmpbuf_sz;
  if (peak_stk_usage) {
    peak_stk_usage = align_to(peak_stk_usage, 16);
    insrtln(".set __BUF, -%d; sub $%d, %%rsp", stack_alloc_loc, peak_stk_usage, peak_stk_usage);
  }

  if (extref_file) {
    fclose(extref_file);
    extref_file = NULL;
    fnobj->extref_buf = ext_refs->buf;
    fnobj->extref_buflen = ext_refs->buflen;
    free(ext_refs->map.buckets);
  }
  ext_refs = NULL;
  current_fn = NULL;
  emit_text_end(fn);
}

// Logic should be in sync with gen_funcall_args()
void prepare_funcall(Node *node, Scope *scope) {
  bool rtn_by_stk = is_mem_class(node->ty);
  calling_convention(node->args, &(int){rtn_by_stk}, &(int){0}, NULL);

  int reg_arg_cnt = 0;
  for (Obj *var = node->args; var; var = var->param_next) {
    var->is_local = true;
    if (var->pass_by_stack) {
      var->ofs = var->stack_offset;
      var->ptr = "%rsp";
      continue;
    }
    if (opt_optimize) {
      Node *arg_expr = var->arg_expr;
      reg_arg_cnt++;

      if (is_gp_ty(arg_expr->ty) && is_const_expr(arg_expr, &(int64_t){0}))
        continue;
      if (is_flonum(arg_expr->ty) && is_const_double(arg_expr, &(long double){0}))
        continue;
      if (gen_load_opt_gp(arg_expr, REG_X64_NULL))
        continue;
      if (has_memop(var->arg_expr))
        continue;
      if (reg_arg_cnt == 1)
        continue;
    }
    var->next = scope->locals;
    scope->locals = var;
  }
}

static void peep_redunt_jmp(char *p) {
  while ((p = strstr(p, "\n\tjmp "))) {
    char *write_p = p;
    char *name_p = p + 6;
    p = strchr(name_p, '\n');
    size_t write_len = p - write_p;
    size_t name_len = p - name_p;

    while (*p == '\n') {
      char *name_p2 = p + 1;
      size_t ins_len = strcspn(name_p2, ":;#\n");
      if (name_p2[ins_len] != ':')
        break;
      p = name_p2 + ins_len + 1;
      if (ins_len == name_len && !memcmp(name_p2, name_p, name_len)) {
        memset(write_p, ' ', write_len);
        break;
      }
    }
  }
}

int codegen(Obj *prog, FILE *out) {
  output_file = out;

  if (opt_g) {
    File **files = get_input_files();
    for (int i = 0; files[i]; i++)
      Printftn(".file %d \"%s\"", files[i]->file_no, files[i]->name);
  }

  for (Obj *var = prog; var; var = var->next) {
    if (!var->is_definition) {
      if (var->is_weak || var->alias_name)
        emit_symbol(var);
      continue;
    }
    if (var->ty->kind == TY_ASM) {
      Printfsn("%s", var->asm_name);
      continue;
    }
    if (var->ty->kind == TY_FUNC) {
      if (!var->is_live)
        continue;
      FuncObj *fn = var->output;
      peep_redunt_jmp(fn->buf);
      fwrite(fn->buf, 1, fn->buflen, out);
      if (fn->extref_buf)
        fwrite(fn->extref_buf, 1, fn->extref_buflen, out);
      continue;
    }
    emit_data(var);
    continue;
  }
  Printstn(".section  .note.GNU-stack,\"\",@progbits");
  return 0;
}

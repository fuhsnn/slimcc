#include "slimcc.h"

#define GP_MAX 6
#define FP_MAX 8

#define GP_SLOTS 6
#define FP_SLOTS 6

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

bool dont_reuse_stack;

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

struct {
  Slot *data;
  int capacity;
  int depth;
} static tmp_stack;

static void load2(Type *ty, int sofs, char *sptr);
static void store2(Type *ty, int dofs, char *dptr);
static void store_gp2(char **reg, int sz, int ofs, char *ptr);

static void gen_expr(Node *node);
static void gen_stmt(Node *node);
static void gen_void_expr(Node *node);
static bool gen_expr_opt(Node *node);
static bool gen_addr_opt(Node *node);

static void imm_add(char *op, char *tmp, int64_t val);
static void imm_sub(char *op, char *tmp, int64_t val);
static void imm_and(char *op, char *tmp, int64_t val);
static void imm_cmp(char *op, char *tmp, int64_t val);
static char *arith_ins(NodeKind kind);

FMTCHK(1,2)
static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

static long resrvln(void) {
  long loc = ftell(output_file);
  fprintf(output_file, "                                                       \n");
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

static bool is_lvar(Node *node) {
  return node->kind == ND_VAR && node->var->is_local;
}

static bool is_memop(Node *node, char *ofs, char **ptr, bool allow_atomic) {
  if (is_bitfield(node))
    return false;

  int offset;
  Obj *var = eval_var_opt(node, &offset, allow_atomic);
  if (var) {
    if (var->is_local && var->ty->kind != TY_VLA) {
      snprintf(ofs, 64, "%d", offset + var->ofs);
      *ptr = var->ptr;
      return true;
    }
    if (!opt_fpic && !var->is_tls) {
      snprintf(ofs, 64, "%d+\"%s\"", offset, var->name);
      *ptr = "%rip";
      return true;
    }
  }
  return false;
}

bool is_trivial_arg(Node *arg) {
  if (!opt_optimize)
    return false;
  return is_lvar(arg) || (arg->kind == ND_ADDR && is_lvar(arg->lhs)) ||
    (arg->kind == ND_NUM && arg->ty->kind != TY_LDOUBLE) || is_nullptr(arg);
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
    insrtln("  mov %s, %s", sl->loc, push_reg, pop_reg);
    if (!must_be_dest)
      return pop_reg;
    println("  mov %s, %s", pop_reg, dest_reg);
    return dest_reg;
  }
  insrtln("  mov %s, %d(%s)", sl->loc, push_reg, sl->st_ofs, lvar_ptr);
  println("  mov %d(%s), %s", sl->st_ofs, lvar_ptr, dest_reg);
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
    insrtln("  %s %%xmm0, %%xmm%d", sl->loc, mv, pop_reg);
    if (!must_be_dest)
      return pop_reg;
    println("  %s %%xmm%d, %%xmm%d", mv, pop_reg, dest_reg);
    return dest_reg;
  }
  insrtln("  %s %%xmm0, %d(%s)", sl->loc, mv, sl->st_ofs, lvar_ptr);
  println("  %s %d(%s), %%xmm%d", mv, sl->st_ofs, lvar_ptr, dest_reg);
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
  insrtln("  fstpt %d(%s)", sl->loc, sl->st_ofs, lvar_ptr);
  println("  fldt %d(%s)", sl->st_ofs, lvar_ptr);
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
    insrtln("  mov %d(%s), %%rdx; mov %%rdx, %d(%s)", sl->loc, ofs, sptr, sl->st_ofs, lvar_ptr);
  }
  println("  lea %d(%s), %s", pos, lvar_ptr, sptr);
}

// When we load a char or a short value to a register, we always
// extend them to the size of int, so we can assume the lower half of
// a register always contains a valid value.
static void load_extend_int2(Type *ty, char *ofs, char *ptr, char *reg) {
  char *insn = ty->is_unsigned ? "movz" : "movs";
  switch (ty->size) {
  case 1: println("  %sbl %s(%s), %s", insn, ofs, ptr, reg); return;
  case 2: println("  %swl %s(%s), %s", insn, ofs, ptr, reg); return;
  case 4: println("  movl %s(%s), %s", ofs, ptr, reg); return;
  case 8: println("  mov %s(%s), %s", ofs, ptr, reg); return;
  }
  internal_error();
}

static void load_extend_int(Type *ty, int ofs, char *ptr, char *reg) {
  char ofs_buf[64];
  snprintf(ofs_buf, 64, "%d", ofs);
  load_extend_int2(ty, ofs_buf, ptr, reg);
}

static void load_extend_int64(Type *ty, char *ofs, char *ptr, char *reg) {
  switch (ty->size) {
  case 4: println("  movslq %s(%s), %s", ofs, ptr, reg); return;
  case 2: println("  movswq %s(%s), %s", ofs, ptr, reg); return;
  case 1: println("  movsbq %s(%s), %s", ofs, ptr, reg); return;
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
      println("  movups %d+%s(%s), %%xmm0", i, sofs, sptr);
      println("  movups %%xmm0, %d+%s(%s)", i, dofs, dptr);
      i += 16;
      continue;
    }
    int p2 = (rem >= 8) ? 8 : (rem >= 4) ? 4 : (rem >= 2) ? 2 : 1;
    println("  mov %d+%s(%s), %s", i, sofs, sptr, reg_dx(p2));
    println("  mov %s, %d+%s(%s)", reg_dx(p2), i, dofs, dptr);
    i += p2;
  }
}

static void gen_mem_copy(int sofs, char *sptr, int dofs, char *dptr, int sz) {
  char sofs_buf[64];
  snprintf(sofs_buf, 64, "%d", sofs);
  char dofs_buf[64];
  snprintf(dofs_buf, 64, "%d", dofs);
  gen_mem_copy2(sofs_buf, sptr, dofs_buf, dptr, sz);
}

static void gen_mem_zero(int dofs, char *dptr, int sz) {
  if (sz >= 16) {
    println("  xorps %%xmm0, %%xmm0");
    for (int i = 0; i < sz;) {
      if (sz < i + 16)
        i = sz - 16;
      println("  movups %%xmm0, %d(%s)", i + dofs, dptr);
      i += 16;
    }
    return;
  }

  println("  xor %%eax, %%eax");
  for (int i = 0; i < sz;) {
    int rem = sz - i;
    int p2 = (rem >= 8) ? 8 : (rem >= 4) ? 4 : (rem >= 2) ? 2 : 1;
    println("  mov %s, %d(%s)", reg_ax(p2), i + dofs, dptr);
    i += p2;
  }
}

static void gen_cmp_setx(NodeKind kind, bool is_unsigned) {
  char *ins;
  switch (kind) {
  case ND_EQ: ins = "sete"; break;
  case ND_NE: ins = "setne"; break;
  case ND_LT: ins = is_unsigned ? "setb" : "setl"; break;
  case ND_LE: ins = is_unsigned ? "setbe" : "setle"; break;
  case ND_GT: ins = is_unsigned ? "seta" : "setg"; break;
  case ND_GE: ins = is_unsigned ? "setae" : "setge"; break;
  default: internal_error();
  }
  println("  %s %%al", ins);
  println("  movzbl %%al, %%eax");
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
      println("  shr $%d, %s", mem->bit_offset, ax);
    imm_and(ax, "%rdx", (1LL << mem->bit_width) - 1);
    return;
  }
  int shft = (mem->ty->size == 8) ? 64 : 32 - mem->bit_width;
  println("  shl $%d, %s", shft - mem->bit_offset, ax);
  println("  sar $%d, %s", shft, ax);
  return;
}

static void gen_bitfield_store(Node *node) {
  Member *mem = node->member;
  Type *alt_ty = bitwidth_to_ty(mem->bit_width, mem->ty->is_unsigned);
  if (alt_ty && (mem->bit_offset == (mem->bit_offset / 8 * 8))) {
    char *reg = pop_inreg(tmpreg64[0]);
    store2(alt_ty, mem->bit_offset / 8, reg);
    return;
  }

  char *ax, *dx, *cx;
  if (mem->ty->size == 8)
    ax = "%rax", cx = "%rcx", dx = "%rdx";
  else
    ax = "%eax", cx = "%ecx", dx = "%edx";

  println("  mov %s, %s", ax, cx);
  imm_and(cx, dx, (1LL << mem->bit_width) - 1);

  char *ptr = pop_inreg(tmpreg64[0]);
  load2(mem->ty, 0, ptr);

  imm_and(ax, dx, ~(((1ULL << mem->bit_width) - 1) << mem->bit_offset));

  if (mem->bit_offset) {
    println("  mov %s, %s", cx, dx);
    println("  shl $%d, %s", mem->bit_offset, dx);
    println("  or %s, %s", dx, ax);
  } else {
    println("  or %s, %s", cx, ax);
  }
  store2(mem->ty, 0, ptr);

  println("  mov %s, %s", cx, ax);
  if (!mem->ty->is_unsigned) {
    int shft = (mem->ty->size == 8) ? 64 : 32 - mem->bit_width;
    println("  shl $%d, %s", shft, ax);
    println("  sar $%d, %s", shft, ax);
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
      println("  mov %d(%s), %%rax", node->var->ofs, node->var->ptr);
      return;
    }

    // Local variable
    if (node->var->is_local) {
      println("  lea %d(%s), %%rax", node->var->ofs, node->var->ptr);
      return;
    }

    if (opt_fpic) {
      // Thread-local variable
      if (node->var->is_tls) {
        clobber_all_regs();
        println("  data16 lea \"%s\"@tlsgd(%%rip), %%rdi", node->var->name);
        println("  .value 0x6666");
        println("  rex64");
        println("  call __tls_get_addr@PLT");
        return;
      }

      // Function or global variable
      println("  mov \"%s\"@GOTPCREL(%%rip), %%rax", node->var->name);
      return;
    }

    // Thread-local variable
    if (node->var->is_tls) {
      println("  mov %%fs:0, %%rax");
      println("  add $\"%s\"@tpoff, %%rax", node->var->name);
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
        println("  lea \"%s\"(%%rip), %%rax", node->var->name);
      else
        println("  mov \"%s\"@GOTPCREL(%%rip), %%rax", node->var->name);
      return;
    }

    // Global variable
    println("  lea \"%s\"(%%rip), %%rax", node->var->name);
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
    println("  movss %s(%s), %%xmm0", sofs, sptr);
    return;
  case TY_DOUBLE:
    println("  movsd %s(%s), %%xmm0", sofs, sptr);
    return;
  case TY_LDOUBLE:
    println("  fninit; fldt %s(%s)", sofs, sptr);
    return;
  }
  load_extend_int2(ty, sofs, sptr, regop_ax(ty));
}

static void load2(Type *ty, int sofs, char *sptr) {
  char ofs_buf[64];
  snprintf(ofs_buf, 64, "%d", sofs);
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
    println("  movss %%xmm0, %s(%s)", dofs, dptr);
    return;
  case TY_DOUBLE:
    println("  movsd %%xmm0, %s(%s)", dofs, dptr);
    return;
  case TY_LDOUBLE:
    println("  fstpt %s(%s)", dofs, dptr);
    println("  fninit; fldt %s(%s)", dofs, dptr);
    return;
  }

  println("  mov %s, %s(%s)", reg_ax(ty->size), dofs, dptr);
}

static void store2(Type *ty, int dofs, char *dptr) {
  char ofs_buf[64];
  snprintf(ofs_buf, 64, "%d", dofs);
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
    println("  xor %s, %s", gp32, gp32);
    return;
  }
  if (val == (uint32_t)val) {
    println("  movl $%"PRIi64", %s", val, gp32);
    return;
  }
  if (val == (int32_t)val) {
    if (ty->size == 8)
      println("  movq $%"PRIi64", %s", val, gp64);
    else
      println("  movl $%"PRIi64", %s", val, gp32);
    return;
  }
  println("  movabsq $%"PRIi64", %s", val, gp64);
}

static void load_val(Type *ty, int64_t val) {
  load_val2(ty, val, "%eax", "%rax");
}

static void load_fval2(Type *ty, long double fval, int reg) {
  if (ty->kind == TY_FLOAT) {
    float pos_z = +0.0f;
    float fv = fval;
    if (!memcmp(&pos_z, &fv, sizeof(float))) {
      println("  xorps %%xmm%d, %%xmm%d", reg, reg);
      return;
    }
    union { float f32; uint32_t u32; } u = { fval };
    println("  movl $%u, %%eax", u.u32);
    println("  movd %%eax, %%xmm%d", reg);
    return;
  }

  double pos_z = +0.0;
  double dv = fval;
  if (!memcmp(&pos_z, &dv, sizeof(double))) {
    println("  xorps %%xmm%d, %%xmm%d", reg, reg);
    return;
  }
  union { double f64; uint64_t u64; } u = { fval };
  println("  movq $%lu, %%rax", u.u64);
  println("  movq %%rax, %%xmm%d", reg);
  return;
}

static void load_fval(Type *ty, long double fval) {
  if (ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE) {
    load_fval2(ty, fval, 0);
    return;
  }

  long double pos_z = +0.0L;
  if (!memcmp(&pos_z, &fval, 10)) {
    println("  fninit; fldz");
    return;
  }
  long double neg_z = -0.0L;
  if (!memcmp(&neg_z, &fval, 10)) {
    println("  fninit; fldz");
    println("  fchs");
    return;
  }
  if (fval == 1) {
    println("  fninit; fld1");
    return;
  }
  if (fval == -1) {
    println("  fninit; fld1");
    println("  fchs");
    return;
  }
  union { long double f80; uint64_t u64[2]; } u;
  memset(&u, 0, sizeof(u));
  u.f80 = fval;
  println("  movq $%lu, %%rax", u.u64[0]);
  println("  movw $%lu, %%dx", u.u64[1]);
  println("  push %%rdx");
  println("  push %%rax");
  println("  fninit; fldt (%%rsp)");
  println("  add $16, %%rsp");
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
    println("  %s", cast_table[t1][t2]);
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

// Load function call arguments. Arguments are already evaluated and
// stored to the stack as local variables. What we need to do in this
// function is to load them to registers or push them to the stack as
// specified by the x86-64 psABI. Here is what the spec says:
//
// - Up to 6 arguments of integral type are passed using RDI, RSI,
//   RDX, RCX, R8 and R9.
//
// - Up to 8 arguments of floating-point type are passed using XMM0 to
//   XMM7.
//
// - If all registers of an appropriate type are already used, push an
//   argument to the stack in the right-to-left order.
//
// - Each argument passed on the stack takes 8 bytes, and the end of
//   the argument area must be aligned to a 16 byte boundary.
//
// - If a function is variadic, set the number of floating-point type
//   arguments to RAX.
static void place_stack_args(Node *node) {
  for (Obj *var = node->args; var; var = var->param_next) {
    if (!var->pass_by_stack)
      continue;

    Node *arg = var->param_arg;
    if (is_trivial_arg(arg)) {
      if (arg->kind == ND_NUM || is_nullptr(arg)) {
        if (is_flonum(arg->ty))
          load_fval(arg->ty, arg->fval);
        else
          load_val(arg->ty, arg->val);
        store2(arg->ty, var->stack_offset, "%rsp");
        continue;
      }
      if (arg->kind == ND_ADDR && is_lvar(arg->lhs)) {
        gen_addr(arg->lhs);
        store2(arg->ty, var->stack_offset, "%rsp");
        continue;
      }
      if (!is_lvar(arg))
        internal_error();
    }

    int ofs;
    char *ptr;
    if (is_lvar(arg)) {
      ofs = arg->var->ofs;
      ptr = arg->var->ptr;
    } else {
      ofs = var->ofs;
      ptr = var->ptr;
    }

    switch (var->ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
    case TY_FLOAT:
    case TY_DOUBLE:
    case TY_LDOUBLE:
      gen_mem_copy(ofs, ptr,
                   var->stack_offset, "%rsp",
                   var->ty->size);
      continue;
    }

    load_extend_int(var->ty, ofs, ptr, regop_ax(var->ty));
    println("  mov %%rax, %d(%%rsp)", var->stack_offset);
  }
}

static void place_reg_args(Node *node, bool rtn_by_stk) {
  int gp = 0, fp = 0;
  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (rtn_by_stk)
    println("  lea %d(%s), %s", node->ret_buffer->ofs, node->ret_buffer->ptr, argreg64[gp++]);

  for (Obj *var = node->args; var; var = var->param_next) {
    if (var->pass_by_stack)
      continue;

    Node *arg = var->param_arg;
    if (is_trivial_arg(arg)) {
      if (arg->kind == ND_NUM || is_nullptr(arg)) {
        if (is_flonum(arg->ty))
          load_fval2(arg->ty, arg->fval, fp++);
        else
          load_val2(arg->ty, arg->val, argreg32[gp], argreg64[gp]), gp++;
        continue;
      }
      if (arg->kind == ND_ADDR && is_lvar(arg->lhs)) {
        Obj *var2 = arg->lhs->var;
        if (var2->ty->kind == TY_VLA)
          println("  mov %d(%s), %s", var2->ofs, var2->ptr, argreg64[gp++]);
        else
          println("  lea %d(%s), %s", var2->ofs, var2->ptr, argreg64[gp++]);
        continue;
      }
      if (!is_lvar(arg))
        internal_error();
    }

    int ofs;
    char *ptr;
    if (is_lvar(arg)) {
      ofs = arg->var->ofs;
      ptr = arg->var->ptr;
    } else {
      ofs = var->ofs;
      ptr = var->ptr;
    }

    switch (var->ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (has_flonum1(var->ty))
        println("  movsd %d(%s), %%xmm%d", ofs, ptr, fp++);
      else
        println("  mov %d(%s), %s",  ofs, ptr, argreg64[gp++]);

      if (var->ty->size > 8) {
        if (has_flonum2(var->ty))
          println("  movsd %d(%s), %%xmm%d", 8 + ofs, ptr, fp++);
        else
          println("  mov %d(%s), %s",  8 + ofs, ptr, argreg64[gp++]);
      }
      continue;
    case TY_FLOAT:
      println("  movss %d(%s), %%xmm%d", ofs, ptr, fp++);
      continue;
    case TY_DOUBLE:
      println("  movsd %d(%s), %%xmm%d", ofs, ptr, fp++);
      continue;
    }

    if (var->ty->size <= 4)
      load_extend_int(var->ty, ofs, ptr, argreg32[gp++]);
    else
      load_extend_int(var->ty, ofs, ptr, argreg64[gp++]);
  }
}

static void copy_ret_buffer(Obj *var) {
  Type *ty = var->ty;
  int gp = 0, fp = 0;

  for (int ofs = 0; ofs < ty->size; ofs += 8) {
    int chunk_sz = MIN(8, ty->size - ofs);

    if ((ofs == 0) ? has_flonum1(ty) : has_flonum2(ty)) {
      if (chunk_sz == 4)
        println("  movss %%xmm%d, %d(%s)", fp, ofs + var->ofs, var->ptr);
      else
        println("  movsd %%xmm%d, %d(%s)", fp, ofs + var->ofs, var->ptr);
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
        println("  movss %d(%s), %%xmm%d", ofs, sptr, fp);
      else
        println("  movsd %d(%s), %%xmm%d", ofs, sptr, fp);
      fp++;
      continue;
    }
    if (gp == 0) {
      println("  mov %%rax, %%rcx");
      sptr = "%rcx";
    }
    int regsz = (chunk_sz > 4) ? 8 : (chunk_sz > 2) ? 4 : (chunk_sz > 1) ? 2 : 1;
    if (gp == 0)
      println("  mov %d(%%rcx), %s", ofs, reg_ax(regsz));
    else
      println("  mov %d(%%rcx), %s", ofs, reg_dx(regsz));
    gp++;
  }
}

static void copy_struct_mem(void) {
  Type *ty = current_fn->ty->return_ty;

  println("  mov -%d(%s), %%rcx", rtn_ptr_ofs, lvar_ptr);
  gen_mem_copy(0, "%rax", 0, "%rcx", ty->size);
  println("  mov %%rcx, %%rax");
}

static void gen_vaarg_reg_copy(Type *ty, Obj *var) {
  int gp_inc = !has_flonum1(ty) + !has_flonum2(ty);
  if (gp_inc) {
    println("  cmpl $%d, (%%rax)", 48 - gp_inc * 8);
    println("  ja 1f");
  }
  int fp_inc = has_flonum1(ty) + has_flonum2(ty);
  println("  cmpl $%d, 4(%%rax)", 176 - fp_inc * 16);
  println("  ja 1f");

  for (int ofs = 0; ofs < ty->size; ofs += 8) {
    if ((ofs == 0) ? has_flonum1(ty) : has_flonum2(ty)) {
      println("  movl 4(%%rax), %%ecx");  // fp_offset
      println("  addq 16(%%rax), %%rcx"); // reg_save_area
      println("  addq $16, 4(%%rax)");
    } else {
      println("  movl (%%rax), %%ecx");   // gp_offset
      println("  addq 16(%%rax), %%rcx"); // reg_save_area
      println("  addq $8, (%%rax)");
    }
    gen_mem_copy(0, "%rcx",
                 ofs + var->ofs, var->ptr,
                 MIN(8, ty->size - ofs));
  }
  println("  lea %d(%s), %%rdx", var->ofs, var->ptr);
  return;
}

static void builtin_alloca(Node *node) {
  // Shift the temporary area by %rax.
  println("  sub %%rax, %%rsp");
  // Align frame pointer
  int align = node->var ? MAX(node->var->align, 16) : 16;
  println("  and $-%d, %%rsp", align);
  if (node->var)
    println("  mov %%rsp, %d(%s)", node->var->ofs, node->var->ptr);
  else
    println("  mov %%rsp, %%rax");
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
        println("  mov %d(%s), %%rsp", vla->ofs, vla->ptr);
      else
        println("  mov -%d(%s), %%rsp", vla_base_ofs, lvar_ptr);

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

  println("  .loc %d %d", tok->display_file_no, tok->display_line_no);

  file_no = tok->display_file_no;
  line_no = tok->display_line_no;
}

static void gen_expr_null_lhs(NodeKind kind, Type *ty, Node *rhs) {
  Node null = {.kind = ND_NULL_EXPR, .ty = ty, .tok = rhs->tok};
  Node expr = {.kind = kind, .lhs = &null, .rhs = rhs,.tok = rhs->tok};
  add_type(&expr);
  gen_expr(new_cast(&expr, ty));
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
      println("  mov $0x80000000, %%eax");
      println("  movd %%eax, %%xmm1");
      println("  xorps %%xmm1, %%xmm0");
      return;
    case TY_DOUBLE:
      println("  mov $0x8000000000000000, %%rax");
      println("  movq %%rax, %%xmm1");
      println("  xorpd %%xmm1, %%xmm0");
      return;
    case TY_LDOUBLE:
      println("  fchs");
      return;
    }

    println("  neg %s", regop_ax(node->lhs->ty));
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
    println("  movq %%rax, %%rcx");
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
  case ND_MEMZERO:
    gen_mem_zero(node->var->ofs, node->var->ptr, node->var->ty->size);
    return;
  case ND_COND: {
    int c = count();
    gen_expr(node->cond);
    println("  test %%al, %%al");
    println("  je .L.else.%d", c);
    gen_expr(node->then);
    println("  jmp .L.end.%d", c);
    println(".L.else.%d:", c);
    gen_expr(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_NOT:
    gen_expr(node->lhs);
    println("  xor $1, %%al");
    return;
  case ND_BITNOT:
    gen_expr(node->lhs);
    println("  not %%rax");
    return;
  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    println("  test %%al, %%al");
    println("  je .L.false.%d", c);
    gen_expr(node->rhs);
    println(".L.false.%d:", c);
    return;
  }
  case ND_LOGOR: {
    int c = count();
    gen_expr(node->lhs);
    println("  test %%al, %%al");
    println("  jne .L.true.%d", c);
    gen_expr(node->rhs);
    println(".L.true.%d:", c);
    return;
  }
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
    gen_expr(node->lhs);
    push();
    gen_expr(node->rhs);
    println("  mov %%al, %%cl");

    char *ax = regop_ax(node->ty);
    pop2((node->ty->size == 8), ax);

    switch (node->kind) {
    case ND_SHL: println("  shl %%cl, %s", ax); break;
    case ND_SHR: println("  shr %%cl, %s", ax); break;
    case ND_SAR: println("  sar %%cl, %s", ax); break;
    }
    return;
  case ND_FUNCALL: {
    if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) {
      gen_expr(node->args->param_arg);
      builtin_alloca(node);
      return;
    }

    bool use_fn_ptr = !(node->lhs->kind == ND_VAR && node->lhs->var->ty->kind == TY_FUNC);
    if (use_fn_ptr) {
      gen_expr(node->lhs);
      push();
    }

    for (Obj *var = node->args; var; var = var->param_next)
      if (!is_trivial_arg(var->param_arg))
        gen_void_expr(var->param_arg);

    // If the return type is a large struct/union, the caller passes
    // a pointer to a buffer as if it were the first argument.
    bool rtn_by_stk= node->ret_buffer && node->ty->size > 16;
    int gp_count = rtn_by_stk;
    int fp_count = 0;
    int arg_stk_align;
    int arg_stk_size = calling_convention(node->args, &gp_count, &fp_count, &arg_stk_align);

    char *fn_ptr_reg;
    if (use_fn_ptr) {
      switch (gp_count) {
      case 6: clobber_gp(4); break;  // %r9
      case 5: clobber_gp(3); break;  // %r8
      case 4:
      case 3:
      case 2: clobber_gp(2); break;  // %rsi
      case 1: clobber_gp(1); break;  // %rdi
      }
      fn_ptr_reg = pop_inreg("%r10");
    }

    if (arg_stk_size) {
      if (arg_stk_align > 16) {
        push_from("%rsp");
        println("  sub $%d, %%rsp", arg_stk_size);
        println("  and $-%d, %%rsp", arg_stk_align);
      } else {
        println("  sub $%d, %%rsp", align_to(arg_stk_size, 16));
      }
    }

    clobber_all_regs();

    place_stack_args(node);
    place_reg_args(node, rtn_by_stk);

    if (node->lhs->ty->is_variadic) {
      if (fp_count)
        println("  movb $%d, %%al", fp_count);
      else
        println("  xor %%al, %%al");
    }

    if (use_fn_ptr)
      println("  call *%s", fn_ptr_reg);
    else
      println("  call \"%s\"%s", node->lhs->var->name, opt_fpic ? "@PLT" : "");

    if (arg_stk_size) {
      if (arg_stk_align > 16)
        pop("%rsp");
      else
        println("  add $%d, %%rsp", align_to(arg_stk_size, 16));
    }

    // It looks like the most significant 48 or 56 bits in RAX may
    // contain garbage if a function return type is short or bool/char,
    // respectively. We clear the upper bits here.
    if (is_integer(node->ty) && node->ty->size < 4) {
      if (node->ty->kind == TY_BOOL)
        println("  %s", cast_table[getTypeId(ty_int)][getTypeId(ty_uchar)]);
      else
        println("  %s", cast_table[getTypeId(ty_int)][getTypeId(node->ty)]);
    }

    // If the return type is a small struct, a value is returned
    // using up to two registers.
    if (node->ret_buffer && node->ty->size <= 16) {
      copy_ret_buffer(node->ret_buffer);
      println("  lea %d(%s), %%rax", node->ret_buffer->ofs, node->ret_buffer->ptr);
    }
    return;
  }
  case ND_LABEL_VAL:
    println("  lea %s(%%rip), %%rax", node->unique_label);
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
    case TY_DOUBLE: println("  movq %%xmm0, %s", dx); break;
    case TY_FLOAT: println("  movd %%xmm0, %s", dx); break;
    default: println("  mov %s, %s", ax, dx); break;
    }

    println("  mov (%s), %s", old, ax);
    println("  lock cmpxchg %s, (%s)", dx, addr);
    println("  sete %%cl");
    println("  je 1f");
    println("  mov %s, (%s)", ax, old);
    println("1:");
    println("  movzbl %%cl, %%eax");
    return;
  }
  case ND_EXCH: {
    gen_expr(node->lhs);
    push();
    gen_expr(node->rhs);
    char *reg = pop_inreg(tmpreg64[0]);

    int sz = node->lhs->ty->base->size;
    println("  xchg %s, (%s)", reg_ax(sz), reg);
    return;
  }
  case ND_ALLOCA:
    gen_expr(node->lhs);
    builtin_alloca(node);
    return;
  case ND_VA_START: {
    gen_expr(node->lhs);
    println("  movl $%d, (%%rax)", va_gp_start);
    println("  movl $%d, 4(%%rax)", va_fp_start);
    println("  lea %d(%%rbp), %%rdx", va_st_start);
    println("  movq %%rdx, 8(%%rax)");
    println("  lea -176(%s), %%rdx", lvar_ptr);
    println("  movq %%rdx, 16(%%rax)");
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
        println("  cmpl $%d, 4(%%rax)", 160);
        println("  ja 1f");
        println("  movl 4(%%rax), %%edx");  // fp_offset
        println("  addq 16(%%rax), %%rdx"); // reg_save_area
        println("  addq $16, 4(%%rax)");
      } else {
        int gp_inc = ty->size > 8 ? 2 : 1;
        println("  cmpl $%d, (%%rax)", 48 - gp_inc * 8);
        println("  ja 1f");
        println("  movl (%%rax), %%edx");   // gp_offset
        println("  addq 16(%%rax), %%rdx"); // reg_save_area
        println("  addq $%d, (%%rax)", gp_inc * 8);
      }
      println("  jmp 2f");
      println("1:");
    }
    println("  movq 8(%%rax), %%rdx"); // overflow_arg_area
    if (ty->align <= 8) {
      println("  addq $%d, 8(%%rax)", align_to(ty->size, 8));
    } else {
      println("  addq $%d, %%rdx", ty->align - 1);
      println("  andq $-%d, %%rdx", ty->align);
      println("  lea %d(%%rdx), %%rcx", align_to(ty->size, 8));
      println("  movq %%rcx, 8(%%rax)");
    }
    if (ty->size <= 16)
      println("2:");
    println("  mov %%rdx, %%rax");
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
      println("  add%s %%xmm%d, %%xmm0", sz, reg);
      return;
    case ND_SUB:
      println("  sub%s %%xmm0, %%xmm%d", sz, reg);
      println("  movaps %%xmm%d, %%xmm0", reg);
      return;
    case ND_MUL:
      println("  mul%s %%xmm%d, %%xmm0", sz, reg);
      return;
    case ND_DIV:
      println("  div%s %%xmm0, %%xmm%d", sz, reg);
      println("  movaps %%xmm%d, %%xmm0", reg);
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GT:
    case ND_GE:
      if (node->kind == ND_GT || node->kind == ND_GE)
        println("  ucomi%s %%xmm0, %%xmm%d", sz, reg);
      else
        println("  ucomi%s %%xmm%d, %%xmm0", sz, reg);

      if (node->kind == ND_EQ) {
        println("  sete %%al");
        println("  setnp %%dl");
        println("  and %%dl, %%al");
      } else if (node->kind == ND_NE) {
        println("  setne %%al");
        println("  setp %%dl");
        println("  or %%dl, %%al");
      } else if (node->kind == ND_LT || node->kind == ND_GT) {
        println("  seta %%al");
      } else if (node->kind == ND_LE || node->kind == ND_GE) {
        println("  setae %%al");
      }

      println("  movzbl %%al, %%eax");
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
      println("  faddp");
      return;
    case ND_SUB:
      println("  fsubp");
      return;
    case ND_MUL:
      println("  fmulp");
      return;
    case ND_DIV:
      println("  fdivp");
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GT:
    case ND_GE:
      if (node->kind == ND_LT || node->kind == ND_LE)
        println("  fxch %%st(1)");

      println("  fucomip");
      println("  fstp %%st(0)");

      if (node->kind == ND_EQ) {
        println("  sete %%al");
        println("  setnp %%dl");
        println("  and %%dl, %%al");
      } else if (node->kind == ND_NE) {
        println("  setne %%al");
        println("  setp %%dl");
        println("  or %%dl, %%al");
      } else if (node->kind == ND_LT || node->kind == ND_GT) {
        println("  seta %%al");
      } else if (node->kind == ND_LE || node->kind == ND_GE) {
        println("  setae %%al");
      }

      println("  movzbl %%al, %%eax");
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
    println("  add %s, %s", op, ax);
    return;
  case ND_SUB:
    println("  sub %s, %s", ax, op);
    println("  mov %s, %s", op, ax);
    return;
  case ND_MUL:
    println("  imul %s, %s", op, ax);
    return;
  case ND_DIV:
  case ND_MOD:
    println("  xchg %s, %s", op, ax);
    if (node->ty->is_unsigned) {
      println("  xor %%edx, %%edx");
      println("  div %s", op);
    } else {
      if (node->lhs->ty->size == 8)
        println("  cqo");
      else
        println("  cdq");
      println("  idiv %s", op);
    }

    if (node->kind == ND_MOD)
      println("  mov %%rdx, %%rax");
    return;
  case ND_BITAND:
    println("  and %s, %s", op, ax);
    return;
  case ND_BITOR:
    println("  or %s, %s", op, ax);
    return;
  case ND_BITXOR:
    println("  xor %s, %s", op, ax);
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
    println("  cmp %s, %s", ax, op);
    gen_cmp_setx(node->kind, node->lhs->ty->is_unsigned);
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
    gen_expr(node->cond);
    println("  test %%al, %%al");
    println("  je  .L.else.%d", c);
    gen_stmt(node->then);
    if (node->els)
      println("  jmp .L.end.%d", c);
    println(".L.else.%d:", c);
    if (node->els) {
      gen_stmt(node->els);
      println(".L.end.%d:", c);
    }
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    println(".L.begin.%d:", c);
    if (node->cond) {
      gen_expr(node->cond);
      println("  test %%al, %%al");
      println("  je %s", node->brk_label);
    }
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    if (node->inc)
      gen_void_expr(node->inc);
    println("  jmp .L.begin.%d", c);
    println("%s:", node->brk_label);
    gen_defr(node);
    return;
  }
  case ND_DO: {
    int c = count();
    println(".L.begin.%d:", c);
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    gen_expr(node->cond);
    println("  test %%al, %%al");
    println("  jne .L.begin.%d", c);
    println("%s:", node->brk_label);
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
        println("  je %s", n->label);
        continue;
      }
      if (n->begin == 0) {
        imm_cmp(ax, dx, n->end);
        println("  jbe %s", n->label);
        continue;
      }
      println("  mov %s, %s", ax, cx);
      imm_sub(cx, dx, n->begin);
      imm_cmp(cx, dx, n->end - n->begin);
      println("  jbe %s", n->label);
    }

    if (node->default_case)
      println("  jmp %s", node->default_case->label);

    println("  jmp %s", node->brk_label);
    gen_stmt(node->then);
    println("%s:", node->brk_label);
    return;
  }
  case ND_CASE:
    println("%s:", node->label);
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
    println("  jmp %s", node->unique_label);
    return;
  case ND_GOTO_EXPR:
    gen_expr(node->lhs);
    println("  jmp *%%rax");
    return;
  case ND_LABEL:
    println("%s:", node->unique_label);
    if (node->lhs)
      gen_stmt(node->lhs);
    return;
  case ND_RETURN: {
    if (!node->lhs) {
      if (has_defr(node))
        gen_defr(node);
      println("  jmp 9f");
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
        println("  jmp 9f");
        return;
      }
      copy_struct_mem();
      if (has_defr(node)) {
        push();
        gen_defr(node);
        pop("%rax");
      }
      println("  jmp 9f");
      return;
    }
    if (has_defr(node)) {
      push_by_ty(ty);
      gen_defr(node);
      pop_by_ty(ty);
    }
    println("  jmp 9f");
    return;
  }
  case ND_EXPR_STMT:
    gen_void_expr(node->lhs);
    return;
  case ND_ASM:
    println("  %s", node->asm_str);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

static void imm_tmpl(char *ins, char *op, int64_t val) {
  if (val == (int32_t)val) {
    println("  %s $%"PRIi64", %s", ins, val, op);
    return;
  }
  if (val == (uint32_t)val)
    println("  movl $%"PRIi64", %%edx", val);
  else
    println("  movabsq $%"PRIi64", %%rdx", val);
  println("  %s %%rdx, %s", ins, op);
  return;
}

static void memop_arith(Node *lhs, Node *rhs, char *ins) {
  char ins_sz[64];
  snprintf(ins_sz, 64, "%s%s", ins, size_suffix(lhs->ty->size));

  int64_t rval;
  char var_ofs[64], *var_ptr;
  if (is_const_expr(rhs, &rval)) {
    if (is_memop(lhs, var_ofs, &var_ptr, false)) {
      char memop[80];
      snprintf(memop, 80, "%s(%s)", var_ofs, var_ptr);
      imm_tmpl(ins_sz, memop, limit_imm(rval, lhs->ty->size));
      return;
    }
    gen_addr(lhs);
    imm_tmpl(ins_sz, "(%rax)", limit_imm(rval, lhs->ty->size));
    return;
  }

  if (is_memop(lhs, var_ofs, &var_ptr, false)) {
    gen_expr(rhs);
    println("  %s %s, %s(%s)", ins_sz, reg_ax(lhs->ty->size), var_ofs, var_ptr);
    return;
  }
  gen_addr(lhs);
  push();
  gen_expr(rhs);
  char *ptr = pop_inreg(tmpreg64[0]);
  println("  %s %s, (%s)", ins_sz, reg_ax(lhs->ty->size), ptr);
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
  if (is_gp_ty(node->lhs->ty) && !is_bitfield(node->lhs) && !node->lhs->ty->is_atomic &&
    is_const_expr(node->rhs, NULL)) {
    memop_arith(node->lhs, node->rhs, "mov");
    return;
  }

  char sofs[64], *sptr;
  if (is_memop(node->rhs, sofs, &sptr, true)) {
    char dofs[64], *dptr;
    if (is_memop(node->lhs, dofs, &dptr, false)) {
      gen_mem_copy2(sofs, sptr, dofs, dptr, write_size(node->lhs));
      return;
    } else if (!is_bitfield(node->lhs) && !node->lhs->ty->is_atomic) {
      gen_addr(node->lhs);
      gen_mem_copy2(sofs, sptr, "0", "%rax", write_size(node->lhs));
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
    println("  %s $%"PRIi64", %s", arith_ins(kind), val, op);
    return;
  }
  println("  mov $%"PRIi64", %s", val, tmp);
  println("  %s %s, %s", arith_ins(kind), tmp, op);
  return;
}

static void imm_add(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0: return;
  case 1: println("  inc %s", op); return;
  case -1: println("  dec %s", op); return;
  }
  imm_arith2(ND_ADD, op, tmp, val);
}

static void imm_sub(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0: return;
  case 1: println("  dec %s", op); return;
  case -1: println("  inc %s", op); return;
  }
  imm_arith2(ND_SUB, op, tmp, val);
}

static void imm_and(char *op, char *tmp, int64_t val) {
  switch (val) {
  case 0: println("  xor %s, %s", op, op); return;
  case -1: return;
  }
  imm_arith2(ND_BITAND, op, tmp, val);
}

static void imm_cmp(char *op, char *tmp, int64_t val) {
  if (val == 0) {
    println("  test %s, %s", op, op);
    return;
  }
  if (in_imm_range(val)) {
    println("  cmp $%"PRIi64", %s", val, op);
    return;
  }
  println("  mov $%"PRIi64", %s", val, tmp);
  println("  cmp %s, %s", tmp, op);
}

static void imm_arith(NodeKind kind, Type *ty, int64_t val) {
  char *ax = reg_ax(ty->size);
  char *dx = reg_dx(ty->size);

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
      println("  xor %%eax, %%eax");
      return;
    }
  }

  if (val == 1)
    if (kind == ND_MUL)
      return;

  if (val == -1) {
    switch (kind) {
    case ND_MUL:
      println("  neg %s", ax);
      return;
    case ND_BITOR:
      println("  mov $-1, %s", ax);
      return;
    case ND_BITXOR:
      println("  not %s", ax);
      return;
    }
  }

  if (kind == ND_MUL && is_pow_of_two(val)) {
    for (int i = 1; i < ty->size * 8; i++) {
      if (1LL << i == val) {
        println("  shl $%d, %s", i, ax);
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
      println("  xor %%eax, %%eax");
    return true;
  }

  if (val == -1) {
    gen_expr(expr);

    if (!ty->is_unsigned) {
      if (kind == ND_DIV)
        println("  neg %s", ax);
      else
        println("  xor %%eax, %%eax");
      return true;
    }

    if (kind == ND_DIV) {
      println("  cmp $-1, %s", ax);
      gen_cmp_setx(ND_EQ, false);
      return true;
    }

    println("  xor %%edx, %%edx");
    println("  cmp $-1, %s", ax);
    println("  cmove %s, %s", dx, ax);
    return true;
  }

  if (kind == ND_DIV && is_pow_of_two(val) && ty->is_unsigned) {
    for (int i = 1; i < ty->size * 8; i++) {
      if (1LL << i == val) {
        gen_expr(expr);
        println("  shr $%d, %s", i, ax);
        return true;
      }
    }
  }

  if (kind == ND_MOD && is_pow_of_two(val) && ty->is_unsigned && val != 0) {
    gen_expr(expr);

    uint64_t msk = val - 1;
    if (msk == UINT32_MAX) {
      println("  movl %%eax, %%eax");
      return true;
    }
    if (msk <= INT32_MAX) {
      println("  and $%d, %%eax", (int)msk);
      return true;
    }
    imm_and(ax, dx, msk);
    return true;
  }

  return false;
}

static bool gen_cmp_opt(Node *lhs, Node *rhs) {
  char var_ofs[64], *var_ptr;
  int64_t val;
  if (is_const_expr(rhs, &val)) {
    if (is_memop(lhs, var_ofs, &var_ptr, false)) {
      char memop[80];
      snprintf(memop, 80, "%s(%s)", var_ofs, var_ptr);
      imm_tmpl(lhs->ty->size == 8 ? "cmpq" : "cmpl", memop, limit_imm(val, lhs->ty->size));
      return true;
    }
    gen_expr(lhs);
    imm_cmp(regop_ax(lhs->ty), "%rdx", limit_imm(val, lhs->ty->size));
    return true;
  }

  if (is_memop(lhs, var_ofs, &var_ptr, false)) {
    gen_expr(rhs);
    println("  cmp %s, %s(%s)", regop_ax(lhs->ty), var_ofs, var_ptr);
    return true;
  }
  return false;
}

static bool lvar_rhs_arith_opt(NodeKind kind, Node *lhs, Node *rhs) {
  char *ax = reg_ax(rhs->ty->size);
  char *dx = reg_dx(rhs->ty->size);

  char *var_ptr;
  char var_ofs[64];

  if (is_int_to_int_cast(rhs)) {
    if (is_memop(rhs->lhs, var_ofs, &var_ptr, true) &&
      (rhs->ty->size > rhs->lhs->ty->size)) {
      gen_expr(lhs);

      if (!rhs->lhs->ty->is_unsigned && rhs->ty->size == 8)
        load_extend_int64(rhs->lhs->ty, var_ofs, var_ptr, "%rdx");
      else
        load_extend_int2(rhs->lhs->ty, var_ofs, var_ptr, "%edx");

      println("  %s %s, %s", arith_ins(kind), dx, ax);
      return true;
    }
    if (is_memop(rhs->lhs, var_ofs, &var_ptr, false) &&
      (rhs->ty->size <= rhs->lhs->ty->size)) {
      gen_expr(lhs);
      println("  %s %s(%s), %s", arith_ins(kind), var_ofs, var_ptr, ax);
      return true;
    }
  }

  if (is_memop(rhs, var_ofs, &var_ptr, false)) {
    gen_expr(lhs);
    println("  %s %s(%s), %s", arith_ins(kind), var_ofs, var_ptr, ax);
    return true;
  }
  return false;
}

static bool lvar_rhs_shift_opt(NodeKind kind, Node *node, Type *ty) {
  char *ax = reg_ax(ty->size);

  println("  mov %d(%s), %%cl", node->var->ofs, node->var->ptr);
  println("  %s %%cl, %s", arith_ins(kind), ax);
  return true;
}

static bool gen_gp_opt(Node *node) {
  NodeKind kind = node->kind;
  Node *lhs = node->lhs;
  Node *rhs = node->rhs;
  Type *ty = node->ty;

  switch (kind) {
  case ND_ADD:
  case ND_MUL:
  case ND_BITAND:
  case ND_BITOR:
  case ND_BITXOR:
    if (lhs->kind == ND_NUM)
      return gen_expr(rhs), imm_arith(kind, ty, lhs->val), true;
  case ND_SUB:
    if (rhs->kind == ND_NUM)
      return gen_expr(lhs), imm_arith(kind, ty, rhs->val), true;
    return lvar_rhs_arith_opt(kind, lhs, rhs);
  case ND_SHL:
  case ND_SHR:
  case ND_SAR:
    if (rhs->kind == ND_NUM)
      return gen_expr(lhs), imm_arith(kind, ty, rhs->val), true;
    if (is_lvar(rhs))
      return gen_expr(lhs), lvar_rhs_shift_opt(kind, rhs, ty), true;
    return false;
  case ND_DIV:
  case ND_MOD:
    if (rhs->kind == ND_NUM)
      return divmod_opt(kind, ty, lhs, rhs->val);
    return false;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
  case ND_GT:
  case ND_GE:
    if (!is_gp_ty(lhs->ty))
      return false;
    if (gen_cmp_opt(lhs, rhs)) {
      gen_cmp_setx(kind, lhs->ty->is_unsigned);
      return true;
    }
    if (gen_cmp_opt(rhs, lhs)) {
      switch (kind) {
      case ND_LT: kind = ND_GT; break;
      case ND_LE: kind = ND_GE; break;
      case ND_GT: kind = ND_LT; break;
      case ND_GE: kind = ND_LE; break;
      }
      gen_cmp_setx(kind, lhs->ty->is_unsigned);
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
  if (is_lvar(node) && node->var->ty->kind == TY_ARRAY) {
    println("  lea %"PRIi64"(%s), %%rax", *ofs + node->var->ofs, node->var->ptr);
    *ofs = 0;
    return;
  }
  gen_expr(node);
}

static void gen_member_opt(Node *node, int64_t *ofs) {
  while (node->kind == ND_MEMBER) {
    *ofs += node->member->offset;
    node = node->lhs;
  }
  if (is_lvar(node)) {
    println("  lea %"PRIi64"(%s), %%rax", *ofs + node->var->ofs, node->var->ptr);
    *ofs = 0;
    return;
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

static bool gen_inv_cmp(Node *node) {
  if (!is_gp_ty(node->lhs->ty))
    return false;

  switch (node->kind) {
  case ND_EQ: node->kind = ND_NE; break;
  case ND_NE: node->kind = ND_EQ; break;
  case ND_LT: node->kind = ND_GE; break;
  case ND_LE: node->kind = ND_GT; break;
  case ND_GT: node->kind = ND_LE; break;
  case ND_GE: node->kind = ND_LT; break;
  default: internal_error();
  }
  gen_expr(node);
  return true;
}

static bool gen_bool_opt(Node *node) {
  Node *boolexpr = NULL;
  bool has_not = false;

  for (;; node = node->lhs) {
    switch (node->kind) {
    case ND_NOT:
      has_not = !has_not;
      continue;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
    case ND_GT:
    case ND_GE:
      if (has_not && gen_inv_cmp(node))
        return true;
    case ND_LOGOR:
    case ND_LOGAND:
      gen_expr(node);
      if (has_not)
        println("  xor $1, %%al");
      return true;
    }
    if (node->ty->kind == TY_BOOL)
      boolexpr = node;
    if (node->kind == ND_CAST && is_scalar(node->ty))
      continue;
    break;
  }

  if (boolexpr) {
    if (boolexpr->kind == ND_CAST && boolexpr->ty->kind == TY_BOOL) {
      gen_cmp_zero(boolexpr->lhs, has_not ? ND_EQ : ND_NE);
      return true;
    }
    gen_expr(boolexpr);
    if (has_not)
      println("  xor $1, %%al");
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
  char var_ofs[64];

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

  if (is_scalar(ty) && is_memop(node, var_ofs, &var_ptr, true)) {
    load3(ty, var_ofs, var_ptr);
    return true;
  }

  if (kind == ND_ASSIGN && is_memop(lhs, var_ofs, &var_ptr, false)) {
    gen_expr(rhs);
    store3(lhs->ty, var_ofs, var_ptr);
    return true;
  }

  if (is_gp_ty(ty) && gen_gp_opt(node))
    return true;

  if (kind == ND_COND && node->cond->kind == ND_NUM) {
    if (node->cond->val)
      gen_expr(node->then);
    else
      gen_expr(node->els);
    return true;
  }

  if (kind == ND_CAST && ty->kind == TY_BOOL)
    if (gen_bool_opt(lhs))
      return true;

  if (kind == ND_NOT && gen_bool_opt(node))
    return true;

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

  if (is_int_to_int_cast(node) && is_memop(lhs, var_ofs, &var_ptr, true)) {
    if (ty->size > lhs->ty->size) {
      if (!lhs->ty->is_unsigned && node->ty->size == 8)
        load_extend_int64(lhs->ty, var_ofs, var_ptr, "%rax");
      else
        load_extend_int2(lhs->ty, var_ofs, var_ptr, "%eax");
      return true;
    }
    if (ty->size < lhs->ty->size && ty->kind != TY_BOOL) {
      load_extend_int2(ty, var_ofs, var_ptr, "%eax");
      return true;
    }
  }

  return false;
}

static bool gen_addr_opt(Node *node) {
  NodeKind kind = node->kind;

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

static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (!var->is_definition)
      continue;

    if (var->ty->kind == TY_FUNC) {
      if (var->is_live)
        emit_data(var->static_lvars);
      continue;
    }

    if (var->is_static)
      println("  .local \"%s\"", var->name);
    else
      println("  .globl \"%s\"", var->name);

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    if (var->is_tentative) {
      if (var->ty->kind == TY_ARRAY && var->ty->size < 0)
        var->ty->size = var->ty->base->size;

      // Common symbol
      if (opt_fcommon) {
        println("  .comm \"%s\", %"PRIi64", %d", var->name, var->ty->size, align);
        continue;
      }
    }

    // .data or .tdata
    if (var->init_data) {
      if (var->is_tls && opt_data_sections)
        println("  .section .tdata.\"%s\",\"awT\",@progbits", var->name);
      else if (var->is_tls)
        println("  .section .tdata,\"awT\",@progbits");
      else if (opt_data_sections)
        println("  .section .data.\"%s\",\"aw\",@progbits", var->name);
      else
        println("  .data");

      println("  .type \"%s\", @object", var->name);
      println("  .size \"%s\", %"PRIi64, var->name, var->ty->size);
      println("  .align %d", align);
      println("\"%s\":", var->name);

      Relocation *rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          println("  .quad \"%s\"%+ld", *rel->label, rel->addend);
          rel = rel->next;
          pos += 8;
        } else {
          println("  .byte %d", var->init_data[pos++]);
        }
      }
      continue;
    }

    // .bss or .tbss
    if (var->is_tls && opt_data_sections)
      println("  .section .tbss.\"%s\",\"awT\",@nobits", var->name);
    else if (var->is_tls)
      println("  .section .tbss,\"awT\",@nobits");
    else if (opt_data_sections)
      println("  .section .bss.\"%s\",\"aw\",@nobits", var->name);
    else
      println("  .bss");

    println("  .align %d", align);
    println("\"%s\":", var->name);
    println("  .zero %"PRIi64, var->ty->size);
  }
}

static void store_fp(int r, int sz, int ofs, char *ptr) {
  switch (sz) {
  case 4:
    println("  movss %%xmm%d, %d(%s)", r, ofs, ptr);
    return;
  case 8:
    println("  movsd %%xmm%d, %d(%s)", r, ofs, ptr);
    return;
  }
  internal_error();
}

static void store_gp2(char **reg, int sz, int dofs, char *dptr) {
  for (int ofs = 0;;) {
    int rem = sz - ofs;
    int p2 = (rem >= 8) ? 8 : (rem >= 4) ? 4 : (rem >= 2) ? 2 : 1;
    switch (p2) {
    case 1: println("  mov %s, %d(%s)", reg[0], ofs + dofs, dptr); break;
    case 2: println("  mov %s, %d(%s)", reg[1], ofs + dofs, dptr); break;
    case 4: println("  mov %s, %d(%s)", reg[2], ofs + dofs, dptr); break;
    case 8: println("  mov %s, %d(%s)", reg[3], ofs + dofs, dptr); break;
    }
    ofs += p2;
    if (ofs >= sz)
      return;
    println("  shr $%d, %s", p2 * 8, reg[3]);
  }
}

static void store_gp(int r, int sz, int ofs, char *ptr) {
  store_gp2((char *[]){argreg8[r], argreg16[r], argreg32[r], argreg64[r]}, sz, ofs, ptr);
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (fn->ty->kind != TY_FUNC || !fn->is_definition)
      continue;

    // No code is emitted for "static inline" functions
    // if no one is referencing them.
    if (!fn->is_live)
      continue;

    if (fn->is_static)
      println("  .local \"%s\"", fn->name);
    else
      println("  .globl \"%s\"", fn->name);
    if (opt_func_sections)
      println("  .section .text.\"%s\",\"ax\",@progbits", fn->name);
    else
      println("  .text");
    println("  .type \"%s\", @function", fn->name);
    println("\"%s\":", fn->name);

    bool rtn_by_stk = fn->ty->return_ty->size > 16;
    int gp_count = rtn_by_stk;
    int fp_count = 0;
    int arg_stk_size = calling_convention(fn->ty->param_list, &gp_count, &fp_count, NULL);

    int lvar_align = get_lvar_align(fn->ty->scopes, 16);
    lvar_ptr = (lvar_align > 16) ? "%rbx" : "%rbp";
    current_fn = fn;

    // Prologue
    println("  push %%rbp");
    println("  mov %%rsp, %%rbp");
    if (lvar_align > 16) {
      println("  push %%rbx");
      println("  and $-%d, %%rsp", lvar_align);
      println("  mov %%rsp, %%rbx");
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
      case 0: println("  movq %%rdi, -176(%s)", lvar_ptr);
      case 1: println("  movq %%rsi, -168(%s)", lvar_ptr);
      case 2: println("  movq %%rdx, -160(%s)", lvar_ptr);
      case 3: println("  movq %%rcx, -152(%s)", lvar_ptr);
      case 4: println("  movq %%r8, -144(%s)", lvar_ptr);
      case 5: println("  movq %%r9, -136(%s)", lvar_ptr);
      }
      if (fp_count < 8) {
        println("  test %%al, %%al");
        println("  je 1f");
        switch (fp_count) {
        case 0: println("  movaps %%xmm0, -128(%s)", lvar_ptr);
        case 1: println("  movaps %%xmm1, -112(%s)", lvar_ptr);
        case 2: println("  movaps %%xmm2, -96(%s)", lvar_ptr);
        case 3: println("  movaps %%xmm3, -80(%s)", lvar_ptr);
        case 4: println("  movaps %%xmm4, -64(%s)", lvar_ptr);
        case 5: println("  movaps %%xmm5, -48(%s)", lvar_ptr);
        case 6: println("  movaps %%xmm6, -32(%s)", lvar_ptr);
        case 7: println("  movaps %%xmm7, -16(%s)", lvar_ptr);
        }
        println("1:");
      }
    }

    if (fn->dealloc_vla) {
      vla_base_ofs = lvar_stk_sz += 8;
      println("  mov %%rsp, -%d(%s)", vla_base_ofs, lvar_ptr);
    }

    if (rtn_by_stk) {
      rtn_ptr_ofs = lvar_stk_sz += 8;
      println("  mov %s, -%d(%s)", argreg64[0], rtn_ptr_ofs, lvar_ptr);
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
      insrtln("  sub $%d, %%rsp", stack_alloc_loc, align_to(peak_stk_usage, 16));

    // [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
    // a special rule for the main function. Reaching the end of the
    // main function is equivalent to returning 0, even though the
    // behavior is undefined for the other functions.
    if (strcmp(fn->name, "main") == 0)
      println("  xor %%eax, %%eax");

    // Epilogue
    println("9:");
    if (lvar_align > 16)
      println("  mov -8(%%rbp), %%rbx");
    println("  leave");
    println("  ret");
  }
}

void codegen(Obj *prog, FILE *out) {
  output_file = out;

  if (opt_g) {
    File **files = get_input_files();
    for (int i = 0; files[i]; i++)
      println("  .file %d \"%s\"", files[i]->file_no, files[i]->name);
  }
  emit_data(prog);
  emit_text(prog);
  println("  .section  .note.GNU-stack,\"\",@progbits");
}

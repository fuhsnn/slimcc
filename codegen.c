#include "slimcc.h"

#define GP_MAX 6
#define FP_MAX 8

static FILE *output_file;
static char *argreg8[] = {"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
static char *argreg16[] = {"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"};
static char *argreg32[] = {"%edi", "%esi", "%edx", "%ecx", "%r8d", "%r9d"};
static char *argreg64[] = {"%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"};
static Obj *current_fn;
static char *lvar_ptr;

bool dont_reuse_stack;

struct {
  int *data;
  int capacity;
  int depth;
  int bottom;
} static tmp_stack;

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

__attribute__((format(printf, 1, 2)))
static void println(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(output_file, fmt, ap);
  va_end(ap);
  fprintf(output_file, "\n");
}

static int count(void) {
  static int i = 1;
  return i++;
}

static int push_tmpstack(void) {
  if (tmp_stack.depth == tmp_stack.capacity) {
    tmp_stack.capacity += 4;
    tmp_stack.data = realloc(tmp_stack.data, sizeof(int) * tmp_stack.capacity);
  }

  int offset;
  if (!dont_reuse_stack) {
      int bottom = current_fn->lvar_stack_size + (tmp_stack.depth + 1) * 8;
    tmp_stack.bottom = MAX(tmp_stack.bottom, bottom);
    offset = -bottom;
  } else {
    tmp_stack.bottom += 8;
    offset = -tmp_stack.bottom;
  }

  tmp_stack.data[tmp_stack.depth] = offset;
  tmp_stack.depth++;
  return offset;
}

static int pop_tmpstack(void) {
  tmp_stack.depth--;
  return tmp_stack.data[tmp_stack.depth];
}

static int push_tmp(void) {
  int offset = push_tmpstack();
  println("  mov %%rax, %d(%s)", offset, lvar_ptr);
  return offset;
}

static void pop_tmp(char *arg) {
  int offset = pop_tmpstack();
  println("  mov %d(%s), %s", offset, lvar_ptr, arg);
}

static void push_tmpf(void) {
  int offset = push_tmpstack();
  println("  movsd %%xmm0, %d(%s)", offset, lvar_ptr);
}

static void pop_tmpf(int reg) {
  int offset = pop_tmpstack();
  println("  movsd %d(%s), %%xmm%d", offset, lvar_ptr, reg);
}

// When we load a char or a short value to a register, we always
// extend them to the size of int, so we can assume the lower half of
// a register always contains a valid value.
static void load_extend_int(Type *ty, int ofs, char *ptr, char *reg) {
  char *insn = ty->is_unsigned ? "movz" : "movs";
  switch (ty->size) {
  case 1: println("  %sbl %d(%s), %s", insn, ofs, ptr, reg); return;
  case 2: println("  %swl %d(%s), %s", insn, ofs, ptr, reg); return;
  case 4: println("  movl %d(%s), %s", ofs, ptr, reg); return;
  case 8: println("  mov %d(%s), %s", ofs, ptr, reg); return;
  }
  unreachable();
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
  unreachable();
}

static char *reg_ax(int sz) {
  switch (sz) {
  case 1: return "%al";
  case 2: return "%ax";
  case 4: return "%eax";
  case 8: return "%rax";
  }
  unreachable();
}

// Compute the absolute address of a given node.
// It's an error if a given node does not reside in memory.
static void gen_addr(Node *node) {
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
        println("  data16 lea %s@tlsgd(%%rip), %%rdi", node->var->name);
        println("  .value 0x6666");
        println("  rex64");
        println("  call __tls_get_addr@PLT");
        return;
      }

      // Function or global variable
      println("  mov %s@GOTPCREL(%%rip), %%rax", node->var->name);
      return;
    }

    // Thread-local variable
    if (node->var->is_tls) {
      println("  mov %%fs:0, %%rax");
      println("  add $%s@tpoff, %%rax", node->var->name);
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
        println("  lea %s(%%rip), %%rax", node->var->name);
      else
        println("  mov %s@GOTPCREL(%%rip), %%rax", node->var->name);
      return;
    }

    // Global variable
    println("  lea %s(%%rip), %%rax", node->var->name);
    return;
  case ND_DEREF:
    gen_expr(node->lhs);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_addr(node->rhs);
    return;
  case ND_MEMBER:
    switch(node->lhs->kind) {
    case ND_FUNCALL:
      if (!node->lhs->ret_buffer)
        break;
    case ND_ASSIGN:
    case ND_COND:
    case ND_STMT_EXPR:
      if (node->lhs->ty->kind != TY_STRUCT && node->lhs->ty->kind != TY_UNION)
        break;
      gen_expr(node->lhs);
      println("  add $%d, %%rax", node->member->offset);
      return;
    default:
      gen_addr(node->lhs);
      println("  add $%d, %%rax", node->member->offset);
      return;
    }
  }

  error_tok(node->tok, "not an lvalue");
}

// Load a value from where %rax is pointing to.
static void load(Type *ty) {
  switch (ty->kind) {
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
  case TY_FLOAT:
    println("  movss (%%rax), %%xmm0");
    return;
  case TY_DOUBLE:
    println("  movsd (%%rax), %%xmm0");
    return;
  case TY_LDOUBLE:
    println("  fldt (%%rax)");
    return;
  }

  char *ax = ty->size <= 4 ? "%eax" : "%rax";
  load_extend_int(ty, 0, "%rax", ax);
}

// Store %rax to an address that the stack top is pointing to.
static void store(Type *ty) {
  pop_tmp("%rdi");

  switch (ty->kind) {
  case TY_STRUCT:
  case TY_UNION:
    for (int i = 0; i < ty->size; i++) {
      println("  mov %d(%%rax), %%r8b", i);
      println("  mov %%r8b, %d(%%rdi)", i);
    }
    return;
  case TY_FLOAT:
    println("  movss %%xmm0, (%%rdi)");
    return;
  case TY_DOUBLE:
    println("  movsd %%xmm0, (%%rdi)");
    return;
  case TY_LDOUBLE:
    println("  fstpt (%%rdi)");
    return;
  }

  if (ty->size == 1)
    println("  mov %%al, (%%rdi)");
  else if (ty->size == 2)
    println("  mov %%ax, (%%rdi)");
  else if (ty->size == 4)
    println("  mov %%eax, (%%rdi)");
  else
    println("  mov %%rax, (%%rdi)");
}

static void cmp_zero(Type *ty) {
  switch (ty->kind) {
  case TY_FLOAT:
    println("  xorps %%xmm1, %%xmm1");
    println("  ucomiss %%xmm1, %%xmm0");
    return;
  case TY_DOUBLE:
    println("  xorpd %%xmm1, %%xmm1");
    println("  ucomisd %%xmm1, %%xmm0");
    return;
  case TY_LDOUBLE:
    println("  fldz");
    println("  fucomip");
    println("  fstp %%st(0)");
    return;
  }

  if (is_integer(ty) && ty->size <= 4)
    println("  cmp $0, %%eax");
  else
    println("  cmp $0, %%rax");
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
static char i32f80[] = "mov %eax, -4(%rsp); fildl -4(%rsp)";

static char u32f32[] = "mov %eax, %eax; cvtsi2ssq %rax, %xmm0";
static char u32i64[] = "mov %eax, %eax";
static char u32f64[] = "mov %eax, %eax; cvtsi2sdq %rax, %xmm0";
static char u32f80[] = "mov %eax, %eax; mov %rax, -8(%rsp); fildll -8(%rsp)";

static char i64f32[] = "cvtsi2ssq %rax, %xmm0";
static char i64f64[] = "cvtsi2sdq %rax, %xmm0";
static char i64f80[] = "movq %rax, -8(%rsp); fildll -8(%rsp)";

static char u64f32[] =
  "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2ss %rax,%xmm0; jmp 2f; "
  "1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; "
  "or %rax,%rdi; cvtsi2ss %rdi,%xmm0; addss %xmm0,%xmm0; 2:";
static char u64f64[] =
  "test %rax,%rax; js 1f; pxor %xmm0,%xmm0; cvtsi2sd %rax,%xmm0; jmp 2f; "
  "1: mov %rax,%rdi; and $1,%eax; pxor %xmm0,%xmm0; shr %rdi; "
  "or %rax,%rdi; cvtsi2sd %rdi,%xmm0; addsd %xmm0,%xmm0; 2:";
static char u64f80[] =
  "mov %rax, -8(%rsp); fildq -8(%rsp); test %rax, %rax; jns 1f;"
  "mov $1602224128, %eax; mov %eax, -4(%rsp); fadds -4(%rsp); 1:";

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
static char f32f80[] = "movss %xmm0, -4(%rsp); flds -4(%rsp)";

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
static char f64f80[] = "movsd %xmm0, -8(%rsp); fldl -8(%rsp)";

#define FROM_F80_1                                           \
  "fnstcw -10(%rsp); movzwl -10(%rsp), %eax; or $12, %ah; " \
  "mov %ax, -12(%rsp); fldcw -12(%rsp); "

#define FROM_F80_2 " -24(%rsp); fldcw -10(%rsp); "

static char f80i8[] = FROM_F80_1 "fistps" FROM_F80_2 "movsbl -24(%rsp), %eax";
static char f80u8[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%rsp), %eax";
static char f80i16[] = FROM_F80_1 "fistps" FROM_F80_2 "movzbl -24(%rsp), %eax";
static char f80u16[] = FROM_F80_1 "fistpl" FROM_F80_2 "movswl -24(%rsp), %eax";
static char f80i32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%rsp), %eax";
static char f80u32[] = FROM_F80_1 "fistpl" FROM_F80_2 "mov -24(%rsp), %eax";
static char f80i64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%rsp), %rax";
static char f80u64[] = FROM_F80_1 "fistpq" FROM_F80_2 "mov -24(%rsp), %rax";
static char f80f32[] = "fstps -8(%rsp); movss -8(%rsp), %xmm0";
static char f80f64[] = "fstpl -8(%rsp); movsd -8(%rsp), %xmm0";

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

static void cast(Type *from, Type *to) {
  if (to->kind == TY_VOID)
    return;

  if (to->kind == TY_BOOL) {
    cmp_zero(from);
    println("  setne %%al");
    println("  movzx %%al, %%eax");
    return;
  }

  int t1 = getTypeId(from);
  int t2 = getTypeId(to);
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
    for (Member *mem = ty->members; mem; mem = mem->next)
      if (!has_flonum(mem->ty, lo, hi, offset + mem->offset))
        return false;
    return true;
  }

  if (ty->kind == TY_ARRAY) {
    for (int i = 0; i < ty->array_len; i++)
      if (!has_flonum(ty->base, lo, hi, offset + ty->base->size * i))
        return false;
    return true;
  }

  return offset < lo || hi <= offset || ty->kind == TY_FLOAT || ty->kind == TY_DOUBLE;
}

static bool has_flonum1(Type *ty) {
  return has_flonum(ty, 0, 8, 0);
}

static bool has_flonum2(Type *ty) {
  return has_flonum(ty, 8, 16, 0);
}

static int calling_convention(Obj *var, int gp_start, int *gp_count, int *fp_count, int *stack_align) {
  int stack = 0;
  int max_align = 16;
  int gp = gp_start, fp = 0;
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
  if (gp_count)
    *gp_count = MIN(gp, GP_MAX);
  if (fp_count)
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

    switch (var->ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      for (int i = 0; i < var->ty->size; i++) {
        println("  mov %d(%s), %%r8b", i + var->ofs, var->ptr);
        println("  mov %%r8b, %d(%%rsp)", i + var->stack_offset);
      }
      continue;
    case TY_FLOAT:
    case TY_DOUBLE:
      println("  movsd %d(%s), %%xmm0", var->ofs, var->ptr);
      println("  movsd %%xmm0, %d(%%rsp)", var->stack_offset);
      continue;
    case TY_LDOUBLE:
      println("  fldt %d(%s)", var->ofs, var->ptr);
      println("  fstpt %d(%%rsp)", var->stack_offset);
      continue;
    }

    char *ax = var->ty->size <= 4 ? "%eax" : "%rax";
    load_extend_int(var->ty, var->ofs, var->ptr, ax);
    println("  mov %%rax, %d(%%rsp)", var->stack_offset);
  }
}

static void place_reg_args(Node *node, bool gp_start) {
  int gp = 0, fp = 0;
  // If the return type is a large struct/union, the caller passes
  // a pointer to a buffer as if it were the first argument.
  if (gp_start)
    println("  lea %d(%s), %s", node->ret_buffer->ofs, node->ret_buffer->ptr, argreg64[gp++]);

  for (Obj *var = node->args; var; var = var->param_next) {
    if (var->pass_by_stack)
      continue;

    switch (var->ty->kind) {
    case TY_STRUCT:
    case TY_UNION:
      if (has_flonum1(var->ty))
        println("  movsd %d(%s), %%xmm%d", var->ofs, var->ptr, fp++);
      else
        println("  mov %d(%s), %s",  var->ofs, var->ptr, argreg64[gp++]);

      if (var->ty->size > 8) {
        if (has_flonum2(var->ty))
          println("  movsd %d(%s), %%xmm%d", 8 + var->ofs, var->ptr, fp++);
        else
          println("  mov %d(%s), %s",  8 + var->ofs, var->ptr, argreg64[gp++]);
      }
      continue;
    case TY_FLOAT:
      println("  movss %d(%s), %%xmm%d", var->ofs, var->ptr, fp++);
      continue;
    case TY_DOUBLE:
      println("  movsd %d(%s), %%xmm%d", var->ofs, var->ptr, fp++);
      continue;
    }

    char *argreg = var->ty->size <= 4 ? argreg32[gp++] : argreg64[gp++];
    load_extend_int(var->ty, var->ofs, var->ptr, argreg);
  }
}

static void copy_ret_buffer(Obj *var) {
  Type *ty = var->ty;
  int gp = 0, fp = 0;

  if (has_flonum1(ty)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4)
      println("  movss %%xmm0, %d(%s)", var->ofs, var->ptr);
    else
      println("  movsd %%xmm0, %d(%s)", var->ofs, var->ptr);
    fp++;
  } else {
    for (int i = 0; i < MIN(8, ty->size); i++) {
      println("  mov %%al, %d(%s)", var->ofs + i, var->ptr);
      println("  shr $8, %%rax");
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12)
        println("  movss %%xmm%d, %d(%s)", fp, var->ofs + 8, var->ptr);
      else
        println("  movsd %%xmm%d, %d(%s)", fp, var->ofs + 8, var->ptr);
    } else {
      char *reg1 = (gp == 0) ? "%al" : "%dl";
      char *reg2 = (gp == 0) ? "%rax" : "%rdx";
      for (int i = 8; i < MIN(16, ty->size); i++) {
        println("  mov %s, %d(%s)", reg1, var->ofs + i, var->ptr);
        println("  shr $8, %s", reg2);
      }
    }
  }
}

static void copy_struct_reg(void) {
  Type *ty = current_fn->ty->return_ty;
  int gp = 0, fp = 0;

  println("  mov %%rax, %%rdi");

  if (has_flonum1(ty)) {
    assert(ty->size == 4 || 8 <= ty->size);
    if (ty->size == 4)
      println("  movss (%%rdi), %%xmm0");
    else
      println("  movsd (%%rdi), %%xmm0");
    fp++;
  } else {
    println("  mov $0, %%rax");
    for (int i = MIN(8, ty->size) - 1; i >= 0; i--) {
      println("  shl $8, %%rax");
      println("  mov %d(%%rdi), %%al", i);
    }
    gp++;
  }

  if (ty->size > 8) {
    if (has_flonum2(ty)) {
      assert(ty->size == 12 || ty->size == 16);
      if (ty->size == 12)
        println("  movss 8(%%rdi), %%xmm%d", fp);
      else
        println("  movsd 8(%%rdi), %%xmm%d", fp);
    } else {
      char *reg1 = (gp == 0) ? "%al" : "%dl";
      char *reg2 = (gp == 0) ? "%rax" : "%rdx";
      println("  mov $0, %s", reg2);
      for (int i = MIN(16, ty->size) - 1; i >= 8; i--) {
        println("  shl $8, %s", reg2);
        println("  mov %d(%%rdi), %s", i, reg1);
      }
    }
  }
}

static void copy_struct_mem(void) {
  Type *ty = current_fn->ty->return_ty;
  Obj *var = current_fn->ty->param_list;

  println("  mov %d(%s), %%rdi", var->ofs, var->ptr);

  for (int i = 0; i < ty->size; i++) {
    println("  mov %d(%%rax), %%dl", i);
    println("  mov %%dl, %d(%%rdi)", i);
  }
  println("  mov %%rdi, %%rax");
}

static void builtin_alloca(Node *node) {
  // Shift the temporary area by %rdi.
  println("  sub %%rax, %%rsp");
  // Align frame pointer
  int align = node->val > 16 ? node->val : 16;
  println("  and $-%d, %%rsp", align);
  if (node->var) {
    println("  mov %%rsp, %d(%s)", node->var->ofs, node->var->ptr);
    println("  mov %%rsp, %d(%s)", node->top_vla->ofs, node->top_vla->ptr);
  }
  println("  mov %%rsp, %%rax");
}

static void dealloc_vla(Node *node) {
  if (!current_fn->vla_base || node->top_vla == node->target_vla)
    return;
  Obj *vla;
  if (node->target_vla)
    vla = node->target_vla;
  else
    vla = current_fn->vla_base;
  println("  mov %d(%s), %%rsp", vla->ofs, vla->ptr);
}

static void print_loc(Token *tok) {
  static int file_no, line_no;

  if (file_no == tok->display_file_no && line_no == tok->display_line_no)
    return;

  println("  .loc %d %d", tok->display_file_no, tok->display_line_no);

  file_no = tok->display_file_no;
  line_no = tok->display_line_no;
}

// Generate code for a given node.
static void gen_expr(Node *node) {
  if (opt_g)
    print_loc(node->tok);

  switch (node->kind) {
  case ND_NULL_EXPR:
    return;
  case ND_NUM: {
    switch (node->ty->kind) {
    case TY_FLOAT: {
      union { float f32; uint32_t u32; } u = { node->fval };
      println("  mov $%u, %%eax  # float %Lf", u.u32, node->fval);
      println("  movq %%rax, %%xmm0");
      return;
    }
    case TY_DOUBLE: {
      union { double f64; uint64_t u64; } u = { node->fval };
      println("  mov $%lu, %%rax  # double %Lf", u.u64, node->fval);
      println("  movq %%rax, %%xmm0");
      return;
    }
    case TY_LDOUBLE: {
      union { long double f80; uint64_t u64[2]; } u;
      memset(&u, 0, sizeof(u));
      u.f80 = node->fval;
      println("  mov $%lu, %%rax  # long double %Lf", u.u64[0], node->fval);
      println("  mov %%rax, -16(%%rsp)");
      println("  mov $%lu, %%rax", u.u64[1]);
      println("  mov %%rax, -8(%%rsp)");
      println("  fldt -16(%%rsp)");
      return;
    }
    }

    println("  mov $%ld, %%rax", node->val);
    return;
  }
  case ND_POS:
    gen_expr(node->lhs);
    return;
  case ND_NEG:
    gen_expr(node->lhs);

    switch (node->ty->kind) {
    case TY_FLOAT:
      println("  mov $1, %%rax");
      println("  shl $31, %%rax");
      println("  movq %%rax, %%xmm1");
      println("  xorps %%xmm1, %%xmm0");
      return;
    case TY_DOUBLE:
      println("  mov $1, %%rax");
      println("  shl $63, %%rax");
      println("  movq %%rax, %%xmm1");
      println("  xorpd %%xmm1, %%xmm0");
      return;
    case TY_LDOUBLE:
      println("  fchs");
      return;
    }

    println("  neg %%rax");
    return;
  case ND_VAR:
    gen_addr(node);
    load(node->ty);
    return;
  case ND_MEMBER: {
    gen_addr(node);
    load(node->ty);

    Member *mem = node->member;
    if (mem->is_bitfield) {
      println("  shl $%d, %%rax", 64 - mem->bit_width - mem->bit_offset);
      if (mem->ty->is_unsigned)
        println("  shr $%d, %%rax", 64 - mem->bit_width);
      else
        println("  sar $%d, %%rax", 64 - mem->bit_width);
    }
    return;
  }
  case ND_DEREF:
    gen_expr(node->lhs);
    load(node->ty);
    return;
  case ND_ADDR:
    gen_addr(node->lhs);
    return;
  case ND_ASSIGN:
    gen_addr(node->lhs);
    int tmp_offset = push_tmp();
    gen_expr(node->rhs);

    if (is_bitfield(node->lhs)) {
      // If the lhs is a bitfield, we need to read the current value
      // from memory and merge it with a new value.
      Member *mem = node->lhs->member;
      println("  mov $%ld, %%rdi", (1L << mem->bit_width) - 1);
      println("  and %%rdi, %%rax");
      println("  mov %%rax, %%r8");

      println("  mov %d(%s), %%rax", tmp_offset, lvar_ptr);
      load(mem->ty);

      long mask = ((1L << mem->bit_width) - 1) << mem->bit_offset;
      println("  mov $%ld, %%rdi", ~mask);
      println("  and %%rdi, %%rax");

      println("  mov %%r8, %%rdi");
      println("  shl $%d, %%rdi", mem->bit_offset);
      println("  or %%rdi, %%rax");
      store(node->ty);
      println("  mov %%r8, %%rax");

      if (!mem->ty->is_unsigned) {
        println("  shl $%d, %%rax", 64 - mem->bit_width);
        println("  sar $%d, %%rax", 64 - mem->bit_width);
      }
      return;
    }

    store(node->ty);
    return;
  case ND_STMT_EXPR:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    dealloc_vla(node);
    return;
  case ND_COMMA:
    gen_expr(node->lhs);
    gen_expr(node->rhs);
    return;
  case ND_CAST:
    gen_expr(node->lhs);
    cast(node->lhs->ty, node->ty);
    return;
  case ND_MEMZERO:
    // `rep stosb` is equivalent to `memset(%rdi, %al, %rcx)`.
    println("  mov $%d, %%rcx", node->var->ty->size);
    println("  lea %d(%s), %%rdi", node->var->ofs, node->var->ptr);
    println("  mov $0, %%al");
    println("  rep stosb");
    return;
  case ND_COND: {
    int c = count();
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
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
    cmp_zero(node->lhs->ty);
    println("  sete %%al");
    println("  movzx %%al, %%rax");
    return;
  case ND_BITNOT:
    gen_expr(node->lhs);
    println("  not %%rax");
    return;
  case ND_LOGAND: {
    int c = count();
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    println("  je .L.false.%d", c);
    gen_expr(node->rhs);
    cmp_zero(node->rhs->ty);
    println("  je .L.false.%d", c);
    println("  mov $1, %%rax");
    println("  jmp .L.end.%d", c);
    println(".L.false.%d:", c);
    println("  mov $0, %%rax");
    println(".L.end.%d:", c);
    return;
  }
  case ND_LOGOR: {
    int c = count();
    gen_expr(node->lhs);
    cmp_zero(node->lhs->ty);
    println("  jne .L.true.%d", c);
    gen_expr(node->rhs);
    cmp_zero(node->rhs->ty);
    println("  jne .L.true.%d", c);
    println("  mov $0, %%rax");
    println("  jmp .L.end.%d", c);
    println(".L.true.%d:", c);
    println("  mov $1, %%rax");
    println(".L.end.%d:", c);
    return;
  }
  case ND_FUNCALL: {
    if (node->lhs->kind == ND_VAR && !strcmp(node->lhs->var->name, "alloca")) {
      gen_expr(node->args_expr);
      builtin_alloca(node);
      return;
    }

    println("  mov %%rsp, %%rax");
    push_tmp();

    gen_expr(node->lhs);
    push_tmp();

    if (node->args_expr)
      gen_expr(node->args_expr);

    // If the return type is a large struct/union, the caller passes
    // a pointer to a buffer as if it were the first argument.
    bool gp_start = node->ret_buffer && node->ty->size > 16;

    int fp_count, stack_align;
    int args_size = calling_convention(node->args, gp_start, NULL, &fp_count, &stack_align);

    println("  sub $%d, %%rsp", args_size);
    println("  and $-%d, %%rsp", stack_align);

    place_stack_args(node);
    place_reg_args(node, gp_start);

    println("  mov $%d, %%rax", fp_count);
    pop_tmp("%r10");
    println("  call *%%r10");

    pop_tmp("%rsp");

    // It looks like the most significant 48 or 56 bits in RAX may
    // contain garbage if a function return type is short or bool/char,
    // respectively. We clear the upper bits here.
    switch (node->ty->kind) {
    case TY_BOOL:
      println("  movzx %%al, %%eax");
      return;
    case TY_PCHAR:
    case TY_CHAR:
      if (node->ty->is_unsigned)
        println("  movzbl %%al, %%eax");
      else
        println("  movsbl %%al, %%eax");
      return;
    case TY_SHORT:
      if (node->ty->is_unsigned)
        println("  movzwl %%ax, %%eax");
      else
        println("  movswl %%ax, %%eax");
      return;
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
    push_tmp();
    gen_expr(node->cas_new);
    push_tmp();
    gen_expr(node->cas_old);
    println("  mov %%rax, %%r8");
    load(node->cas_old->ty->base);
    pop_tmp("%rdx"); // new
    pop_tmp("%rdi"); // addr

    int sz = node->cas_addr->ty->base->size;
    println("  lock cmpxchg %s, (%%rdi)", reg_dx(sz));
    println("  sete %%cl");
    println("  je 1f");
    println("  mov %s, (%%r8)", reg_ax(sz));
    println("1:");
    println("  movzbl %%cl, %%eax");
    return;
  }
  case ND_EXCH: {
    gen_expr(node->lhs);
    push_tmp();
    gen_expr(node->rhs);
    pop_tmp("%rdi");

    int sz = node->lhs->ty->base->size;
    println("  xchg %s, (%%rdi)", reg_ax(sz));
    return;
  }
  }

  switch (node->lhs->ty->kind) {
  case TY_FLOAT:
  case TY_DOUBLE: {
    gen_expr(node->rhs);
    push_tmpf();
    gen_expr(node->lhs);
    pop_tmpf(1);

    char *sz = (node->lhs->ty->kind == TY_FLOAT) ? "ss" : "sd";

    switch (node->kind) {
    case ND_ADD:
      println("  add%s %%xmm1, %%xmm0", sz);
      return;
    case ND_SUB:
      println("  sub%s %%xmm1, %%xmm0", sz);
      return;
    case ND_MUL:
      println("  mul%s %%xmm1, %%xmm0", sz);
      return;
    case ND_DIV:
      println("  div%s %%xmm1, %%xmm0", sz);
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      println("  ucomi%s %%xmm0, %%xmm1", sz);

      if (node->kind == ND_EQ) {
        println("  sete %%al");
        println("  setnp %%dl");
        println("  and %%dl, %%al");
      } else if (node->kind == ND_NE) {
        println("  setne %%al");
        println("  setp %%dl");
        println("  or %%dl, %%al");
      } else if (node->kind == ND_LT) {
        println("  seta %%al");
      } else {
        println("  setae %%al");
      }

      println("  and $1, %%al");
      println("  movzb %%al, %%rax");
      return;
    }

    error_tok(node->tok, "invalid expression");
  }
  case TY_LDOUBLE: {
    gen_expr(node->lhs);
    gen_expr(node->rhs);

    switch (node->kind) {
    case ND_ADD:
      println("  faddp");
      return;
    case ND_SUB:
      println("  fsubrp");
      return;
    case ND_MUL:
      println("  fmulp");
      return;
    case ND_DIV:
      println("  fdivrp");
      return;
    case ND_EQ:
    case ND_NE:
    case ND_LT:
    case ND_LE:
      println("  fcomip");
      println("  fstp %%st(0)");

      if (node->kind == ND_EQ)
        println("  sete %%al");
      else if (node->kind == ND_NE)
        println("  setne %%al");
      else if (node->kind == ND_LT)
        println("  seta %%al");
      else
        println("  setae %%al");

      println("  movzb %%al, %%rax");
      return;
    }

    error_tok(node->tok, "invalid expression");
  }
  }

  gen_expr(node->rhs);
  push_tmp();
  gen_expr(node->lhs);
  pop_tmp("%rdi");

  char *ax, *di, *dx;

  if (node->lhs->ty->size == 8 || node->lhs->ty->base) {
    ax = "%rax";
    di = "%rdi";
    dx = "%rdx";
  } else {
    ax = "%eax";
    di = "%edi";
    dx = "%edx";
  }

  switch (node->kind) {
  case ND_ADD:
    println("  add %s, %s", di, ax);
    return;
  case ND_SUB:
    println("  sub %s, %s", di, ax);
    return;
  case ND_MUL:
    println("  imul %s, %s", di, ax);
    return;
  case ND_DIV:
  case ND_MOD:
    if (node->ty->is_unsigned) {
      println("  mov $0, %s", dx);
      println("  div %s", di);
    } else {
      if (node->lhs->ty->size == 8)
        println("  cqo");
      else
        println("  cdq");
      println("  idiv %s", di);
    }

    if (node->kind == ND_MOD)
      println("  mov %%rdx, %%rax");
    return;
  case ND_BITAND:
    println("  and %s, %s", di, ax);
    return;
  case ND_BITOR:
    println("  or %s, %s", di, ax);
    return;
  case ND_BITXOR:
    println("  xor %s, %s", di, ax);
    return;
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    println("  cmp %s, %s", di, ax);

    if (node->kind == ND_EQ) {
      println("  sete %%al");
    } else if (node->kind == ND_NE) {
      println("  setne %%al");
    } else if (node->kind == ND_LT) {
      if (node->lhs->ty->is_unsigned)
        println("  setb %%al");
      else
        println("  setl %%al");
    } else if (node->kind == ND_LE) {
      if (node->lhs->ty->is_unsigned)
        println("  setbe %%al");
      else
        println("  setle %%al");
    }

    println("  movzb %%al, %%rax");
    return;
  case ND_SHL:
    println("  mov %%rdi, %%rcx");
    println("  shl %%cl, %s", ax);
    return;
  case ND_SHR:
    println("  mov %%rdi, %%rcx");
    if (node->lhs->ty->is_unsigned)
      println("  shr %%cl, %s", ax);
    else
      println("  sar %%cl, %s", ax);
    return;
  }

  error_tok(node->tok, "invalid expression");
}

static void gen_stmt(Node *node) {
  if (opt_g)
    print_loc(node->tok);

  switch (node->kind) {
  case ND_IF: {
    int c = count();
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    println("  je  .L.else.%d", c);
    gen_stmt(node->then);
    println("  jmp .L.end.%d", c);
    println(".L.else.%d:", c);
    if (node->els)
      gen_stmt(node->els);
    println(".L.end.%d:", c);
    return;
  }
  case ND_FOR: {
    int c = count();
    if (node->init)
      gen_stmt(node->init);
    println(".L.begin.%d:", c);
    if (node->cond) {
      gen_expr(node->cond);
      cmp_zero(node->cond->ty);
      println("  je %s", node->brk_label);
    }
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    if (node->inc)
      gen_expr(node->inc);
    println("  jmp .L.begin.%d", c);
    println("%s:", node->brk_label);
    dealloc_vla(node);
    return;
  }
  case ND_DO: {
    int c = count();
    println(".L.begin.%d:", c);
    gen_stmt(node->then);
    println("%s:", node->cont_label);
    gen_expr(node->cond);
    cmp_zero(node->cond->ty);
    println("  jne .L.begin.%d", c);
    println("%s:", node->brk_label);
    return;
  }
  case ND_SWITCH:
    gen_expr(node->cond);

    for (Node *n = node->case_next; n; n = n->case_next) {
      char *ax, *di, *dx;

      if (node->cond->ty->size == 8) {
        ax = "%rax";
        di = "%rdi";
        dx = "%rdx";
      } else {
        ax = "%eax";
        di = "%edi";
        dx = "%edx";
      }

      if (n->begin == n->end) {
        println("  mov $%ld, %s", n->begin, dx);
        println("  cmp %s, %s", dx, ax);
        println("  je %s", n->label);
        continue;
      }

      // [GNU] Case ranges
      println("  mov %s, %s", ax, di);
      println("  mov $%ld, %s", n->begin, dx);
      println("  sub %s, %s", dx, di);
      println("  mov $%ld, %s", n->end - n->begin, dx);
      println("  cmp %s, %s", dx, di);
      println("  jbe %s", n->label);
    }

    if (node->default_case)
      println("  jmp %s", node->default_case->label);

    println("  jmp %s", node->brk_label);
    gen_stmt(node->then);
    println("%s:", node->brk_label);
    return;
  case ND_CASE:
    println("%s:", node->label);
    if (node->lhs)
      gen_stmt(node->lhs);
    return;
  case ND_BLOCK:
    for (Node *n = node->body; n; n = n->next)
      gen_stmt(n);
    dealloc_vla(node);
    return;
  case ND_GOTO:
    dealloc_vla(node);
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
  case ND_RETURN:
    if (node->lhs) {
      gen_expr(node->lhs);
      Type *ty = node->lhs->ty;

      switch (ty->kind) {
      case TY_STRUCT:
      case TY_UNION:
        if (ty->size <= 16)
          copy_struct_reg();
        else
          copy_struct_mem();
        break;
      }
    }

    println("  jmp .L.return.%s", current_fn->name);
    return;
  case ND_EXPR_STMT:
    gen_expr(node->lhs);
    return;
  case ND_ASM:
    println("  %s", node->asm_str);
    return;
  }

  error_tok(node->tok, "invalid statement");
}

static int get_lvar_align(Scope *scp, int align) {
  for (Obj *var = scp->locals; var; var = var->next) {
      if (var->ofs)
      continue;
    align = MAX(align, var->align);
  }

  for (Scope *sub = scp->children; sub; sub = sub->sibling_next) {
    int sub_max = get_lvar_align(sub, align);
    align = MAX(align, sub_max);
  }
  return align;
}

static int assign_lvar_offsets2(Scope *sc, int bottom, char *ptr) {
  for (Obj *var = sc->locals; var; var = var->next) {
    if (var->ofs)
      continue;

    // AMD64 System V ABI has a special alignment rule for an array of
    // length at least 16 bytes. We need to align such array to at least
    // 16-byte boundaries. See p.14 of
    // https://github.com/hjl-tools/x86-psABI/wiki/x86-64-psABI-draft.pdf.
    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    bottom += var->ty->size;
    bottom = align_to(bottom, align);
    var->ofs = -bottom;
    var->ptr = ptr;
  }

  int max_depth = bottom;
  for (Scope *sub = sc->children; sub; sub = sub->sibling_next) {
    int sub_depth= assign_lvar_offsets2(sub, bottom, ptr);
    if (!dont_reuse_stack)
      max_depth = MAX(max_depth, sub_depth);
    else
      bottom = max_depth = sub_depth;
  }
  return max_depth;
}

// Assign offsets to local variables.
static void assign_lvar_offsets(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    if (fn->large_rtn) {
      fn->large_rtn->param_next = fn->ty->param_list;
      fn->ty->param_list = fn->large_rtn;
    }

    // If a function has many parameters, some parameters are
    // inevitably passed by stack rather than by register.
    // The first passed-by-stack parameter resides at RBP+16.
    int top = 16;

    calling_convention(fn->ty->param_list, 0, NULL, NULL, NULL);

    // Assign offsets to pass-by-stack parameters.
    for (Obj *var = fn->ty->param_list; var; var = var->param_next) {
      if (!var->pass_by_stack)
        continue;

      var->ofs = var->stack_offset + top;
      var->ptr = "%rbp";
    }

    fn->stack_align = get_lvar_align(fn->ty->scopes, 16);

    char *lvar_ptr = (fn->stack_align > 16) ? "%rbx" : "%rbp";
    fn->lvar_stack_size = assign_lvar_offsets2(fn->ty->scopes, 0, lvar_ptr);
  }
}

static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function || !var->is_definition)
      continue;

    if (var->is_static)
      println("  .local %s", var->name);
    else
      println("  .globl %s", var->name);

    int align = (var->ty->kind == TY_ARRAY && var->ty->size >= 16)
      ? MAX(16, var->align) : var->align;

    // Common symbol
    if (opt_fcommon && var->is_tentative) {
      println("  .comm %s, %d, %d", var->name, var->ty->size, align);
      continue;
    }

    // .data or .tdata
    if (var->init_data) {
      if (var->is_tls && opt_data_sections)
        println("  .section .tdata.%s,\"awT\",@progbits", var->name);
      else if (var->is_tls)
        println("  .section .tdata,\"awT\",@progbits");
      else if (opt_data_sections)
        println("  .section .data.%s,\"aw\",@progbits", var->name);
      else
        println("  .data");

      println("  .type %s, @object", var->name);
      println("  .size %s, %d", var->name, var->ty->size);
      println("  .align %d", align);
      println("%s:", var->name);

      Relocation *rel = var->rel;
      int pos = 0;
      while (pos < var->ty->size) {
        if (rel && rel->offset == pos) {
          println("  .quad %s%+ld", *rel->label, rel->addend);
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
      println("  .section .tbss.%s,\"awT\",@nobits", var->name);
    else if (var->is_tls)
      println("  .section .tbss,\"awT\",@nobits");
    else if (opt_data_sections)
      println("  .section .bss.%s,\"aw\",@nobits", var->name);
    else
      println("  .bss");

    println("  .align %d", align);
    println("%s:", var->name);
    println("  .zero %d", var->ty->size);
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
  unreachable();
}

static void store_gp(int r, int sz, int ofs, char *ptr) {
  switch (sz) {
  case 1:
    println("  mov %s, %d(%s)", argreg8[r], ofs, ptr);
    return;
  case 2:
    println("  mov %s, %d(%s)", argreg16[r], ofs, ptr);
    return;
  case 4:
    println("  mov %s, %d(%s)", argreg32[r], ofs, ptr);
    return;
  case 8:
    println("  mov %s, %d(%s)", argreg64[r], ofs, ptr);
    return;
  default:
    for (int i = 0; i < sz; i++) {
      println("  mov %s, %d(%s)", argreg8[r], ofs + i, ptr);
      println("  shr $8, %s", argreg64[r]);
    }
    return;
  }
}

static void emit_text(Obj *prog) {
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function || !fn->is_definition)
      continue;

    // No code is emitted for "static inline" functions
    // if no one is referencing them.
    if (!fn->is_live)
      continue;

    if (fn->is_static)
      println("  .local %s", fn->name);
    else
      println("  .globl %s", fn->name);

    if (opt_func_sections)
      println("  .section .text.%s,\"ax\",@progbits", fn->name);
    else
      println("  .text");
    println("  .type %s, @function", fn->name);
    println("%s:", fn->name);
    current_fn = fn;
    tmp_stack.bottom = fn->lvar_stack_size;

    bool use_rbx = fn->stack_align > 16;
    lvar_ptr = use_rbx ? "%rbx" : "%rbp";

    // Prologue
    println("  push %%rbp");
    println("  mov %%rsp, %%rbp");
    if (use_rbx) {
      println("  push %%rbx");
      println("  mov %%rsp, %%rbx");
      println("  and $-%d, %%rbx", fn->stack_align);
      println("  mov %%rbx, %%rsp");
    }

    long reserved_pos = ftell(output_file);
    println("                           ");
    if (fn->vla_base)
      println("  mov %%rsp, %d(%s)", fn->vla_base->ofs, fn->vla_base->ptr);

    // Save arg registers if function is variadic
    if (fn->va_area) {
      int gp, fp;
      int stack = calling_convention(fn->ty->param_list, 0, &gp, &fp, NULL);

      int off = fn->va_area->ofs;
      char *ptr = lvar_ptr;

      // __reg_save_area__
      println("  movq %%rdi, %d(%s)", off + 24, ptr);
      println("  movq %%rsi, %d(%s)", off + 32, ptr);
      println("  movq %%rdx, %d(%s)", off + 40, ptr);
      println("  movq %%rcx, %d(%s)", off + 48, ptr);
      println("  movq %%r8, %d(%s)", off + 56, ptr);
      println("  movq %%r9, %d(%s)", off + 64, ptr);
      println("  test %%al, %%al");
      println("  je 1f");
      println("  movsd %%xmm0, %d(%s)", off + 72, ptr);
      println("  movsd %%xmm1, %d(%s)", off + 88, ptr);
      println("  movsd %%xmm2, %d(%s)", off + 104, ptr);
      println("  movsd %%xmm3, %d(%s)", off + 120, ptr);
      println("  movsd %%xmm4, %d(%s)", off + 136, ptr);
      println("  movsd %%xmm5, %d(%s)", off + 152, ptr);
      println("  movsd %%xmm6, %d(%s)", off + 168, ptr);
      println("  movsd %%xmm7, %d(%s)", off + 184, ptr);
      println("1:");

      // va_elem
      println("  movl $%d, %d(%s)", gp * 8, off, ptr);           // gp_offset
      println("  movl $%d, %d(%s)", fp * 16 + 48, off + 4, ptr); // fp_offset
      println("  lea %d(%%rbp), %%rax", stack + 16);             // overflow_arg_area
      println("  mov %%rax, %d(%s)", off + 8, ptr);
      println("  lea %d(%s), %%rax", off + 24, ptr);             // reg_save_area
      println("  mov %%rax, %d(%s)", off + 16, ptr);
    }

    // Save passed-by-register arguments to the stack
    int gp = 0, fp = 0;
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
      case TY_LDOUBLE:
        unreachable();
        break;
      default:
        store_gp(gp++, ty->size, var->ofs, var->ptr);
      }
    }

    // Emit code
    gen_stmt(fn->body);
    assert(tmp_stack.depth == 0);

    long cur_pos = ftell(output_file);

    fseek(output_file, reserved_pos, SEEK_SET);
    println("  sub $%d, %%rsp", align_to(tmp_stack.bottom, 16));

    fseek(output_file, cur_pos, SEEK_SET);

    // [https://www.sigbus.info/n1570#5.1.2.2.3p1] The C spec defines
    // a special rule for the main function. Reaching the end of the
    // main function is equivalent to returning 0, even though the
    // behavior is undefined for the other functions.
    if (strcmp(fn->name, "main") == 0)
      println("  mov $0, %%rax");

    // Epilogue
    println(".L.return.%s:", fn->name);
    if (use_rbx)
      println("  mov -8(%%rbp), %%rbx");
    println("  mov %%rbp, %%rsp");
    println("  pop %%rbp");
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
  assign_lvar_offsets(prog);
  emit_data(prog);
  emit_text(prog);
  println("  .section  .note.GNU-stack,\"\",@progbits");
}

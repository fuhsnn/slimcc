#include "test.h"

void mem_operand_triival() {
  int i;
  __asm__ ("movl $11, %0" : "=m"(i));
  ASSERT(11, i);

  struct {
    char c,d,e;
  } s;
  __asm__ ("movb $22, %0" : "=m"(s.d));
  ASSERT(22, s.d);

  short arr[10];
  __asm__ ("movw $33, %0" : "=m"(arr[7]));
  ASSERT(33, arr[7]);

  static struct {
    char c;
    struct {
      long long l[11];
    } s[11];
  } s2 [11];

  __asm__ ("movq $44, %0" : "=m"(s2[3].s[7].l[5]));

  ASSERT(44, s2[3].s[7].l[5]);
}

void verify_123(int i, int j, int k) {
  ASSERT(11, i);
  ASSERT(22, j);
  ASSERT(33, k);
}
void reg_label() {
  register int
  i __asm__("di"),
  j __asm__("si"),
  k __asm__("dx");
  __asm__ ("movl $11, %0;"
           "movl $22, %1;"
           "movl $33, %2;"
           "call verify_123"
           : "=r"(i), "=r"(j) : "r"(k));
}

void i_constraint(int var) {
  static int v = 22;
  static struct { char c; long long i; } s = {.i = 33};
  __asm__ volatile (
    "movl %[lvar], %%edi;"
    "addl %[imm], %%edi;"
#if !defined(__PIC__)
    "movabsq %[ptr], %%rsi; movl (%%rsi), %%esi;"
    "movabsq %[agg], %%rdx; movl (%%rdx), %%edx;"
    "movabsq %[fun], %%rax; callq *%%rax;"
#else
    "movl $22, %%esi;"
    "movl $33, %%edx;"
    "call verify_123"
#endif
    :
    : [imm]"i"(6), [lvar]"ir"(var)
#if !defined(__PIC__)
      , [ptr]"i"(&v), [agg]"i"(&s.i), [fun]"i"(verify_123)
#endif
    : "rsi", "rdi", "rdx", "rax"
  );
}

void reg_assign(void) {
  int a,b,c,d,e,f,g;
  __asm__ volatile(
    "movl %%edx, %3;"
    "movl %%ebp, %6;"
    :"=a"(a), "=b"(b), "=c"(c), "=&r"(d), "=S"(e), "=D"(f), "=&r"(g)
    :"0"(0), "1"(0), "2"(0), "Q"(44), "4"(0), "5"(0), "R"(55)
  );
  ASSERT(44, d);
  ASSERT(55, g);
}

long long *indir_flag(void) { static long long var; return &var; }
int float_flag(float f) {
  float register fz __asm__("xmm3") = 0.0f;
  static int e;
  short np;
  char p;
  __asm__ (
    "xorps %%xmm0, %%xmm0;"
    "ucomiss %[f_in], %%xmm3"
    : "=@ccp"(p), "=@ccnp"(np), "=@cce"(e), "=@ccne"(*indir_flag())
    : [f_in]"x"(f), "x"(fz)
    : "xmm0", "cc"
  );
  return p * 1000 + np * 100 + e * 10 + *indir_flag();
}

int asm_goto(int arg){
  __asm__ goto (
    "subl $5, %0; je %l2;"
    "subl $2, %0; jl %l1;"
    :
    : "r"(arg)
    :
    : labA, labB
  );
  return 11;
  labA:;
  return 22;
  labB:;
  return 33;
}

int asm_goto_output(int *arg){
  __asm__ goto (
    "subl $5, %0; je %l[labB];"
    "subl $2, %0; jl %l[labA];"
    "movq $0, %%rbp;"
    "movl $3, %0;"
    : "+r"(*arg)
    :
    : "rbp"
    : labA, labB
  );
  return *arg + 10;
  labA:;
  return *arg + 20;
  labB:;
  return *arg + 30;
}

int asm_goto_local_label(void) {
#define LE_OR_QUIT(_v) do { __label__ LE;                    \
    __asm goto ("cmpl $10, %[val]; jle %l[LE]; jmp %l[QUIT]" \
      ::[val]"r"(_v) : "cc" : LE, QUIT);                     \
    LE:                                                      \
  } while (0)

  int cnt = 0;
  LE_OR_QUIT(9);
  cnt += 1;
  LE_OR_QUIT(10);
  cnt += 10;
  LE_OR_QUIT(11);
  cnt += 100;
QUIT:
  return cnt;
}

void x87_clobber(void) {
  volatile long double f1;
  volatile double f2;
  volatile float f3;

  __asm__ volatile (""::
    "f"((float){11}),
    "t"((double){22}),
    "u"((long double){33}) : "%st", "st(1)"
    );
  __asm__ volatile ("": "=t"(f1));
  __asm__ volatile ("": "=t"(f2));
  ASSERT(1, 33 == f1);
  ASSERT(1, 11 == f2);

  __asm__ volatile (""::
    "u"((float){44}),
    "f"((double){55}),
    "t"((long double){66}) : "st"
    );
  __asm__ volatile("": "=t"(f3));
  ASSERT(1, 55 == f3);

  __asm__ volatile (""::
    "f"((float){77}),
    "u"((double){88}),
    "t"((long double){99}) : "st", "st(1)", "st(2)"
    );
  __asm__ volatile ("": "=t"(f1));
  __asm__ volatile ("": "=t"(f2));
  ASSERT(1, 88 == f1);
  ASSERT(1, 77 == f2);
}

int ptr_conversion(char *str) {
    int res;
    __asm__ volatile (
      "  leaq %3, %%rsi;"
      "  call *%2"
      : "=a"(res)
      : "D"("bar"), "r"(strcmp), "m"(*str)
      : "rsi"
    );
    return !res;
}

struct Large {
    char arr[33];
    int i;
};

int exhaust_inner2(struct Large s) {
  int i[] = {1,2,3,4,5,6,7,8,9,10};

  _Alignas(1024) int res;
  ASSERT(0, 1023 & (intptr_t)&res);

  int register init __asm__("%bh") = 0;
  __asm__ volatile(
    "shl $4, %1;"
    "add %1, %%ebx;"
    "shl $3, %%eax;"
    "shl $3, %%ecx;"
    "shl $3, %%edx;"
    "add %%eax, %%ebx;"
    "add %%ecx, %%ebx;"
    "add %%edx, %%ebx;"
    "shl $2, %%esi;"
    "shl $2, %%edi;"
    "shl $2, %%ebp;"
    "add %%edi, %%ebx;"
    "add %%esi, %%ebx;"
    "add %%ebp, %%ebx;"
    "shl $1, %%r8d;"
    "shl $1, %%r9d;"
    "shl $1, %%r10d;"
    "shl $1, %%r11d;"
    "add %%r8d, %%ebx;"
    "add %%r9d, %%ebx;"
    "add %%r10d, %%ebx;"
    "add %%r11d, %%ebx;"
    : "=b"(res),"+r"(s.i),
      "+U"(i[6]),"+U"(i[7]),"+U"(i[8]),"+U"(i[9]), // r8,r9,r10,r11
      "+R"(i[0]),"+R"(i[1]),"+R"(i[2]), // si,di,bp
      "+Q"(i[3]),"+Q"(i[4]),"+Q"(i[5]) // ax,cx,dx
    : "r"(init)
  );

  ASSERT(68, i[6] + i[7] + i[8] + i[9]);
  ASSERT(120, i[3] + i[4] + i[5]);
  ASSERT(24, i[0] + i[1] + i[2]);
  ASSERT(272, s.i);
  return res;
}

int exhaust_inner(void) {
  struct Large s = {.i = 17};
  return exhaust_inner2(s);
}

void exhaust(void) {
  void *(arr)[12];
  int res;
  __asm__ volatile(
    "movq %%r12, %[arr];"
    "movq %%r13, 8+0%[arr];"
    "movq %%r14, 16+0%[arr];"
    "movq %%r15, 24+0%[arr];"
    "movq %%rbp, 32+0%[arr];"
    "movq %%rbx, 40+0%[arr];"
    "call exhaust_inner;"
    "movq %%r12, 48+0%[arr];"
    "movq %%r13, 56+0%[arr];"
    "movq %%r14, 64+0%[arr];"
    "movq %%r15, 72+0%[arr];"
    "movq %%rbp, 80+0%[arr];"
    "movq %%rbx, 88+0%[arr];"
    :"=a"(res), [arr]"=m"(arr)
  );
  ASSERT(484, res);
  ASSERT(0, memcmp(&arr[0], &arr[6], 6 * 8));
}

void bitfield_out() {
  struct S {
   int i:4, j:3, k:5, z:20;
  };
  struct S s;
  s.z = -1;
  struct S *p = &s;
  __asm__ (
    "mov $13, %b2;"
    "mov $6, %b0;"
    "mov $-2, %b1;"
    :"=r"(p->i), "=r"(p->j), "=r"(p->k)
  );
  ASSERT(6, s.i);
  ASSERT(-2, s.j);
  ASSERT(13, s.k);
  ASSERT(-1, s.z);
}

void x87(void) {
  long double f1 = 11, f2 = 22;
  long double *p2 = &f2;
  __asm__ (
    "fxch %1;"
    "fmul %2, %0;"
    :"+t"(f1), "+u"(*p2)
    :"f"(3.0L)
  );
  ASSERT(1, 66 == f1);
  ASSERT(1, 11 == f2);
}

int main(void) {
  mem_operand_triival();
  reg_label();
  i_constraint(5);
  reg_assign();
  x87();
  x87_clobber();
  exhaust();
  bitfield_out();

  ASSERT(0, ptr_conversion("foo"));
  ASSERT(1, ptr_conversion("bar"));

  ASSERT(101, float_flag(1.0f));
  ASSERT(110, float_flag(0.0f));
  ASSERT(1010, float_flag(0.0f/0.0f));

  ASSERT(11, asm_goto(7));
  ASSERT(22, asm_goto(6));
  ASSERT(33, asm_goto(5));

  ASSERT(13, asm_goto_output(&(int){7}));
  ASSERT(19, asm_goto_output(&(int){6}));
  ASSERT(30, asm_goto_output(&(int){5}));

  ASSERT(11, asm_goto_local_label());

  printf("OK\n");
}

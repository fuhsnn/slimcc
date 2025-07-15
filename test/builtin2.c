#include "test.h"

#ifndef __has_builtin
#error
#endif

#if !__has_builtin(__builtin_offsetof)
#error
#endif

struct ofs_S1 {
  char c;
  struct {
  struct {
    struct {
    struct {
      char k[7];
    } n;
    char z;
    };
  } m[7][7];
  };
};
SASSERT(__builtin_offsetof(struct ofs_S1, m[1][2].n.k[3]) == 76);

int va_expr_in_arg(int i, ...) {
  __builtin_va_list ap, ap2;
  __builtin_va_start(ap, (i += 7, i));
  __builtin_va_copy(ap2, ap);
  ASSERT(1, !memcmp(ap,ap2,sizeof(__builtin_va_list)));
  __builtin_c23_va_start(ap);
  __builtin_c23_va_start(ap, i);
  __builtin_va_end((i += 13, ap));
  return i;
}

int va_in_comma(int i, ...) {
  __builtin_va_list ap, ap2;
  return __builtin_va_start(ap, i),
  __builtin_va_copy(ap2, ap),
  i = __builtin_va_arg(ap2, int),
  __builtin_va_end(ap2),
  i;
}

static int garr[2];
static void va_fn1(void) { garr[0] = 111; }
static void va_fn2(void) { garr[1] = 222; }
static void va_fn0(int cnt, ...) {
    va_list ap;
    va_start(ap, cnt);
    for(int i = 0; i < cnt; i++)
        va_arg(ap, void(*)(void))();
    va_end(ap);
}

SASSERT(__builtin_constant_p(1 ? 0 : 0));
int g;
SASSERT(!__builtin_constant_p(g));

static __auto_type builtin_constant_p_test = __builtin_constant_p(0);
SASSERT(_Generic(&builtin_constant_p_test, int *:1));

static __auto_type builtin_expect_test = __builtin_expect(1.0f,1.0);
SASSERT(_Generic(&builtin_expect_test, long *:1));

int main(void) {
  ASSERT(30, va_expr_in_arg(17));
  ASSERT(33, va_in_comma(0,33));

  va_fn0(2, &va_fn1, &va_fn2);
  ASSERT(111, garr[0]);
  ASSERT(222, garr[1]);

#define runtime_ofs(x,y,z) __builtin_offsetof(struct ofs_S1, m[x][y].n.k[z])
  ASSERT(324, ({int x = 5, y = 3; runtime_ofs(x,5,y); }));
  ASSERT(100, ({int x = 1, y = 5, z = 3; runtime_ofs(x,y,z); }));
  ASSERT(222, ({int y = 6; runtime_ofs(3,y,5); }));

  {
    int i;
    SASSERT(0 == __builtin_types_compatible_p(int[4][i], int[2][i]));
    SASSERT(0 == __builtin_types_compatible_p(int[i][4], int[i][2]));
  }

  printf("OK\n");
}

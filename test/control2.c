#include "test.h"
#include <stdint.h>

char c23_label(void) {

  {
    int i = 0;
    if (i != 0)
lab3:
      i = 1;
    ASSERT(0, i);
  }

  switch (1) {
    default:
    case 2:
  }
  if (0) {
    {
      lab1:
    }
    {
      lab2:
      int j;
    }
    return 77;
  }
  goto lab1;
}

int label_in_secondary_block(int i) {
  if (i == 2)
    goto labl;
  if (i == 3)
    labl:
      return 1;
  return 0;
}

int local_labels(void) {
  __label__ d;

  int cnt = 0;

  int a_looped = 0;
  int b_looped = 0;
  int d_looped = 0;
  int f_looped = 0;
  int g_looped = 0;

  { __label__ a; goto a; a: }
  a:;
  { __label__ a; goto a; a: }

  { __label__ b; goto b; b: }
  { b: }
  { __label__ b; goto b; b: }

  if (!a_looped++) {
    cnt++;
    goto a;
  }
  if (!b_looped++) {
    cnt++;
    goto b;
  }

  {
    __label__ c;
    {
      goto c;
      { __label__ c; { goto c; } c: }
      cnt -= 10;
      c:
      cnt++;
    }
  }
  c:

  int p_loop = 0;

  goto d;
  {
    __label__ f;
#ifdef NOTCLANG
    __label__ decl_only;
#endif

    cnt -= 10;
    { d: }
    e:
    f:
    g:

    cnt++;

    if (!d_looped++)
      goto d;

    if (!f_looped++)
      goto f;

    if (!g_looped++)
      goto g;

    void *p;

    switch (p_loop++) {
      case 0:
        p = &&d;
        break;
      case 1:
        p = &&f;
        break;
      case 2:
        p = &&g;
        break;
      default:
        p = &&end;
    }
    goto *p;
    cnt++;
  }
  end:

  return cnt;
}

int main(void){
  ASSERT(2, ({ uint32_t i=0; switch(i){case 0 ...0xFFFFFFFF: i=2;} i; }));
  ASSERT(2, ({ int32_t i=0; switch(i){case 0x80000000 ...0x7FFFFFFF: i=2;} i; }));
  ASSERT(2, ({ uint64_t i=0; switch(i){case 0 ...0xFFFFFFFFFFFFFFFF: i=2;} i; }));
  ASSERT(2, ({ int64_t i=0; switch(i){case 0x8000000000000000 ...0x7FFFFFFFFFFFFFFF: i=2;} i; }));

  ASSERT(2, ({ uint32_t i=0; switch(i){case 0 ...0x100000000: i=2;} i; }));
  ASSERT(2, ({ int32_t i=-1; switch(i){case -1 ...(int64_t)-1: i=2;} i; }));

  ASSERT(2, ({ int64_t i=0; switch(0x123456789){case 0x123456789: i=2;} i; }));

  ASSERT(0, label_in_secondary_block(1));
  ASSERT(1, label_in_secondary_block(2));
  ASSERT(1, label_in_secondary_block(3));
  ASSERT(77, c23_label());
  ASSERT(10, local_labels());

  printf("OK\n");
}

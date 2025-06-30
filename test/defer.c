#include "test.h"

// Adapted from https://www.open-std.org/JTC1/SC22/WG14/www/docs/n3199.htm

int n3199_ex2(void) {
  int r = 4;
  int* p = &r;
  defer { *p = 5; }
  return *p;
}

int n3199_ex5(void) {
  int r = 0;
  {
    defer {
      defer r *= 4;
      r *= 2;
      defer {
        r += 3;
      }
    }
    defer r += 1;
  }
  return r;
}

void n3199_ex4(void) {
  char buf[64] = "c";
  {
    defer {
      snprintf(buf, 64, "%s meow", strdup(buf));
    }
    if (1)
      defer snprintf(buf, 64, "%sat", strdup(buf));
    snprintf(buf, 64, "%s says", strdup(buf));
  }
  ASSERT(0, strcmp(buf, "cat says meow"));
}

int block_with_loop_switch() {
  int cnt = 0;
  for (int i = 0; i <= 25; i++) {
    defer {
      F1:
      for (int j = 0; j < 10; j++) {
        S1:
        switch (j) {
        case 3:
          break;
        case 2:
          continue F1;
        case 5:
          break S1;
        default:
          continue;
        }
        no_op:
        if (i % j == 0) {
          cnt++;
          break F1;
        }
      }
    }
    if (i % 3 == 0)
      continue;
    if (i == 20)
      break;
  }
  return cnt;
}

int block_with_local_labels(void) {
  int arr[4] = {0};
  int *cnt = &arr[0];
  int inc = 9;

  for (int i = 0; i < 4; i++) {
    defer {
      __label__ a,b,d,g,end;
      int a_looped = 0;
      int b_looped = 0;
      int d_looped = 0;
      int f_looped = 0;
      int g_looped = 0;

      a:;
      { __label__ a; goto a; a: }

      { __label__ b; goto b; b: }
      { b: }

      if (!a_looped++) {
        *cnt += inc;;
        goto a;
      }
      if (!b_looped++) {
        *cnt += inc;;
        goto b;
      }

      {
        __label__ c;
        {
          goto c;
          { __label__ c; { goto c; } c: }
          *cnt -= 10;
          c:
          *cnt += inc;;
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

        *cnt -= 10;
        { d: }
        e:
        f:
        g:

        *cnt += inc;;

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
        *cnt++;
      }
      end:
    }
    cnt = &arr[1];
    inc = 1;
    if (i == 1)
      continue;

    cnt = &arr[2];
    inc = 2;
    if (i == 2)
      break;

    cnt = &arr[3];
    inc = 3;
  }

  ASSERT(10, arr[1]);
  ASSERT(20, arr[2]);
  ASSERT(30, arr[3]);
  return arr[0];
}

int main () {
  ASSERT(4, n3199_ex2());
  n3199_ex4();
  ASSERT(20, n3199_ex5());

  ASSERT(10, block_with_loop_switch());

  ASSERT(0, block_with_local_labels());

  printf("OK\n");
  return 0;
}

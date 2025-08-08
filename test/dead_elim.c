#include "test.h"

extern void not_to_be_called();

void exit_w_0[[noreturn]]() {
  exit(0);
}

int test_unreach(int v) {
  void *p;
  for (int i = 0;;) {
    if (i++ == 0) {
      continue;
      not_to_be_called();
    }
    p = &&end;
    break;
    not_to_be_called();
  }

  F1:
  for (; v > 0;) {
    switch(v) {
    case 1:
      if (1)
        goto end;
      not_to_be_called();
    case 2:
      if (1) {
        break F1;
        not_to_be_called();
      }
    case 3:
      if (0)
        not_to_be_called();
      v = -1;
      continue F1;
      not_to_be_called();
    case 4:
      if (1)
        goto end;
      else
        not_to_be_called();
      not_to_be_called();
    case 5:
      if (0)
        not_to_be_called();
      else {
        v = -1;
        break;
      }
      not_to_be_called();
    case 6:
    case 7:
      if (v < 7) {
        v += 1;
        goto *p;
        not_to_be_called();
      } else {
        v += 2;
        {
          goto *p;
          not_to_be_called();
        }
        not_to_be_called();
      }
      not_to_be_called();
    }
  }

  end:
  return v;
  not_to_be_called();
}

int test_reach(int v) {
  switch (v) {
    unreachable();
    not_to_be_called();
  case 1:
    return v;
    not_to_be_called();

  case -2:
    if (0) {
      not_to_be_called();
  case 2:
      return v;
    }

  case -3:
    if (1)
      return 999;
  not_to_be_called();
  case 3:
    return v;

  not_to_be_called();
  case -4:
    if (1)
      return 999;
    else
  case 4:
      return v;
  not_to_be_called();

    do {
  case 5:
      return v;
    } while (0);
  }
  return 0;
}

int main(void) {
  ASSERT(1, test_unreach(1));
  ASSERT(2, test_unreach(2));
  ASSERT(-1, test_unreach(3));
  ASSERT(4, test_unreach(4));
  ASSERT(-1, test_unreach(5));
  ASSERT(7, test_unreach(6));
  ASSERT(9, test_unreach(7));

  ASSERT(1, test_reach(1));
  ASSERT(2, test_reach(2));
  ASSERT(3, test_reach(3));
  ASSERT(4, test_reach(4));
  ASSERT(5, test_reach(5));
  ASSERT(999, test_reach(-2));
  ASSERT(999, test_reach(-3));
  ASSERT(999, test_reach(-4));

  printf("OK\n");
  exit_w_0();
  not_to_be_called();
}

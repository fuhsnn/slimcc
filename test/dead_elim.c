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

__attribute__((noreturn))
void exit_w_1() {
  exit(1);
}

#define A __attribute__((noreturn))
#define B [[gnu::noreturn]]

A void (*fnp1_g)(void) = &exit_w_1;
void (*A fnp2_g)(void) = &exit_w_1;
void (*fnp3_g)(void) A = &exit_w_1;

B void (*fnp4_g)(void) = &exit_w_1;
void (*fnp5_g B)(void) = &exit_w_1;

int attr_noreturn(void) {
  volatile bool f = false;

  if (f) { fnp1_g(); not_to_be_called(); }
  if (f) { fnp2_g(); not_to_be_called(); }
  if (f) { fnp3_g(); not_to_be_called(); }
  if (f) { fnp4_g(); not_to_be_called(); }
  if (f) { fnp5_g(); not_to_be_called(); }

  if (f) { A void (*p)(void) = &exit_w_1; p(); not_to_be_called(); }
  if (f) { void (*A p)(void) = &exit_w_1; p(); not_to_be_called(); }
  if (f) { void (*p)(void) A = &exit_w_1; p(); not_to_be_called(); }
  if (f) { B void (*p)(void) = &exit_w_1; p(); not_to_be_called(); }
  if (f) { void (*p B)(void) = &exit_w_1; p(); not_to_be_called(); }

  if (f) { A static void (*p)(void) = &exit_w_1; p(); not_to_be_called(); }
  if (f) { static void (*A p)(void) = &exit_w_1; p(); not_to_be_called(); }
  if (f) { static void (*p)(void) A = &exit_w_1; p(); not_to_be_called(); }
  if (f) { B static void (*p)(void) = &exit_w_1; p(); not_to_be_called(); }
  if (f) { static void (*p B)(void) = &exit_w_1; p(); not_to_be_called(); }

  if (f) { struct { A void (*p)(void); } s = { &exit_w_1 }; s.p(); not_to_be_called(); }
  if (f) { struct { void (*A p)(void); } s = { &exit_w_1 }; s.p(); not_to_be_called(); }
  if (f) { struct { void (*p)(void) A; } s = { &exit_w_1 }; s.p(); not_to_be_called(); }
  if (f) { struct { B void (*p)(void); } s = { &exit_w_1 }; s.p(); not_to_be_called(); }
  if (f) { struct { void (*p B)(void); } s = { &exit_w_1 }; s.p(); not_to_be_called(); }

  return 1;
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

  ASSERT(1, attr_noreturn());

  printf("OK\n");
  exit_w_0();
  not_to_be_called();
}

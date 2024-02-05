#include "test.h"

// adapted from David Mazières's "Recursive macros with C++20 __VA_OPT__"
// https://www.scs.stanford.edu/~dm/blog/va-opt.html

int E(int x) {
  return x * 17;
}

void rescan(void) {

#define E(x) x
  int X = 3;

  ASSERT(51, E(E)(X) ); // E(X)

#define T() E(X)
#define PRNS ()
  ASSERT(3, E(T()) ); // X
  ASSERT(51, E(T PRNS) ); // E(X)

#define LPRN (
#define E2(arg) arg
  int x_0 = E(E2)(E)(X) ; // X
  int x_1 = E(E2 LPRN)E)(X) ; // X
  int x_2 = E(E2 LPRN E))(X) ; // E(X)
  int x_3 = E(E2 (E))(X) ; // E(X)
  ASSERT(3, x_0 );
  ASSERT(3, x_1 );
  ASSERT(51, x_2 );
  ASSERT(51, x_3 );

#define F2() F
#define F() __COUNTER__ F2 PRNS()
#define H(x) #x
#define STR(x) H(x)
  ASSERT(0, strcmp("0 F2 ()()",  STR(F()) ));
  ASSERT(0, strcmp("1 2 F2 ()()", STR(E(F())) ));
  ASSERT(0, strcmp("3 4 5 F2 ()()", STR(E(E(F()))) ));
}

int arr[4];

void write_arr(int i) {
  static int idx = 0;
  if (idx >= 4)
    exit(1);

  arr[idx++] = i;
}

void foreach(void) {

#define EXPAND(...) EXPAND2(EXPAND2(__VA_ARGS__))
#define EXPAND2(...) EXPAND1(EXPAND1(__VA_ARGS__))
#define EXPAND1(...) __VA_ARGS__

#define FOR_EACH(macro, ...)                                    \
  __VA_OPT__(EXPAND(FOR_EACH_HELPER(macro, __VA_ARGS__)))
#define FOR_EACH_HELPER(macro, a1, ...)                         \
  macro(a1);                                                    \
  __VA_OPT__(FOR_EACH_AGAIN PRNS (macro, __VA_ARGS__))
#define FOR_EACH_AGAIN() FOR_EACH_HELPER

FOR_EACH(write_arr, 11, 22, 33, 44)

  ASSERT(11, arr[0] );
  ASSERT(22, arr[1] );
  ASSERT(33, arr[2] );
  ASSERT(44, arr[3] );
}

int main(void) {
  rescan();
  foreach();

  printf("OK\n");
}

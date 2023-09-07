#include "test.h"

// adpated from https://www.scs.stanford.edu/~dm/blog/va-opt.html

int E(int x) {
  return x * 17;
}

int main(void) {

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

  printf("OK\n");
}

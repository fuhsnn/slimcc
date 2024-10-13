#include "test.h"

int main(void) {
  enum E1;
  typedef enum E1 const C2;
  typedef C2 *P;
  P p;

  enum E1 {
    E1A,
    E1B = 5000000000,
  };

  DASSERT(sizeof(*p) == 8);
  DASSERT(5000000000 == E1B);

  enum E2 : short;
  enum E2 : short { E2A };
  DASSERT(sizeof(E2A) == 2);

  enum : short unsigned { ANON_E };
  DASSERT(sizeof(ANON_E) == 2);

  printf("OK\n");
}

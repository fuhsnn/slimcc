#include "test.h"

int fn(int i) {
F:
  for (;; i += 1) {
  S1:
  S2:
    switch (i)
    F1:
    case 11:
    F2:
      for (;; i += 7) {
        switch (i)
        case 10:
          continue F;
        break S2;
      case 3:
        continue F1;
      default:
        break F;
      case 0:
        break S1;
      }
    i += 2;
  }
  return i;
}

int main(void) {
  ASSERT(14, fn(0));
  printf("OK\n");
}

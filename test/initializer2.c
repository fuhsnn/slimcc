#include "test.h"

int main(void) {
  {
    int i[2][4] = {[0][0 ... 1] = 4, 2, 3, 7, [1][1 ... 2] = 1, 9};
    int ans[] = {4,4,2,3,7,1,1,9};
    ASSERT(0, memcmp(&i, &ans, sizeof(ans)));
  }

  printf("OK\n");
}


#include "test.h"


int main(void) {
  DASSERT(u8'a' == 'a');
  SASSERT(_Generic(u8'b', unsigned char:1));

  printf("OK\n");
}

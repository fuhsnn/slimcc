#include "test.h"

int neg(int i) {
  return -i;
}

typeof(int(int)) g_proto __asm("neg") __attribute__((visibility("default")));

typeof(int(int)) *g_fp = &g_proto;

extern typeof(int(int)) *ext_fp __asm("g_fp");

int main(void) {
  extern typeof(int(int)) l_proto __asm("neg");

  ASSERT(-11, g_proto(11));
  ASSERT(-22, l_proto(22));
  ASSERT(-33, g_fp(33));
  ASSERT(-44, ext_fp(44));

  printf("OK\n");
}

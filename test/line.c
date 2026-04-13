#include "test.h"

int main() {
#line 500 "foo"
  ASSERT(500, __LINE__);
  ASSERT(0, strcmp(__FILE__, "foo"));

#line 800 "bar"
  ASSERT(800, __LINE__);
  ASSERT(0, strcmp(__FILE__, "bar"));

#line 1
  ASSERT(1, __LINE__);

# 200 "xyz" 2 3
  ASSERT(200, __LINE__);
  ASSERT(0, strcmp(__FILE__, "xyz"));

  printf("OK\n");
  return 0;
}

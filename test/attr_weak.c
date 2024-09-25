#include "test.h"
int no_def(void) __attribute__ ((weak));
int has_def(void) __attribute__ ((weak));

int main(int argc, const char **argv)
{
  int res = 0;
  int no_def_test;
  if (no_def) {
    no_def_test = 1;
    res = no_def();
  } else {
    no_def_test = 0;
  }
  int has_def_test;
  if (has_def) {
    has_def_test = 1;
    res = has_def();
  } else {
    has_def_test = 0;
  }
  ASSERT(0, no_def_test);
  ASSERT(1, has_def_test);
  ASSERT(123, res);

  printf("OK\n");
}

int has_def(void){
  return 123;
}

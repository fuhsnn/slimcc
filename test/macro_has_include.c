#ifndef __has_include
#error
#endif

#if !__has_include("test.h")
#error
#endif

#if !__has_include(<stdio.h>)
#error
#endif

#define A B
#define B C(test.h)
#define C(x) STR(x)
#define STR(x) #x

#if !__has_include(A)
#error
#endif

#include "test.h"

int main(void) {
  printf("OK\n");
}

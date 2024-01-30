#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>

#define ASSERT(x, y) test_assert(x, y, #y)
#define DASSERT(x) static_assert(x); ASSERT(1, x)

extern
#if defined __cplusplus
"C"
#endif
void test_assert(int expected, int actual, char *code);

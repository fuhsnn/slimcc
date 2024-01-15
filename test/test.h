#define ASSERT(x, y) assert(x, y, #y)

#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
extern void assert(int expected, int actual, char *code);

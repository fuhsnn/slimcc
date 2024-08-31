#include "test.h"

// Adapted from https://www.open-std.org/JTC1/SC22/WG14/www/docs/n3307.htm

int main(void) {
#define MAKE_SEMI(X, ...) X, __VA_TAIL__()
  char arr1[] = {MAKE_SEMI('A')};
  char arr2[] = {MAKE_SEMI('A', 'B')};
  char arr3[] = {MAKE_SEMI('A', 'B', 'C')};
  ASSERT(0, strncmp(arr1, "A", 1));
  ASSERT(0, strncmp(arr2, "AB", 2));
  ASSERT(0, strncmp(arr3, "ABC", 3));

#define MAKE_SEMI2(X, ...) X, __VA_TAIL__(MAKE_SEMI2)
  char arr4[] = {MAKE_SEMI2('D')};
  char arr5[] = {MAKE_SEMI2('D', 'E')};
  char arr6[] = {MAKE_SEMI2('D', 'E', 'F')};
  ASSERT(0, strncmp(arr4, "D", 1));
  ASSERT(0, strncmp(arr5, "DE", 2));
  ASSERT(0, strncmp(arr6, "DEF", 3));

#define AddUp(X, ...) (X __VA_OPT__(+__VA_TAIL__()))
  ASSERT(11, AddUp(11));
  ASSERT(33, AddUp(11, 22));
  ASSERT(66, AddUp(11, 22, 33));

#define REVERT(X, ...) __VA_TAIL__()__VA_OPT__(,) X
  char arr7[] = {REVERT('G')};
  char arr8[] = {REVERT('G', 'H')};
  char arr9[] = {REVERT('G', 'H', 'I')};
  ASSERT(0, strncmp(arr7, "G", 1));
  ASSERT(0, strncmp(arr8, "HG", 2));
  ASSERT(0, strncmp(arr9, "IHG", 3));

#define LEFT(X, ...) X __VA_OPT__(,__VA_TAIL__(RIGHT))
#define RIGHT(X, ...) __VA_OPT__(__VA_TAIL__(LEFT),) X
  char arr10[] = {RIGHT('J', 'K', 'L', 'M', 'N', 'O')};
  ASSERT(0, strncmp(arr10, "KMONLJ", 6));

#define A 35*
#define B(Y) 3*(Y)
#define C 7
#define tailer(X, ...) (__VA_OPT__(+)X __VA_TAIL__())
  ASSERT(735, tailer(A, B, C));

  printf("OK\n");
}

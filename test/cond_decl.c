#include "test.h"

static int buf[128];
static int idx;
static void inc(void *p) { buf[idx++] = *(int *)p; }
#define CLN [[gnu::cleanup(inc)]]

static int if_cond(int i) {
  if (CLN int a = i + 1, b = a + 2; CLN int c = b - 10)
    return a + c;
  else if (int c CLN = -i, d CLN = c + 3; c += 7)
    ;
  else if (int d CLN = c)
    ;
  else
    return a + b + (c -= 11) + (d += 5);
  return 99;
}

static int sw_cond(int i, int j) {
  switch (int z = i * j) {
  case 0:
    return 99;
  case 24:
    return 24;
  }
  switch (char(*p)[i][j]; sizeof(*p)) {
  case 0:
    return 99;
  case 21:
    return 21;
  }
  switch (char arr[i][j]; int j = sizeof(arr)) {
  case 0:
    return 99;
  case 25:
    return 25;
  }
  return 99;
}

static void w_cond(int i) {
  while (CLN int a = i--) {
    CLN int b = -i;
  }
  int z = 9;
  inc(&z);
}
static void f_cond(int i) {
  int z = 9;
  for (int a CLN = i--; CLN int c = i--; inc(&z)) {
    CLN int d = i--;
  }
  inc(&z);
}

int main(void) {
  {
    idx = 0;
    ASSERT(-4, if_cond(1));
    int ans[] = {-6, 4, 2};
    ASSERT(0, memcmp(&buf, &ans, sizeof(ans)));
  }
  {
    idx = 0;
    ASSERT(12, if_cond(7));
    int ans[] = {5, -4, -11, 0, 10, 8};
    ASSERT(0, memcmp(&buf, &ans, sizeof(ans)));
  }

  ASSERT(24, sw_cond(4, 6));
  ASSERT(21, sw_cond(7, 3));
  ASSERT(25, sw_cond(5, 5));
  {
    idx = 0;
    w_cond(2);
    int ans[] = {-1, 2, 0, 1, 0, 9};
    ASSERT(0, memcmp(&buf, &ans, sizeof(ans)));
  }
  {
    idx = 0;
    f_cond(5);
    int ans[] = {3, 9, 4, 1, 9, 2, 0, 5, 9};
    ASSERT(0, memcmp(&buf, &ans, sizeof(ans)));
  }

  printf("OK\n");
}

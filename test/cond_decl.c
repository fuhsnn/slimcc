#include "test.h"

static int storage_extern() {
  if (extern int ext1; ext1 == 5)
    ext1 = 13;

  extern int ext1;
  ASSERT(13, ext1);
  return 1;
}

static int storage_static() {
  switch (static int i = 0) {
  case 0:
    i = 2;
    storage_static();
    return i;
  case 2:
    i = 1;
  }
  return 0;
}

static int vla_zinit(int num) {
  if (int arr[num] = {})
    return arr[0] == 0;
  return 0;
}

static int vla_typedef(int num) {
  if (typedef int Arr[num]; !(Arr *){0})
    return sizeof(Arr) == sizeof(int) * num;

  return 0;
}

int main(void) {
  ASSERT(1, storage_extern());
  ASSERT(1, storage_static());
  ASSERT(1, vla_zinit(11));
  ASSERT(1, vla_typedef(13));

  printf("OK\n");
}

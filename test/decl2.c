#include "test.h"

long double ld = 0.0L;

#define CAT(x) b##x
int CAT(6_) = 7;
int CAT(8µ) = 9;

int arr[2][2] = {{1,2},{3,4}};
int *p1 = arr[1];

int fn(float), var1 = 7, fn2(void), var2 = 11;

int tentative_var;
int tentative_var;

short incomplete_tentative_arr[];

extern int init_extern = 3;

int main(void) {
  {
    ASSERT(7, b6_);
    ASSERT(9, b8µ);
  }
  {
    ASSERT(3, *p1);
  }
  {
    ASSERT(7, var1);
    ASSERT(11, var2);
    ASSERT(13, fn(13.0f));
    ASSERT(19, fn2());
  }
  {
    char arr2[3] = {11, 22, 33};
    ASSERT(33, (&arr2)[0][2]);
    ASSERT(22, (*&arr2)[1]);
    ASSERT(11, (**&arr2));
  }

  ASSERT(0, tentative_var);

  ASSERT(0, incomplete_tentative_arr[0]);

  ASSERT(3, init_extern);

  printf("OK\n");
}


int fn(float f) { return f; }
int fn2(void) { return 19; }

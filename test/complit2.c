#include "test.h"

static int arr[4] = {1,2,3,4};

int sum(int a, int b){
  return a + b;
}
int* getarr(int idx) {
  return &arr[idx];
}

struct s {
  int i,j,k;
} s0 = {1,2,3};

int main() {
  ASSERT(1, ({ (int[2]){1,2}[0]; }));
  ASSERT(2, ({ (int[2]){1,2}[1]; }));

  ASSERT(1, ({ (int*){&arr}[0]; }));
  ASSERT(3, ({ (int*){&arr[1]}[1]; }));

  ASSERT(4, ({ (int(*)[2]){&arr}[1][1]; }));

  ASSERT(0, ({ (struct s){.j=2}.i; }));
  ASSERT(2, ({ (struct s){.j=2}.j; }));
  ASSERT(0, ({ (struct s){.j=2}.k; }));

  ASSERT(3, ({ (struct s*){&s0}->k; }));

  ASSERT(3, ({ int a=(int){3}++; a; }));

  ASSERT(1, ({ int *p=(int*){&arr[0]}++; *p; }));

  ASSERT(10, ({ (int(*)(int,int)){sum}(7,3); }));

  ASSERT(3, ({ (int*(*)(int)){&getarr}(1)[1]; }));

  printf("OK\n");
  return 0;
}

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

int static_local() {
  struct S {
    int i;
    int *p;
  };
  static struct S s = {(int){}, (int*){&arr[1]}};
  return (s.p++)[s.i++];
}

int static_complit_counter(void) {
  int *p = &(static int){};
  return (*p)++;
}

int static_complit_counter2(void) {
  static int *p = &(static int){3};
  return (*p)++;
}

void constexpr_complit_adr(int recur, const int *local_p, const int *static_p) {
  if (recur >= 3)
    return;

  const int *local_vp = &(constexpr int){(int){55}};
  const int *static_vp = &(static constexpr int){(int){66}};

  ASSERT(55, *local_vp);
  ASSERT(66, *static_vp);

  if (recur) {
    ASSERT(55, *local_p);
    ASSERT(66, *static_p);

    ASSERT(1, local_vp != local_p );
    ASSERT(1, static_vp == static_p );
  }
  constexpr_complit_adr(recur + 1, local_vp, static_vp);
}

int main() {
  ASSERT(1, ({ (int[2]){1,2}[0]; }));
  ASSERT(2, ({ (int[2]){1,2}[1]; }));

  ASSERT(1, ({ (int*){(void*)&arr}[0]; }));
  ASSERT(3, ({ (int*){&arr[1]}[1]; }));

  ASSERT(4, ({ (int(*)[2]){(void*)&arr}[1][1]; }));

  ASSERT(0, ({ (struct s){.j=2}.i; }));
  ASSERT(2, ({ (struct s){.j=2}.j; }));
  ASSERT(0, ({ (struct s){.j=2}.k; }));

  ASSERT(3, ({ (struct s*){&s0}->k; }));

  ASSERT(3, ({ int a=(int){3}++; a; }));

  ASSERT(1, ({ int *p=(int*){&arr[0]}++; *p; }));

  ASSERT(10, ({ (int(*)(int,int)){sum}(7,3); }));

  ASSERT(3, ({ (int*(*)(int)){&getarr}(1)[1]; }));

  ASSERT(2, static_local());
  ASSERT(4, static_local());

  ASSERT(0, static_complit_counter());
  ASSERT(3, static_complit_counter2());

  ASSERT(1, static_complit_counter());
  ASSERT(4, static_complit_counter2());

  constexpr_complit_adr(0, NULL, NULL);

  printf("OK\n");
  return 0;
}

#include "test.h"

constexpr int i = 7;
constexpr int arr[i] = { 1,2,3,4,5,6,7 };

int main(void){
  constexpr int j = arr[(i - 1)];
  DASSERT( arr[(i - 6)*6] == j);

  constexpr int arr2[] = { 5,6,7,8,9 };
  DASSERT( sizeof(arr2) == 5 * sizeof(int) );
  DASSERT( arr[arr2[arr[0]]] == 7);

  DASSERT( (0, arr)[5] == (i == j ? arr2 : arr)[1] );

  constexpr int arr3[3][2] = {1,2,3,4,5,6};
  DASSERT( (arr3[0] + 1)[0] == 2);
  DASSERT( (arr3[0] + 0)[1] == 2);
  DASSERT( (arr3 + 2)[-1][0] == 3);
  DASSERT( (arr3[1] + 2)[-1] == 4);

  struct S1 { int i; };
  constexpr struct S1 sa[4] = { 1,2,3,4 };
  DASSERT( sa[2].i == 3);

  struct S2 { int i[16]; };
  constexpr struct S2 sa2 = { 1,2,3,4 };
  DASSERT( sa2.i[1] == 2);

  printf("OK\n");
}

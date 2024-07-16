#include "test.h"
#include <stdint.h>
typedef int32_t i32;
typedef uint8_t u8;

void fn1(i32 z, i32 a[z], i32 b1[sizeof(a)],  i32 (*b2)[sizeof(a)]){
  ASSERT(8, sizeof(a));
  ASSERT(8, sizeof(b1));
  ASSERT(32, sizeof(*b2));
}

void fn2(i32 z, i32 (*a)[z], i32 b1[sizeof(*a)],  i32 (*b2)[sizeof(*a)]){
  ASSERT(28, sizeof(*a));
  ASSERT(8, sizeof(b1));
  ASSERT(112, sizeof(*b2));
}

void fn3(i32 x, i32 y, u8 arr[++y][(x+=2,x)]){
  ASSERT(9, x);
  ASSERT(12, y);
  ASSERT(8, sizeof arr);
  ASSERT(9, sizeof *arr);
}

void fn4(i32 x,i32,i32 y,i32,i32,i32 z,i32 a[x+y][y+z][z+x]) {
  ASSERT(8, sizeof(a));
  ASSERT(1008, sizeof(a[0]));
  ASSERT(56, sizeof(a[0][0]));
}

int max(i32 a, i32 b) {
  return a > b ? a : b;
}
int fn5(i32 x, i32 y, i32 (*z) [max(x, y)]) {
  return sizeof *z;
}

int g;
void fn6(i32 x, i32 (*a)[g += x], i32 y, i32(*b)[sizeof(*a) + y], i32 (*c)[sizeof(*b) + g], i32 as, i32 bs, i32 cs){
  ASSERT(as, sizeof(*a));
  ASSERT(bs, sizeof(*b));
  ASSERT(cs, sizeof(*c));
}

void fn7(i32 *x, i32(*a)[*(++x)], i32(*b)[*(++x)] ) {
  ASSERT(88, sizeof(*a));
  ASSERT(132, sizeof(*b));
}

void fn8(i32 x, i32 (*a)[sizeof(u8[x])], i32 (*b)[sizeof(*a)+sizeof(u8[sizeof(*a)+sizeof(u8[x])])]) {
  ASSERT(468, sizeof(*b));
  ASSERT(52, sizeof(*a));
}

void fn9(char* str, int b, i32 (*arr)[b == strcmp(str, "FOOBAR")]) {
  ASSERT(4, sizeof *arr);
}

_Bool z = 1;

void
fn_oldstyle(a, b, c, d, z)
float c;
uint8_t b,d;
int16_t (*a)[++b][(__typeof__(b))c][d++];
{
  ASSERT(720, sizeof(*a));
  ASSERT(144, sizeof((*a)[0]));
  ASSERT(16, sizeof((*a)[1][2]));
  ASSERT(2, sizeof((*a)[1][2][3]));

  z = c;
  ASSERT(5, b);
  ASSERT(9, d);
  ASSERT(777, z);
}

int main(void){
  fn1(7,0,0,0);
  fn2(7,0,0,0);
  fn3(7,11,0);
  fn4(3,0,7,0,0,11,0);
  ASSERT(28, fn5(3, 7, 0));
  ASSERT(44, fn5(11, 5, 0));
  g = 13;
  fn6(11,0,7,0,0,96,412,1744);
  g = 3;
  fn6(11,0,7,0,0,56,252,1064);
  i32 arr[3] = {11,22,33};
  fn7(&arr, 0, 0);
  fn8(13, 0, 0);
  fn9("FOOBAR", 0, 0);

  double f = 777.0;
  fn_oldstyle(0,260,f,264);

  printf("OK\n");
}

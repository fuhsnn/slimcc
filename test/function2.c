#include "test.h"

typedef struct {
  char g;
} G;

typedef struct {
  float f;
} F;

typedef struct {
  char l;
  float d;
} A;

extern float struct_test101(
  G g0, G g1, G g2, G g3, G g4, G g5,
  F f0, F f1, F f2, F f3, F f4, F f5, F f6, F f7,
  G gs0, F fs0, G gs1, F fs1
);
float struct_test100(
  G g0, G g1, G g2, G g3, G g4, G g5,
  F f0, F f1, F f2, F f3, F f4, F f5, F f6, F f7,
  G gs0, F fs0, G gs1, F fs1
) {
  return g0.g + g1.g + g2.g + g3.g + g4.g + g5.g +
  f0.f + f1.f + f2.f + f3.f + f4.f + f5.f + f6.f + f7.f +
  gs0.g + gs1.g + fs0.f + fs1.f;
}

extern float struct_test111(
  F f0, F f1, F f2, F f3, F f4, F f5, F f6, F f7,
  G g0, G g1, G g2, G g3, G g4, G g5,
  G gs0, F fs0, G gs1, F fs1
);
float struct_test110(
  F f0, F f1, F f2, F f3, F f4, F f5, F f6, F f7,
  G g0, G g1, G g2, G g3, G g4, G g5,
  G gs0, F fs0, G gs1, F fs1
) {
  return
  g0.g + g1.g + g2.g + g3.g + g4.g + g5.g +
  f0.f + f1.f + f2.f + f3.f + f4.f + f5.f + f6.f + f7.f +
  gs0.g + gs1.g + fs0.f + fs1.f;
}

extern float struct_test121(
  A reg0, A reg1, A reg2, A reg3, A reg4, A reg5,
  A s0,
  F f6, F f7,
  A s1);

float struct_test120(
  A reg0, A reg1, A reg2, A reg3, A reg4, A reg5,
  A s0,
  F f6, F f7,
  A s1)
{
  return
  reg0.l + reg0.d + reg1.l + reg1.d + reg2.l + reg2.d +
  reg3.l + reg3.d + reg4.l + reg4.d + reg5.l + reg5.d +
  s0.l + s0.d + s1.l + s1.d + f6.f + f7.f;
}


int main(void) {
  G g[] = {10,11,12,13,14,15};
  F f[] = {20,21,22,23,24,25,26,27};
  G gs[]  = {30,31};
  F fs[]  = {40,41};

  ASSERT(405, struct_test100(
g[0], g[1], g[2], g[3], g[4], g[5],
f[0], f[1], f[2], f[3], f[4], f[5], f[6], f[7],
gs[0], fs[0], gs[1], fs[1]));

  ASSERT(405, struct_test101(
g[0], g[1], g[2], g[3], g[4], g[5],
f[0], f[1], f[2], f[3], f[4], f[5], f[6], f[7],
gs[0], fs[0], gs[1], fs[1]));

  ASSERT(405, struct_test110(
f[0], f[1], f[2], f[3], f[4], f[5], f[6], f[7],
g[0], g[1], g[2], g[3], g[4], g[5],
gs[0], fs[0], gs[1], fs[1]));

  ASSERT(405, struct_test111(
f[0], f[1], f[2], f[3], f[4], f[5], f[6], f[7],
g[0], g[1], g[2], g[3], g[4], g[5],
gs[0], fs[0], gs[1], fs[1]));

  A reg[] = {10,11,20,21,30,31,40,41,50,51,60,61};
  A as[] = {70,71,80,81};

  ASSERT(781, struct_test120(
reg[0], reg[1], reg[2], reg[3], reg[4], reg[5],
as[0],
f[6], f[7],
as[1]));

  ASSERT(781, struct_test121(
reg[0], reg[1], reg[2], reg[3], reg[4], reg[5],
as[0],
f[6], f[7],
as[1]));

  printf("OK\n");

}


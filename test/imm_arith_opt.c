#include "test.h"

#define TESTOPT2(Ty,x,Op,imm) \
  ASSERT(1,((Ty)(x)Op(Ty)(imm)) == ({Ty a=x; a Op (Ty)(imm);}));\

#define TESTOPT(Ty,x,Op,imm) \
  TESTOPT2(Ty,x,Op,imm) \
  TESTOPT2(u##Ty,x,Op,imm)

#define FILL32 0xF1234567
#define FILL64 0xABCDEF0123456789

int main(void) {
  TESTOPT(int32_t, FILL32, +, 0);
  TESTOPT(int64_t, FILL64, +, 0);
  TESTOPT(int32_t, FILL32, +, 1);
  TESTOPT(int64_t, FILL64, +, 1);
  TESTOPT(int32_t, FILL32, +, -1);
  TESTOPT(int64_t, FILL64, +, -1);

  TESTOPT(int32_t, FILL32, -, 0);
  TESTOPT(int64_t, FILL64, -, 0);
  TESTOPT(int32_t, FILL32, -, 1);
  TESTOPT(int64_t, FILL64, -, 1);
  TESTOPT(int32_t, FILL32, -, -1);
  TESTOPT(int64_t, FILL64, -, -1);

  TESTOPT(int32_t, FILL32, &, 0);
  TESTOPT(int64_t, FILL64, &, 0);
  TESTOPT(int32_t, FILL32, &, -1);
  TESTOPT(int64_t, FILL64, &, -1);

  TESTOPT(int32_t, FILL32, |, 0);
  TESTOPT(int64_t, FILL64, |, 0);
  TESTOPT(int32_t, FILL32, |, -1);
  TESTOPT(int64_t, FILL64, |, -1);

  TESTOPT(int32_t, FILL32, ^, 0);
  TESTOPT(int64_t, FILL64, ^, 0);
  TESTOPT(int32_t, FILL32, ^, -1);
  TESTOPT(int64_t, FILL64, ^, -1);

  TESTOPT(int32_t, FILL32, *, 0);
  TESTOPT(int64_t, FILL64, *, 0);
  TESTOPT(int32_t, FILL32, *, 1);
  TESTOPT(int64_t, FILL64, *, 1);
  TESTOPT(int32_t, FILL32, *, -1);
  TESTOPT(int64_t, FILL64, *, -1);

  TESTOPT(int32_t, FILL32, /, 1);
  TESTOPT(int64_t, FILL64, /, 1);
  TESTOPT(int32_t, FILL32, /, -1);
  TESTOPT(int64_t, FILL64, /, -1);
  TESTOPT(int32_t, -1, /, -1);
  TESTOPT(int64_t, -1, /, -1);

  TESTOPT(int32_t, FILL32, %, 1);
  TESTOPT(int64_t, FILL64, %, 1);
  TESTOPT(int32_t, FILL32, %, -1);
  TESTOPT(int64_t, FILL64, %, -1);
  TESTOPT(int32_t, -1, %, -1);
  TESTOPT(int64_t, -1, %, -1);

  TESTOPT(int32_t, FILL32, *, 1024);
  TESTOPT(int64_t, FILL64, *, 1024);

  TESTOPT2(uint32_t, FILL32, /, 1 << 5);
  TESTOPT2(uint64_t, FILL64, /, 1LL << 35);

  TESTOPT2(uint32_t, FILL32, %, 1U << 5);
  TESTOPT2(uint32_t, FILL32, %, 1U << 31);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 31);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 32);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 33);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 63);

  printf("OK\n");
}

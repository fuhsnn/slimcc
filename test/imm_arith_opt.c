#include "test.h"

#define TESTOPT2(Ty,x,Op,imm) \
  ASSERT(1,((Ty)(x)Op(Ty)(imm)) == ({Ty a=x; a Op (Ty)(imm);}));\
  ASSERT(1,((Ty)(x)Op(Ty)(imm)) == ({Ty b=imm; (Ty)(x) Op b;}));

#define TESTOPT(Ty,x,Op,imm) \
  TESTOPT2(Ty,x,Op,imm) \
  TESTOPT2(u##Ty,x,Op,imm)

#define FILL32 0xF1234567
#define FILL64 0xABCDEF0123456789
#define FILL32i 0x789ABCDE
#define FILL64i 0x789ABCDEF0123456

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

  TESTOPT2(uint32_t, FILL32, %, 1U << 1);
  TESTOPT2(uint32_t, FILL32, %, 1U << 2);
  TESTOPT2(uint32_t, FILL32, %, 1U << 3);
  TESTOPT2(uint32_t, FILL32, %, 1U << 29);
  TESTOPT2(uint32_t, FILL32, %, 1U << 30);
  TESTOPT2(uint32_t, FILL32, %, 1U << 31);

  TESTOPT2(int32_t, FILL32, %, 1U << 1);
  TESTOPT2(int32_t, FILL32, %, 1U << 2);
  TESTOPT2(int32_t, FILL32, %, 1U << 3);
  TESTOPT2(int32_t, FILL32, %, 1U << 29);
  TESTOPT2(int32_t, FILL32, %, 1U << 30);
  TESTOPT2(int32_t, FILL32, %, 1U << 31);

  TESTOPT2(int32_t, FILL32i, %, 1U << 1);
  TESTOPT2(int32_t, FILL32i, %, 1U << 2);
  TESTOPT2(int32_t, FILL32i, %, 1U << 3);
  TESTOPT2(int32_t, FILL32i, %, 1U << 29);
  TESTOPT2(int32_t, FILL32i, %, 1U << 30);
  TESTOPT2(int32_t, FILL32i, %, 1U << 31);

  TESTOPT2(uint32_t, FILL32, /, 1U << 1);
  TESTOPT2(uint32_t, FILL32, /, 1U << 2);
  TESTOPT2(uint32_t, FILL32, /, 1U << 3);
  TESTOPT2(uint32_t, FILL32, /, 1U << 29);
  TESTOPT2(uint32_t, FILL32, /, 1U << 30);
  TESTOPT2(uint32_t, FILL32, /, 1U << 31);

  TESTOPT2(int32_t, FILL32, /, 1U << 1);
  TESTOPT2(int32_t, FILL32, /, 1U << 2);
  TESTOPT2(int32_t, FILL32, /, 1U << 3);
  TESTOPT2(int32_t, FILL32, /, 1U << 29);
  TESTOPT2(int32_t, FILL32, /, 1U << 30);
  TESTOPT2(int32_t, FILL32, /, 1U << 31);

  TESTOPT2(int32_t, FILL32i, /, 1U << 1);
  TESTOPT2(int32_t, FILL32i, /, 1U << 2);
  TESTOPT2(int32_t, FILL32i, /, 1U << 3);
  TESTOPT2(int32_t, FILL32i, /, 1U << 29);
  TESTOPT2(int32_t, FILL32i, /, 1U << 30);
  TESTOPT2(int32_t, FILL32i, /, 1U << 31);

  TESTOPT2(uint64_t, FILL64, %, 1ULL << 1);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 2);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 3);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 29);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 30);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 31);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 32);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 33);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 34);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 61);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 62);
  TESTOPT2(uint64_t, FILL64, %, 1ULL << 63);

  TESTOPT2(int64_t, FILL64, %, 1ULL << 1);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 2);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 3);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 29);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 30);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 31);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 32);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 33);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 34);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 61);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 62);
  TESTOPT2(int64_t, FILL64, %, 1ULL << 63);

  TESTOPT2(int64_t, FILL64i, %, 1ULL << 1);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 2);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 3);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 29);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 30);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 31);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 32);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 33);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 34);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 61);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 62);
  TESTOPT2(int64_t, FILL64i, %, 1ULL << 63);

  TESTOPT2(uint64_t, FILL64, /, 1ULL << 1);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 2);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 3);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 29);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 30);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 31);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 32);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 33);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 34);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 61);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 62);
  TESTOPT2(uint64_t, FILL64, /, 1ULL << 63);

  TESTOPT2(int64_t, FILL64, /, 1ULL << 1);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 2);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 3);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 29);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 30);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 31);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 32);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 33);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 34);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 61);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 62);
  TESTOPT2(int64_t, FILL64, /, 1ULL << 63);

  TESTOPT2(int64_t, FILL64i, /, 1ULL << 1);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 2);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 3);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 29);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 30);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 31);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 32);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 33);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 34);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 61);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 62);
  TESTOPT2(int64_t, FILL64i, /, 1ULL << 63);

  printf("OK\n");
}

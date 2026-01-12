#include "test.h"

#if !defined(__has_attribute) || !defined(__has_c_attribute)
#error
#endif

#if !__has_attribute(packed)
#error
#endif

#if __has_c_attribute(packed)
#error
#endif

#define CAT(x,y) x##y

int has_attr(void) {
  DASSERT(__has_attribute(packed) == 1);
  DASSERT( CAT(__has,_attribute)(packed) == 1);
  DASSERT(__has_c_attribute(gnu::packed) == 1);
  DASSERT(__has_c_attribute(gnu::__packed__) == 1);
  DASSERT(__has_c_attribute(__gnu__::packed) == 1);
  DASSERT(__has_c_attribute(clang::packed) == 0);
}


[[_Noreturn]]
int fallthrough(int i) {
  switch (i){
    case 3:
    [[fallthrough]];
    default:
  }
  exit(0);
}

void packed(void) {
  EASSERT(0, offsetof(struct [[gnu::packed]] { char a; int b; }, a));
  EASSERT(1, offsetof(struct [[gnu::__packed__]] { char a; int b; }, b));
  ASSERT(5, ({ struct [[gnu::packed]] { char a; int b; } x; sizeof(x); }));
  ASSERT(9, ({ typedef struct [[gnu::packed]] { char a; int b[2]; } T; sizeof(T); }));
  EASSERT(1, offsetof(struct [[gnu::packed]] T { char a; int b[2]; }, b));

  // no-op
#ifdef NOTCLANG
  ASSERT(8, ({ struct { char a; int b; } [[gnu::packed]] x; sizeof(x); }));
  EASSERT(0, offsetof(struct { char a; int b; } [[gnu::packed]], a));
  EASSERT(4, offsetof(struct { char a; int b; } [[gnu::packed]], b));
#endif
  ASSERT(12, ({ typedef struct [[packed]] { char a; int b[2]; } T; sizeof(T); }));
  EASSERT(4, offsetof(struct [[packed]] { char a; int b[2]; }, b));

  ASSERT(9, ({ struct {  struct { char m2; long long m5;  } __attribute__((packed)); } T; sizeof(T); }));
  ASSERT(2, ({ typedef struct { struct { int :9, m16; } __attribute__((packed)); } T; offsetof(T, m16); }));
  ASSERT(12, ({ struct __attribute__((packed)) { long long :48, :46; } a; sizeof(a); }));

  ASSERT(1, ({ typedef struct { char c; long long __attribute__((,,packed,,)) i; } S; offsetof(S, i); }));
  ASSERT(9, ({ typedef struct { char c; long long i [[,,gnu::packed,,gnu::packed,,]]; } S; sizeof(S); }));

  ASSERT(6, ({ typedef struct { short s; int i [[gnu::packed]] : 24; char c; } S; sizeof(S); }));
  ASSERT(6, ({ typedef struct { char c; int i : 24 __attribute__((packed)); short s; } S; sizeof(S); }));

  EASSERT(1, offsetof(struct { char a; __attribute__((packed)) int(*b)(int); }, b));
#ifdef NOTGCC
  EASSERT(1, offsetof(struct { char a; int(*__attribute__((packed)) b)(int); }, b));
#endif
  EASSERT(1, offsetof(struct { char a; int(*b)(int) __attribute__((packed)); }, b));

  EASSERT(1, offsetof(struct { char a; [[gnu::packed]] int(*b)(int); }, b));
  EASSERT(1, offsetof(struct { char a; int(*b [[gnu::packed]])(int); }, b));
}

int main() {
  has_attr();
  packed();

  printf("OK\n");
  return 0;
}

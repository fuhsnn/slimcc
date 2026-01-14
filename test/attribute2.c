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
#define A __attribute__((packed))
#define B [[gnu::packed]]

#ifdef NOTCLANG
  ASSERT(8, ({ typedef struct A s S; struct s { char c; int i; }; sizeof(S); })); // clang: 5
#endif
  ASSERT(5, ({ typedef struct s S; struct A s { char c; int i; }; sizeof(S); }));
  ASSERT(5, ({ typedef struct s S; struct s { char c; int i; } A; sizeof(S); }));

  ASSERT(5, ({ typedef struct s S; struct B s { char c; int i; }; sizeof(S); }));

  ASSERT(9, ({ typedef struct { char c; long long A i; } S; sizeof(S); }));
  ASSERT(1, ({ typedef struct { char c; long long A i; } S; offsetof(S, i); }));
  ASSERT(9, ({ typedef struct { char c; long long i B; } S; sizeof(S); }));
  ASSERT(1, ({ typedef struct { char c; long long i B; } S; offsetof(S, i); }));

  ASSERT(6, ({ typedef struct { short s; int i B : 24; char c; } S; sizeof(S); }));
  ASSERT(6, ({ typedef struct { char c; int i : 24 A; short s; } S; sizeof(S); }));

  ASSERT(9, ({ typedef struct B { char a; int b[2]; } T; sizeof(T); }));
  SASSERT(1 == offsetof(struct B T { char a; int b[2]; }, b));

  ASSERT(9, ({ struct {  struct { char m2; long long m5;  } A; } T; sizeof(T); }));
  ASSERT(2, ({ typedef struct { struct { int :9, m16; } A; } T; offsetof(T, m16); }));
  ASSERT(12, ({ struct A { long long :48, :46; } a; sizeof(a); }));

  SASSERT(1 == offsetof(struct { char a; A int(*b)(int); }, b));
#ifdef NOTGCC
  SASSERT(1 == offsetof(struct { char a; int(*A b)(int); }, b));
#endif
  SASSERT(1 == offsetof(struct { char a; int(*b)(int) A; }, b));

  SASSERT(1 == offsetof(struct { char a; B int(*b)(int); }, b));
  SASSERT(1 == offsetof(struct { char a; int(*b B)(int); }, b));

  // format
  ASSERT(1, ({ typedef struct { char c; long long __attribute__((,,packed,,__packed__,,)) i; } S; offsetof(S, i); }));
  ASSERT(9, ({ typedef struct { char c; long long i [[,,gnu::packed,,gnu::__packed__,,]]; } S; sizeof(S); }));

  // no-op
#ifdef NOTCLANG
  ASSERT(8, ({ struct { char a; int b; } B x; sizeof(x); }));
  EASSERT(0, offsetof(struct { char a; int b; } B, a));
  EASSERT(4, offsetof(struct { char a; int b; } B, b));
#endif
  ASSERT(12, ({ typedef struct [[packed]] { char a; int b[2]; } T; sizeof(T); }));
  EASSERT(4, offsetof(struct [[packed]] { char a; int b[2]; }, b));
}

int main() {
  has_attr();
  packed();

  printf("OK\n");
  return 0;
}

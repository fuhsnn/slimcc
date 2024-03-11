#include "test.h"

int main(void) {
  typedef struct S s0;
  typedef const struct S s1;
  typedef volatile struct S s2;
  typedef __attribute__((aligned(64))) struct S a0;

  SASSERT(__builtin_types_compatible_p(s0, s1));
  SASSERT(__builtin_types_compatible_p(s0, s2));
  SASSERT(__builtin_types_compatible_p(s1, s2));
  SASSERT(__builtin_types_compatible_p(s0, a0));

  typedef struct S *p0;
  typedef const struct S *p1;
  typedef s1 *p2;
  typedef const s0 *p3;

  SASSERT(!__builtin_types_compatible_p(p0, p1));
  SASSERT(!__builtin_types_compatible_p(p0, p2));
  SASSERT(!__builtin_types_compatible_p(p0, p3));
  SASSERT(__builtin_types_compatible_p(p1, p2));
  SASSERT(__builtin_types_compatible_p(p1, p3));
  SASSERT(__builtin_types_compatible_p(p2, p3));

  struct S { int i; };

  SASSERT(__builtin_types_compatible_p(s0, s1));
  SASSERT(__builtin_types_compatible_p(s0, s2));
  SASSERT(__builtin_types_compatible_p(s1, s2));
  SASSERT(__builtin_types_compatible_p(s0, a0));
  SASSERT(_Alignof(s0) != _Alignof(a0));

  SASSERT(!__builtin_types_compatible_p(p0, p1));
  SASSERT(!__builtin_types_compatible_p(p0, p2));
  SASSERT(!__builtin_types_compatible_p(p0, p3));
  SASSERT(__builtin_types_compatible_p(p1, p2));
  SASSERT(__builtin_types_compatible_p(p1, p3));
  SASSERT(__builtin_types_compatible_p(p2, p3));

  typedef struct S s3;
  typedef s0 s4;
  typedef __attribute__((aligned(64))) s0 a1;

  SASSERT(__builtin_types_compatible_p(s3, s4));
  SASSERT(__builtin_types_compatible_p(s3, a1));
  SASSERT(__builtin_types_compatible_p(a0, a1));
  SASSERT(_Alignof(s0) != _Alignof(a1));
  SASSERT(_Alignof(a0) == _Alignof(a1));

  SASSERT(sizeof(s0) > 0);
  SASSERT(sizeof(s1) > 0);
  SASSERT(sizeof(s2) > 0);
  SASSERT(sizeof(s3) > 0);
  SASSERT(sizeof(s4) > 0);

  SASSERT(sizeof(*(p0){0}) > 0);
  SASSERT(sizeof(*(p1){0}) > 0);
  SASSERT(sizeof(*(p2){0}) > 0);
  SASSERT(sizeof(*(p3){0}) > 0);

  SASSERT(sizeof(a0) > 0);
  SASSERT(sizeof(a1) > 0);

  typedef struct FS fs0;
  struct FS { int i; char arr[]; };
  typedef struct FS fs1;
  static fs0 fs00 = {1,2};
  static fs0 fs01 = {1,2,3};
  static fs1 fs10 = {1,2,3};
  static fs1 fs11 = {1,2,3,4};
  SASSERT(_Generic(fs00, struct FS: 1));
  SASSERT(_Generic(fs01, struct FS: 1));
  SASSERT(_Generic(fs10, struct FS: 1));
  SASSERT(_Generic(fs11, struct FS: 1));
  SASSERT(sizeof(fs00) > 0);
  SASSERT(sizeof(fs10) > 0);
  SASSERT(sizeof(fs01) > 0);
  SASSERT(sizeof(fs11) > 0);

#define A __attribute__((aligned(1024)))
  ASSERT(1024, ({ A typedef struct s S; struct s { char c; }; _Alignof(S); }));
  ASSERT(1024, ({ typedef A struct s S; struct s { char c; }; _Alignof(S); }));
  ASSERT(1,    ({ typedef struct A s S; struct s { char c; }; _Alignof(S); })); // clang: 1024
  ASSERT(1024, ({ typedef struct s A S; struct s { char c; }; _Alignof(S); }));
  ASSERT(1024, ({ typedef struct s S A; struct s { char c; }; _Alignof(S); }));
  ASSERT(1,    ({ typedef struct s S; A struct s { char c; }; _Alignof(S); }));
  ASSERT(1024, ({ typedef struct s S; struct A s { char c; }; _Alignof(S); }));
  ASSERT(1024, ({ typedef struct s S; struct s { A char c; }; _Alignof(S); }));
  ASSERT(1024, ({ typedef struct s S; struct s { char A c; }; _Alignof(S); }));
  ASSERT(1024, ({ typedef struct s S; struct s { char c A; }; _Alignof(S); }));
  ASSERT(1024, ({ typedef struct s S; struct s { char c; } A; _Alignof(S); }));

#define A2 __attribute__((aligned(2048)))
  ASSERT(2048, ({ typedef struct s S A; struct s { char c; } A2; _Alignof(S); })); // clang: 1024
  ASSERT(2048, ({ typedef struct s S A2; struct s { char c; } A; _Alignof(S); }));

#define P __attribute__((packed))
  ASSERT(8, ({ typedef struct P s S; struct s { char c; int i; }; sizeof(S); })); // clang: 5
  ASSERT(5, ({ typedef struct s S; struct P s { char c; int i; }; sizeof(S); }));
  ASSERT(5, ({ typedef struct s S; struct s { char c; int i; } P; sizeof(S); }));

  printf("OK\n");
  return 0;
}


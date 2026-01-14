#include "test.h"

void cv_qualified_member(int i) {
  const int a1[4];
  SASSERT(__builtin_types_compatible_p(typeof(&(*a1)), const int*));
  SASSERT(!__builtin_types_compatible_p(typeof(&(*a1)), int*));

  SASSERT(__builtin_types_compatible_p(typeof(&a1[0]), const int*));
  SASSERT(!__builtin_types_compatible_p(typeof(&a1[0]), int*));

  volatile int vla[i];
  SASSERT(__builtin_types_compatible_p(typeof(&(*vla)), volatile int*));
  SASSERT(!__builtin_types_compatible_p(typeof(&(*vla)), int*));

  SASSERT(__builtin_types_compatible_p(typeof(&vla[0]), volatile int*));
  SASSERT(!__builtin_types_compatible_p(typeof(&vla[0]), int*));

  struct S1 {
    int i;
  };

  struct S2 {
    int const i;
  };

  struct S3 {
    const struct S1 s1;
  };

  struct S4 {
    struct S1 s[4];
  };

  struct S1 const s1;
  SASSERT(__builtin_types_compatible_p(typeof(&s1.i), const int*));
  SASSERT(!__builtin_types_compatible_p(typeof(&s1.i), int*));

  struct S1 *p1;
  SASSERT(__builtin_types_compatible_p(typeof(&p1->i), int*));
  SASSERT(!__builtin_types_compatible_p(typeof(&p1->i), int const*));

  struct S1 const* p1c;
  SASSERT(!__builtin_types_compatible_p(typeof(&p1c->i), int*));
  SASSERT(__builtin_types_compatible_p(typeof(&p1c->i), int const*));

  struct S2 *p2;
  SASSERT(!__builtin_types_compatible_p(typeof(&p2->i), int*));
  SASSERT(__builtin_types_compatible_p(typeof(&p2->i), int const*));

  struct S3 *p3;
  SASSERT(!__builtin_types_compatible_p(typeof(&p3->s1.i), int*));
  SASSERT(__builtin_types_compatible_p(typeof(&p3->s1.i), int const*));

  struct S4 const *p4;
  SASSERT(!__builtin_types_compatible_p(typeof(&p4->s[3].i), int*));
  SASSERT(__builtin_types_compatible_p(typeof(&p4->s[3].i), int const*));

  SASSERT(!__builtin_types_compatible_p(typeof(&p4->s->i), int*));
  SASSERT(__builtin_types_compatible_p(typeof(&p4->s->i), int const*));

  SASSERT(!__builtin_types_compatible_p(typeof(&(*p4->s).i), int*));
  SASSERT(__builtin_types_compatible_p(typeof(&(*p4->s).i), int const*));

  struct S5 {
    struct {
      int i;
    };
  };
  struct S5 const *p5;
  SASSERT(_Generic(&p5->i, int const*:1));
}

struct B {
  int i;
};
typedef struct {
  struct B b1;
  struct B b2;
} A;
int regress1(const A* a, int i) {
  return (((i) ? &((a)->b1) : &((a)->b2)))->i;
}

int main(int argc, char** argv) {
  {
    typedef _Atomic const int A1[2];
    typedef volatile A1 A2[2];
    typedef A2 A3[2];
    A3 a1;
    typeof_unqual(A3) a2;
    static_assert(_Generic(&(***a1), const volatile _Atomic int*: 1));
    static_assert(_Generic(&(***a2), _Atomic int*: 1));
  }

  ASSERT(17, regress1(&(A){31,17}, 0));
  ASSERT(31, regress1(&(A){31,17}, 1));

  printf("OK\n");
  return 0;
}

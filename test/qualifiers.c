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
}

int main(int argc, char** argv) {

  printf("OK\n");
  return 0;
}

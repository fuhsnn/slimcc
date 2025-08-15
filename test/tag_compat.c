#include "test.h"

#define TAG_COMPAT_CHK(_res, ...) \
    static_assert(_res == _Generic( __VA_ARGS__ :1, default:0))

struct tag {int i;} tagged;
struct     {int i;} nontag;

TAG_COMPAT_CHK(1, tagged, struct tag {int i;} );
TAG_COMPAT_CHK(0, tagged, struct     {int i;} );
TAG_COMPAT_CHK(0, nontag, struct tag {int i;} );
TAG_COMPAT_CHK(0, nontag, struct     {int i;} );

TAG_COMPAT_CHK(1, struct tag {int i;}, typeof(tagged) );
TAG_COMPAT_CHK(0, struct     {int i;}, typeof(tagged) );
TAG_COMPAT_CHK(0, struct tag {int i;}, typeof(nontag) );
TAG_COMPAT_CHK(0, struct     {int i;}, typeof(nontag) );

typedef
struct tag {int i;} tagged_def;
typedef
struct     {int i;} nontag_def;

TAG_COMPAT_CHK(1, struct tag {int i;}, tagged_def );
TAG_COMPAT_CHK(0, struct     {int i;}, tagged_def );
TAG_COMPAT_CHK(0, struct tag {int i;}, nontag_def );
TAG_COMPAT_CHK(0, struct     {int i;}, nontag_def );

struct { struct tag {int i;} tagged; } inner_tagged;
struct { struct     {int i;} nontag; } inner_nontag;

TAG_COMPAT_CHK(1, inner_tagged.tagged, struct tag {int i;} );
TAG_COMPAT_CHK(0, inner_tagged.tagged, struct     {int i;} );
TAG_COMPAT_CHK(0, inner_nontag.nontag, struct tag {int i;} );
TAG_COMPAT_CHK(0, inner_nontag.nontag, struct     {int i;} );

TAG_COMPAT_CHK(1, struct tag {int i;}, struct tag {int i;} );

TAG_COMPAT_CHK(1, struct t2 {int :3, i;}, struct t2 {int :3, i;} );
TAG_COMPAT_CHK(1, struct t3 {struct{int i;};}, struct t3 {struct{int i;};} );
TAG_COMPAT_CHK(1, struct t4 {struct t5{int i;} s;}, struct t4 {struct t5{int i;} s;} );
struct t6 {int i;} *p1;
TAG_COMPAT_CHK(1, *p1, struct t6 {int i;} );

typedef struct t7 {int i;} t8;
TAG_COMPAT_CHK(1, struct t7 {int i;}, struct t7 {int i;} );
typedef struct t7 {int i;} t8;
TAG_COMPAT_CHK(1, t8, struct t7 {int i;} );
TAG_COMPAT_CHK(1, struct t7 {int i;}, t8 );

t8 s0;
int assign(struct t7 {int i;} s1, t8 s2, struct t7 {int i;} s3) {
    s0 = s1;
    s1 = s2;
    s2 = s3;
    s3 = s0;
    return 1;
}

typedef const struct t7 {int i;} t9;
TAG_COMPAT_CHK(0, struct t7 {int i;}, t9 );
TAG_COMPAT_CHK(1, const struct t7 {int i;}, t9 );
TAG_COMPAT_CHK(1, const t8, t9 );
TAG_COMPAT_CHK(1, t8, typeof_unqual(t9) );

int flexible(void) {
  typedef struct t10 {int i; int b[];} t11;
  t11 *flex = malloc(sizeof(t11) + 137 * sizeof(int));
  struct t10 {int i; int b[];}* flex2 =  malloc(sizeof(t11) + 321 * sizeof(int));
  TAG_COMPAT_CHK(1, *flex, struct t10 {int i; int b[];} );
  TAG_COMPAT_CHK(1, flex, typeof(flex2) );
  free(flex), free(flex2);

  {
    struct S { char c; char arr[]; };
    const struct S s0;
    static struct S { char c; char arr[]; } s = {1,{2,3,4}};
    typedef struct S ST;
    static_assert(_Generic(s0, typeof(s):1));
    static_assert(_Generic(s0, ST:1));
    {
      static const struct S s2 = {1,{1,2,3,4,5}};
      static_assert(_Generic(s2, ST:1));
      static_assert(_Generic(s2, typeof(s):1));
      static_assert(_Generic(typeof(s2), typeof(s0):1));
    }
  }
  return 1;
}

int incomplete_ptr(void) {
  struct list { struct list *next; };
  TAG_COMPAT_CHK(1, struct lists { struct list lst; }, struct lists { struct list lst; } );

  struct S *p;
  {
    typedef struct S {} T;
    static_assert(_Generic(*p, T:0, default:1));
  }
  struct S { int i; };
  { static_assert(_Generic(struct S { int i;}, typeof(*p): 1)); }
  { static_assert(_Generic(struct S { int j;}, typeof(*p): 0, default: 1)); }

  return 1;
}

void param1(struct S *) {
  typedef const int I;
  struct S { I i; };
}

void param2(struct S *) {
  struct S { int i; };
}

void param3(struct S { int i; } *) {
}

void param4(struct S *) {
  struct S { const int i; };
}

int incomplete_param(void) {
  SASSERT(0 == _Generic(param1, typeof(&param2): 1, default: 0));
  SASSERT(0 == _Generic(param1, typeof(&param3): 1, default: 0));
  SASSERT(1 == _Generic(param1, typeof(&param4): 1, default: 0));
  SASSERT(1 == _Generic(param2, typeof(&param3): 1, default: 0));
  SASSERT(0 == _Generic(param2, typeof(&param4): 1, default: 0));
  SASSERT(0 == _Generic(param3, typeof(&param4): 1, default: 0));

  return 1;
}

int main(void) {
  ASSERT(1, assign((t8){},(t8){},(t8){}));
  ASSERT(1, flexible());
  ASSERT(1, incomplete_ptr());
  ASSERT(1, incomplete_param());


  printf("OK\n");
}

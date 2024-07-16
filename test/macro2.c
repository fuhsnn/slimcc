#include "test.h"


#define AAA
#define MACRO_WITH_DEFINED_INSIDE   (defined(AAA) && !defined(BBB))

#if MACRO_WITH_DEFINED_INSIDE
#else
#error
#endif

int main(void) {
#define H(x) #x
#define STR(x) H(x)

#define M1(X) H(X) H(X)
ASSERT(0, strcmp("00", M1(__COUNTER__)) );

#define M2(A,B,...) H(__VA_OPT__(A##B))
ASSERT(0, strcmp("XY", M2(X,Y,z)) );

#define M3(A,B,...) #__VA_OPT__(A##B)
ASSERT(0, strcmp("XY", M3(X,Y,z)) );

#define EXPAND_PASTE(x,y) x + x##y
#define i 5
ASSERT(107, ({ int i3=100; EXPAND_PASTE(1+i,3); }));
#undef i

#define M4(A,...) H((A,##__VA_ARGS__))
ASSERT(0, strcmp("(X,)",  M4(X,) ));

#define M5(x,...) b ## __VA_OPT__(x)
ASSERT(0, strcmp("bbz", STR(M5(M5(z,a),b)) ));

#define hash_hash # ## #
#define mkstr(a) # a
#define in_between(a) mkstr(a)
#define join(c, d) in_between(c hash_hash d)
ASSERT(0, strcmp("x ## y", join(x,y) ));


printf("OK\n");

}

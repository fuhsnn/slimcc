#include "test.h"

#define EMPTY_O
#define EMPTY()
#define H(x) #x
#define STR(x) H(x)


int main (void) {
 ASSERT(0, strcmp("+( )", STR(+EMPTY()(
))  ));

 ASSERT(0, strcmp("a ( )", STR(a EMPTY()(
))  ));

 ASSERT(0, strcmp("+ ()", STR(+EMPTY()
()) ));

 ASSERT(0, strcmp("a ()", STR(a EMPTY()
()) ));

 ASSERT(0, strcmp("+()", STR(+EMPTY(
)()) ));

 ASSERT(0, strcmp("a ()", STR(a EMPTY(
)()) ));

 ASSERT(0, strcmp("+()", STR(+EMPTY
()()) ));

 ASSERT(0, strcmp("a ()", STR(a EMPTY
()()) ));

 ASSERT(0, strcmp("+ ()", STR(+
EMPTY()()) ));

 ASSERT(0, strcmp("a ()", STR(a
EMPTY()()) ));

#define F5(a,b,c,d,e) (a, b, c, d, e)
 ASSERT(0, strcmp("(A, , , , Z)", STR(F5(A,,EMPTY_O,EMPTY(),Z)) ));

 printf("OK\n");
}

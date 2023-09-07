#include "test.h"

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

 printf("OK\n");
}

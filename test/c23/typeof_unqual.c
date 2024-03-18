#include <test.h>

int main(void) {
    const int i1;
    typeof_unqual(i1) i2;

    SASSERT(!__builtin_types_compatible_p(typeof(&i1), int *));
    SASSERT(__builtin_types_compatible_p(typeof(&i1), int const*));

    SASSERT(__builtin_types_compatible_p(typeof(&i2), int *));
    SASSERT(!__builtin_types_compatible_p(typeof(&i2), int const*));

    printf("OK\n");
}

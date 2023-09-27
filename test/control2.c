#include "test.h"
#include <stdint.h>

char c23_label(void) {

  {
    int i = 0;
    if (i != 0)
lab3:
      i = 1;
    ASSERT(0, i);
  }

  switch (1) {
    default:
    case 2:
  }
  if (0) {
    {
      lab1:
    }
    {
      lab2:
      int j;
    }
    return 77;
  }
  goto lab1;
}

int main(void){
  ASSERT(2, ({ uint32_t i=0; switch(i){case 0 ...0xFFFFFFFF: i=2;} i; }));
  ASSERT(2, ({ int32_t i=0; switch(i){case 0x80000000 ...0x7FFFFFFF: i=2;} i; }));
  ASSERT(2, ({ uint64_t i=0; switch(i){case 0 ...0xFFFFFFFFFFFFFFFF: i=2;} i; }));
  ASSERT(2, ({ int64_t i=0; switch(i){case 0x8000000000000000 ...0x7FFFFFFFFFFFFFFF: i=2;} i; }));

  ASSERT(2, ({ uint32_t i=0; switch(i){case 0 ...0x100000000: i=2;} i; }));
  ASSERT(2, ({ int32_t i=-1; switch(i){case -1 ...(int64_t)-1: i=2;} i; }));

  ASSERT(2, ({ int64_t i=0; switch(0x123456789){case 0x123456789: i=2;} i; }));


  ASSERT(77, c23_label());

  printf("OK\n");
}
